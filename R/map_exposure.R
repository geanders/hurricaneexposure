#' Create a default map with eastern US states
#'
#' Creates a \code{ggplot} object with the underlying map of all states in the
#' eastern section of the US that might be prone to hurricane-related
#' exposure. Other lines and points can be added to the output using
#' \code{ggplot2} plotting functions.
#'
#' @details Only states in the eastern half of the United States (i.e., ones
#' prone to exposure to Atlantic basin tropical storms) are included on this
#' map.
#'
#' @return A \code{ggplot} object that maps the states of the Eastern United States
#'
#' @export
#'
#' @examples
#' default_map()
default_map <- function(){

        map_data <- get_eastern_map("state")

        out <- ggplot2::ggplot(map_data,
                               ggplot2::aes_(x = ~ long, y = ~ lat,
                                             group = ~ group)) +
                ggplot2::geom_polygon(fill = "lightgray", color = "white") +
                ggmap::theme_nothing(legend = TRUE) +
                ggplot2::coord_map()
        return(out)
}

#' Get map data for eastern US states
#'
#' @param map A character string giving the map database from which to pull
#'
#' @return A dataframe with map data pulled using the \code{map_data} function in
#'    \code{ggplot2}, filtered to states in the eastern half of the United States.
#'
#' @importFrom dplyr %>%
get_eastern_map <- function(map  = "county"){

        eastern_states <- c("alabama", "arkansas", "connecticut", "delaware",
                            "district of columbia", "florida", "georgia", "illinois",
                            "indiana", "iowa", "kansas", "kentucky", "louisiana",
                            "maine", "maryland", "massachusetts", "michigan",
                            "mississippi", "missouri", "new hampshire", "new jersey",
                            "new york", "north carolina", "ohio", "oklahoma",
                            "pennsylvania", "rhode island", "south carolina",
                            "tennessee", "texas", "vermont", "virginia",
                            "west virginia", "wisconsin")

        map_data <- ggplot2::map_data(map = map) %>%
                dplyr::filter_(~ region %in% eastern_states)

        if(map == "county"){
                county.fips <- maps::county.fips %>%
                        dplyr::mutate_(polyname = ~ as.character(polyname)) %>%
                        dplyr::mutate_(polyname = ~ stringr::str_replace(polyname,
                                                                         ":.+", ""))
                map_data <- map_data %>%
                        tidyr::unite_(col = "polyname", from = c("region", "subregion"),
                                      sep = ",") %>%
                        dplyr::left_join(county.fips, by = "polyname") %>%
                        dplyr::mutate_(fips = ~ stringr::str_pad(fips, 5,
                                                                 side = "left", pad = "0"))
        }

        return(map_data)
}

#' Plot Atlantic basin hurricane tracks
#'
#' Plot the tracks of any selected storms in the hurricane tracking
#'    dataset for the Atlantic basin. This function allows you to
#'    plot a new map or add the tracks to an existing ggplot object.
#'
#' @param storms Character vector with the names of all storms to plot.
#'    This parameter must use the unique storm identifiers from the
#'    `storm_id` column of the `hurr_tracks` dataframe.
#' @param plot_object NULL or the name of a ggplot object to use as the
#'    underlying plot object. If NULL, the function will generate a new
#'    map of the eastern US states using `default_map`.
#' @param padding Numerical value giving the number of degrees to add to the
#'    outer limits of the plot object (or default map if `plot_object` is
#'    left as NULL) when cropping hurricane tracks.
#' @param plot_points TRUE / FALSE indicator of whether to include points,
#'    as well as lines, when plotting the hurricane tracks. These points show
#'    the times for which observations were recorded in the hurricane track data.
#' @param alpha Numerical value designating the amount of transparency to
#'    use for plotting tracks.
#' @param color Character string giving the color to use to plot the tracks.
#'
#' @return Returns a ggplot object with plotting data for the storm tracks
#'    of the selected storms. This object can be printed directly or added
#'    to with other \code{ggplot2} commands.
#'
#' @examples
#' # Ensure that data package is available before running the example.
#' #  If it is not, see the `hurricaneexposure` package vignette for details
#' # on installing the required data package.
#' if (requireNamespace("hurricaneexposuredata", quietly = TRUE)) {
#'
#' map_tracks(storms = "Sandy-2012")
#' map_tracks(storms = "Floyd-1999", plot_points = TRUE)
#' map_tracks(storms = c("Sandy-2012", "Floyd-1999"))
#' a <- map_tracks(storms = "Sandy-2012", color = "blue", alpha = 0.3)
#' b <- map_tracks(storms = "Floyd-1999", plot_object = a)
#' b
#' }
#' @importFrom dplyr %>%
#'
#' @export
map_tracks <- function(storms, plot_object = NULL, padding = 2, plot_points = FALSE,
                       alpha = 1, color = "firebrick"){

        hasData()

        if(is.null(plot_object)){
                plot_object <- default_map()
        }

        map_dim <- apply(matrix(c(-106.65037, 25.12993, -67.00742, 47.48101),
                                byrow = TRUE, ncol = 2),
                         MARGIN = 2,
                         function(x) range(x) + c(-1, 1) * padding)
        tracks <- hurricaneexposuredata::hurr_tracks %>%
                dplyr::select_(~ latitude, ~ longitude, ~ storm_id,
                               ~ date) %>%
                dplyr::filter_(~ as.character(storm_id) %in% storms &
                              longitude > map_dim[1, 1] &
                              longitude < map_dim[2, 1] &
                              latitude > map_dim[1, 2] &
                              latitude < map_dim[2, 2]) %>%
                dplyr::mutate_(date = ~ lubridate::ymd_hm(date))

        splt_tracks <- split(tracks, tracks$storm_id)
        full_tracks <- lapply(splt_tracks, interp_track)
        full_tracks <- do.call("rbind", full_tracks)

        out <- plot_object +
                        ggplot2::geom_path(data = full_tracks,
                                           ggplot2::aes_(x = ~ longitude,
                                                        y = ~ latitude,
                                                        group = ~ storm_id),
                                           alpha = alpha,
                                           color = color) +
                ggplot2::coord_map()

        if(plot_points){
                out <- out + ggplot2::geom_point(data = tracks,
                                                    ggplot2::aes_(x = ~ longitude,
                                                                 y = ~ latitude,
                                                                 group = ~ storm_id),
                                                    alpha = alpha)
        }
        return(out)
}

#' Interpolate a storm track
#'
#' This function takes a wider-spaced storm track (e.g., every 6 hours) and
#' interpolates to a finer interval (e.g., every 15 minutes). To do this, it
#' fits GLMs of latitude and longitude regressed on natural cubic splines of
#' date-time, and then predicts these splines to new intervals. These
#' splines use degrees of freedom equal to the number of original observations
#' divided by two.
#'
#' @param track A dataframe with hurricane track data for a single storm
#' @param tint A numeric vector giving the time interval to impute to, in units
#'    of hours (e.g., 0.25, the default, interpolates to 15 minute-intervals).
#'
#' @return A dataframe with hurricane track data for a single storm,
#'    interpolated to the interval specified by \code{tint}.
interp_track <- function(track, tint = 0.25){

        if(nrow(track) < 3){
                return(track)
        } else {
                interp_df <- floor(nrow(track) / 2)
                interp_date <- seq(from = min(track$date),
                                to = max(track$date),
                                by = 900) # interpolate to 15 minutes
                interp_date <- data.frame(date = interp_date)

                lat_spline <- stats::glm(latitude ~ splines::ns(date,
                                                                df = interp_df),
                                        data = track)
                interp_lat <- stats::predict.glm(lat_spline,
                                         newdata = as.data.frame(interp_date))
                lon_spline <- stats::glm(longitude ~ splines::ns(date,
                                                                 df = interp_df),
                                        data = track)
                interp_lon <- stats::predict.glm(lon_spline,
                                                 newdata = interp_date)

                full_track <- data.frame(storm_id = track$storm_id[1],
                                        date = interp_date,
                                        latitude = interp_lat,
                                        longitude = interp_lon)
        }
        return(full_track)
}

#' Map counties with rain exposure
#'
#' Map counties as "exposed" or "unexposed" based on the criteria that the
#' storm came within a given distance (specified by \code{dist_limit}) of
#' the county's population mean center and a certain amount of rain
#' \code{rain_limit} fell during a specified window of days (\code{days_included}).
#'
#' @param add_track TRUE / FALSE of whether to add the storm's track to the map. The
#'    default is TRUE.
#' @inheritParams county_distance
#' @inheritParams county_rain
#' @inheritParams map_counties
#'
#' @examples
#' # Ensure that data package is available before running the example.
#' #  If it is not, see the `hurricaneexposure` package vignette for details
#' # on installing the required data package.
#' if (requireNamespace("hurricaneexposuredata", quietly = TRUE)) {
#'
#' map_rain_exposure(storm = "Floyd-1999", rain_limit = 50, dist_limit = 100)
#'
#' # Example of customizine track appearance
#' allison_map <- map_rain_exposure(storm = "Allison-2001", rain_limit = 125,
#'                                  dist_limit = 100, days_included = -5:3,
#'                                  add_track = FALSE)
#' map_tracks("Allison-2001", plot_object = allison_map, plot_points = TRUE)
#'}
#' @importFrom dplyr %>%
#'
#' @export
map_rain_exposure <- function(storm, rain_limit, dist_limit,
                              days_included = c(-2, -1, 0, 1), add_track = TRUE){

        map_data <- filter_storm_data(storm = storm,
                                           days_included = days_included,
                                           include_rain = TRUE,
                                           output_vars = c("fips", "tot_precip",
                                                           "storm_dist")) %>%
                dplyr::mutate_(exposed = ~ tot_precip >= rain_limit &
                                       storm_dist <= dist_limit) %>%
                dplyr::mutate_(value = ~ factor(exposed,
                                                levels = c("FALSE", "TRUE"))) %>%
                dplyr::tbl_df()

        out_data <- get_eastern_map() %>%
                dplyr::left_join(map_data, by = "fips")
        out <- ggplot2::ggplot() +
                ggplot2::geom_polygon(data = out_data,
                                      ggplot2::aes_(x = ~ long, y = ~ lat, group = ~ group,
                                                    fill = ~ value),
                                      color = "lightgray", size = 0.2) +
                ggplot2::borders("state", regions = c("virginia", "north carolina", "south carolina",
                                                      "georgia", "florida", "alabama", "kentucky",
                                                      "tennessee", "maryland", "west virginia",
                                                      "district of columbia", "pennsylvania",
                                                      "new jersey", "delaware", "mississippi",
                                                      "louisiana", "texas", "oklahoma", "arkansas",
                                                      "new york", "connecticut", "rhode island",
                                                      "massachusetts", "new hampshire", "vermont",
                                                      "maine", "kansas", "missouri", "iowa", "michigan",
                                                      "illinois", "ohio", "wisconsin", "indiana"),
                                 colour = "black", fill = NA, size = 0.2, alpha = 0.5) +
                ggplot2::theme_void() +
                ggplot2::coord_map() +
                ggplot2::scale_fill_manual(name = paste("Rain >", rain_limit, "mm"),
                                           values = c("white", "navy"),
                                           labels = c("Unexposed", "Exposed"))

        if(add_track){
                out <- map_tracks(storm, plot_object = out)
        }

        return(out)
}

#' Map counties with distance exposure
#'
#' Map counties as "exposed" or "unexposed" based on the criterion that the
#' storm came within a given distance (specified by \code{dist_limit}) of
#' the county's population mean center.
#'
#' @inheritParams county_distance
#' @inheritParams county_rain
#' @inheritParams map_counties
#' @inheritParams map_rain_exposure
#'
#' @return Plots a map showing whether eastern US counties were exposed or
#'    unexposed to a specific storm based on a distance criterion.
#'
#' @examples
#' # Ensure that data package is available before running the example.
#' #  If it is not, see the `hurricaneexposure` package vignette for details
#' # on installing the required data package.
#' if (requireNamespace("hurricaneexposuredata", quietly = TRUE)) {
#'
#'
#' floyd_map <- map_distance_exposure(storm = "Floyd-1999", dist_limit = 75)
#' floyd_map
#'
#' allison_map <- map_distance_exposure(storm = "Allison-2001",
#'                                      dist_limit = 75)
#' map_tracks("Allison-2001", plot_points = FALSE, plot_object = allison_map)
#' }
#' @importFrom dplyr %>%
#'
#' @export
map_distance_exposure <- function(storm, dist_limit, add_track = TRUE){

        map_data <- filter_storm_data(storm = storm,
                                      output_vars = c("fips", "storm_dist")) %>%
                dplyr::mutate_(exposed = ~ storm_dist <= dist_limit) %>%
                dplyr::mutate_(value = ~ factor(exposed,
                                                levels = c("FALSE", "TRUE"))) %>%
                dplyr::tbl_df()

        out_data <- get_eastern_map() %>%
                dplyr::left_join(map_data, by = "fips")
        out <- ggplot2::ggplot() +
                ggplot2::geom_polygon(data = out_data,
                                      ggplot2::aes_(x = ~ long, y = ~ lat, group = ~ group,
                                                    fill = ~ value),
                                      color = "lightgray", size = 0.2) +
                ggplot2::borders("state", regions = c("virginia", "north carolina", "south carolina",
                                                      "georgia", "florida", "alabama", "kentucky",
                                                      "tennessee", "maryland", "west virginia",
                                                      "district of columbia", "pennsylvania",
                                                      "new jersey", "delaware", "mississippi",
                                                      "louisiana", "texas", "oklahoma", "arkansas",
                                                      "new york", "connecticut", "rhode island",
                                                      "massachusetts", "new hampshire", "vermont",
                                                      "maine", "kansas", "missouri", "iowa", "michigan",
                                                      "illinois", "ohio", "wisconsin", "indiana"),
                                 colour = "black", fill = NA, size = 0.2, alpha = 0.5) +
                ggplot2::theme_void() +
                ggplot2::coord_map() +
                ggplot2::scale_fill_manual(name = paste("Distance <", dist_limit, "km"),
                                           values = c("white", "forestgreen"),
                                           labels = c("Unexposed", "Exposed"))

        if(add_track){
                out <- map_tracks(storm, plot_object = out)
        }

        return(out)
}

#' Map counties with wind exposure
#'
#' Map counties as "exposed" or "unexposed" based on the criterion that the
#' storm had a certain wind speed (specified by \code{wind_limit}, in meters
#' per second).
#'
#' @inheritParams filter_wind_data
#' @inheritParams county_rain
#' @inheritParams map_rain_exposure
#'
#' @return Plots a map showing whether eastern US counties were exposed or
#'    unexposed to a specific storm based on a wind criterion.
#'
#' @examples
#' # Ensure that data package is available before running the example.
#' #  If it is not, see the `hurricaneexposure` package vignette for details
#' # on installing the required data package.
#' if (requireNamespace("hurricaneexposuredata", quietly = TRUE)) {
#'
#' map_wind_exposure(storm = "Beryl-1988", wind_limit = 15)
#' }
#' @importFrom dplyr %>%
#'
#' @export
map_wind_exposure <- function(storm, wind_var = "vmax_sust", wind_limit,
                              add_track = TRUE, wind_source = "modeled"){

        if(wind_var %in% c("vmax_sust", "vmax_gust")){
                wind_metric <- "m / s"
        } else {
                wind_metric <- "minutes"
        }

        map_data <- filter_wind_data(storm = storm, wind_source = wind_source,
                                      output_vars = c("fips", wind_var)) %>%
                `colnames<-`(c("fips", "wind_value")) %>%
                dplyr::mutate_(exposed = ~ wind_value >= wind_limit) %>%
                dplyr::mutate_(value = ~ factor(exposed,
                                                levels = c("FALSE", "TRUE"))) %>%
                dplyr::tbl_df()

        out_data <- get_eastern_map() %>%
                dplyr::left_join(map_data, by = "fips")
        out <- ggplot2::ggplot() +
                ggplot2::geom_polygon(data = out_data,
                                      ggplot2::aes_(x = ~ long, y = ~ lat, group = ~ group,
                                                    fill = ~ value),
                                      color = "lightgray", size = 0.2) +
                ggplot2::borders("state", regions = c("virginia", "north carolina", "south carolina",
                                                      "georgia", "florida", "alabama", "kentucky",
                                                      "tennessee", "maryland", "west virginia",
                                                      "district of columbia", "pennsylvania",
                                                      "new jersey", "delaware", "mississippi",
                                                      "louisiana", "texas", "oklahoma", "arkansas",
                                                      "new york", "connecticut", "rhode island",
                                                      "massachusetts", "new hampshire", "vermont",
                                                      "maine", "kansas", "missouri", "iowa", "michigan",
                                                      "illinois", "ohio", "wisconsin", "indiana"),
                                 colour = "black", fill = NA, size = 0.2, alpha = 0.5) +
                ggplot2::theme_void() +
                ggplot2::coord_map() +
                ggplot2::scale_fill_manual(name = paste("Wind >", wind_limit, wind_metric),
                                           values = c("white", "darkorange"),
                                           labels = c("Unexposed", "Exposed"))

        if(add_track){
                out <- map_tracks(storm, plot_object = out)
        }

        return(out)
}

#' Map county-level exposure based on reported events
#'
#' Map counties as "exposed" or "unexposed" based on the criterion that the county
#' had an event listing of a specified type in the NOAA Storm Events database.
#' For more information on the underlying data, see the helpfile for the
#' \code{storm_events} dataset.
#'
#' @param storm_id Character vector with the storm for which to map events
#'    (e.g., \code{"Katrina-2005"})
#' @inheritParams county_distance
#' @inheritParams county_events
#' @inheritParams map_rain_exposure
#'
#' @return A map showing whether eastern US counties were exposed or unexposed
#'    to a specific storm based on event listings.
#'
#' @note Note that flood events are not available for any year before 1996.
#'
#' @examples
#' # Ensure that data package is available before running the example.
#' #  If it is not, see the `hurricaneexposure` package vignette for details
#' # on installing the required data package.
#' if (requireNamespace("hurricaneexposuredata", quietly = TRUE)) {
#'
#' map_event_exposure(storm_id = "Floyd-1999", event_type = "flood")
#' map_event_exposure(storm_id = "Floyd-1999", event_type = "tornado")
#' map_event_exposure(storm_id = "Floyd-1999", event_type = "wind")
#' map_event_exposure(storm_id = "Floyd-1999", event_type = "tropical_storm")
#' }
#' @export
map_event_exposure <- function(storm_id, event_type, add_track = TRUE){

        hasData()

        storm <- storm_id
        storm_year <- gsub("*.+-", "", storm_id)
        counties <- hurricaneexposuredata::closest_dist %>%
                dplyr::filter_(~ storm_id == storm) %>%
                dplyr::select_(quote(fips), quote(storm_dist))
        map_data <- county_events(counties = counties$fips,
                                  start_year = storm_year,
                                  end_year = storm_year,
                                  event_type = event_type) %>%
                dplyr::filter_(~ storm_id == storm) %>%
                dplyr::select_(quote(fips)) %>%
                dplyr::mutate_(event = ~ 1) %>%
                dplyr::right_join(counties, by = "fips") %>%
                dplyr::mutate_(event = ~ !is.na(event)) %>%
                dplyr::rename_(value = ~ event) %>%
                dplyr::select_(quote(-storm_dist))

        out_data <- get_eastern_map() %>%
                dplyr::left_join(map_data, by = "fips")
        out <- ggplot2::ggplot() +
                ggplot2::geom_polygon(data = out_data,
                                      ggplot2::aes_(x = ~ long, y = ~ lat, group = ~ group,
                                                    fill = ~ value),
                                      color = "lightgray", size = 0.2) +
                ggplot2::borders("state", regions = c("virginia", "north carolina", "south carolina",
                                                      "georgia", "florida", "alabama", "kentucky",
                                                      "tennessee", "maryland", "west virginia",
                                                      "district of columbia", "pennsylvania",
                                                      "new jersey", "delaware", "mississippi",
                                                      "louisiana", "texas", "oklahoma", "arkansas",
                                                      "new york", "connecticut", "rhode island",
                                                      "massachusetts", "new hampshire", "vermont",
                                                      "maine", "kansas", "missouri", "iowa", "michigan",
                                                      "illinois", "ohio", "wisconsin", "indiana"),
                                 colour = "black", fill = NA, size = 0.2, alpha = 0.5) +
                ggplot2::theme_void() +
                ggplot2::coord_map() +
                ggplot2::scale_fill_manual(name = paste(stringr::str_to_title(event_type),
                                                        "event"),
                                           values = c("white", "red"),
                                           labels = c("Unexposed", "Exposed"))

        if(add_track){
                out <- map_tracks(storm, plot_object = out)
        }

        return(out)
}

#' Map counties
#'
#' @param storm Character string giving the name of the storm to plot (e.g.,
#'    "Floyd-1999")
#' @param metric Character string giving the metric to plot. Current options are
#'    \code{"distance"}, \code{"wind"}, and \code{"rainfall"}. These options are used
#'    to customize the color palette and scale of the choropleth map produced
#'    by this function.
#' @inheritParams county_rain
#' @inheritParams map_rain_exposure
#' @inheritParams filter_wind_data
#'
#' @return This function creates a choropleth map of counties in the eastern
#'    part of the United States, showing distance from a storm track, maximum
#'    wind speed (or duration of winds at or above 20 m / s), or total
#'    rainfall over a given window of one or more days near the date of the storm's
#'    closest approach.
#'
#' @examples
#' # Ensure that data package is available before running the example.
#' #  If it is not, see the `hurricaneexposure` package vignette for details
#' # on installing the required data package.
#' if (requireNamespace("hurricaneexposuredata", quietly = TRUE)) {
#'
#' map_counties("Floyd-1999", metric = "rainfall", days_included = c(-2, -1, 0, 1))
#'
#' map_counties("Katrina-2005", metric = "wind")
#' map_counties("Katrina-2005", metric = "wind", wind_var = "vmax_gust")
#' map_counties("Katrina-2005", metric = "wind", wind_var = "sust_dur")
#' map_counties("Katrina-2005", metric = "wind", wind_source = "ext_tracks")
#'}
#' @export
#'
#' @importFrom dplyr %>%
map_counties <- function(storm, metric = "distance", wind_var = "vmax_sust",
                        days_included = c(-2, -1, 0, 1), add_track = TRUE,
                        wind_source = "modeled"){
        if(metric == "distance"){
                map_data <- filter_storm_data(storm = storm,
                                              output_vars = c("fips",
                                                              "storm_dist")) %>%
                        dplyr::rename_(value = ~ storm_dist)
        } else if(metric == "rainfall"){
                map_data <- filter_storm_data(storm = storm, include_rain = TRUE,
                                              days_included = days_included,
                                              output_vars = c("fips",
                                                              "tot_precip")) %>%
                        dplyr::rename_(value = ~ tot_precip)
        } else if (metric == "wind") {
                map_data <- filter_wind_data(storm = storm,
                                             output_vars = c("fips", wind_var),
                                             wind_var = wind_var) %>%
                        `colnames<-`(c("fips", "value"))
        } else{
                stop("`metric` must be either `distance`, `rainfall`, or `wind`")
        }
        map_data <- map_data %>%
                dplyr::tbl_df()
        out <- hurr_choropleth(map_data, metric = metric, wind_var = wind_var,
                               wind_source = wind_source)

        if(add_track){
                out <- map_tracks(storm, plot_object = out)
        }

        return(out)
}

#' Create a map customized for this package
#'
#' Creates a county choropleth map customized for displaying
#' hurricane exposure. This function is used as a helper function within other
#' mapping functions in the package.
#'
#' @param map_data A dataframe with columns with FIPS numbers (in numeric
#'    class) for all counties in the eastern US (\code{region}) and the
#'    exposure value (\code{value})
#' @inheritParams map_counties
#' @inheritParams filter_wind_data
#'
#' @return A \code{ggplot} object with a map of hurricane exposure in eastern
#'    US counties
#'
#' @details The function only maps counties in states likely to be exposed
#' to Atlantic basin tropical storms.
#'
#' @importFrom dplyr %>%
hurr_choropleth <- function(map_data, metric = "distance", wind_var = "vmax_sust",
                            wind_source = "modeled"){

        if(metric == "rainfall"){
                breaks <- seq(0, 200, by = 25)
                palette_name <- "Blues"
                exposure_legend <- "Rainfall (mm)"
        } else if(metric == "distance"){
                breaks <- seq(0, 200, by = 25)
                palette_name <- "Greens"
                exposure_legend <- "Distance (km)"
        } else if(metric == "wind"){
                palette_name <- "Reds"
                if(wind_var %in% c("vmax_gust", "vmax_sust")){
                        exposure_legend <- "Wind speed (m / s)"
                        if(wind_source == "modeled"){
                                breaks <- c(0, seq(15, 45, by = 5))
                                exposure_legend <- "Wind speed (m / s)"
                        } else if (wind_source == "ext_tracks"){
                                if(wind_var == "vmax_sust"){
                                        breaks <- c(0, 17.4, 25.7, 32.9)
                                } else if (wind_var == "vmax_gust"){
                                        breaks <- c(0, 26.0, 38.3, 49.0)
                                }
                        }
                } else {
                        breaks <- seq(0, 600, by = 60)
                        exposure_legend <- "Wind duration\n(minutes)"
                }

        }

        if(wind_source == "ext_tracks"){
                exposure_palette <- c("#feb24c", "#fc4e2a", "#b10026")
        } else {
                exposure_palette <- RColorBrewer::brewer.pal(length(breaks) - 2,
                                                             name = palette_name)
        }

        # Adjust for right outliers
        if(max(map_data$value) > max(breaks)){
                breaks <- c(breaks, max(map_data$value))
        }

        exposure_palette <- c("#f7f7f7", exposure_palette, "#1a1a1a")
        if(metric == "distance"){
                exposure_palette <- rev(exposure_palette)
        }

        map_data <- map_data %>%
                dplyr::mutate_(value = ~ cut(value, breaks = breaks,
                                              include.lowest = TRUE))

        if(metric == "distance"){
                level_names <- levels(map_data$value)
                level_names[length(level_names)] <- ">200"
                map_data$value <- factor(map_data$value,
                                         levels = levels(map_data$value),
                                         labels = level_names)
                exposure_palette <- utils::tail(exposure_palette,
                                         length(unique(map_data$value)))
        }

        out_data <- get_eastern_map() %>%
                dplyr::left_join(map_data, by = "fips")
        out <- ggplot2::ggplot() +
                ggplot2::geom_polygon(data = out_data,
                                      ggplot2::aes_(x = ~ long, y = ~ lat, group = ~ group,
                                                    fill = ~ value),
                                      color = "lightgray", size = 0.2) +
                ggplot2::borders("state", regions = c("virginia", "north carolina", "south carolina",
                                                      "georgia", "florida", "alabama", "kentucky",
                                                      "tennessee", "maryland", "west virginia",
                                                      "district of columbia", "pennsylvania",
                                                      "new jersey", "delaware", "mississippi",
                                                      "louisiana", "texas", "oklahoma", "arkansas",
                                                      "new york", "connecticut", "rhode island",
                                                      "massachusetts", "new hampshire", "vermont",
                                                      "maine", "kansas", "missouri", "iowa", "michigan",
                                                      "illinois", "ohio", "wisconsin", "indiana"),
                                 colour = "black", fill = NA, size = 0.2, alpha = 0.5) +
                ggplot2::theme_void() +
                ggplot2::coord_map() +
                ggplot2::scale_fill_manual(name = exposure_legend,
                                           values = exposure_palette)

        return(out)
}
