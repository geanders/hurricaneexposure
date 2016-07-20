#' Create a default map with eastern US states
#'
#' Creates a \code{ggplot} object with the underlying map of all states in the
#' eastern section of the US that might be prone to hurricane-related
#' exposure. Other lines and points can be added to the output using
#' `ggplot2` plotting functions.
#'
#' @details Only states in the eastern half of the United States (i.e., ones
#' prone to exposure to Atlantic basin tropical storms) are included on this
#' map.
#'
#' @return A ggplot object that maps the states of the Eastern United States
#'
#' @export
#'
#' @examples
#' default_map()
default_map <- function(){

        eastern_states <- c("alabama", "arkansas", "connecticut", "delaware",
                            "district of columbia", "florida", "georgia", "illinois",
                            "indiana", "iowa", "kansas", "kentucky", "louisiana",
                            "maine", "maryland", "massachusetts", "michigan",
                            "mississippi", "missouri", "new hampshire", "new jersey",
                            "new york", "north carolina", "ohio", "oklahoma",
                            "pennsylvania", "rhode island", "south carolina",
                            "tennessee", "texas", "vermont", "virginia",
                            "west virginia", "wisconsin")

        map_data <- ggplot2::map_data("state")
        map_data <- map_data %>%
                dplyr::filter_(~ region %in% eastern_states)
        out <- ggplot2::ggplot(map_data,
                               ggplot2::aes_(x = ~ long, y = ~ lat,
                                             group = ~ group)) +
                ggplot2::geom_polygon(fill = "lightgray", color = "white") +
                ggmap::theme_nothing(legend = TRUE)
        return(out)
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
#'    as well as lines, when plotting the hurricane tracks.
#' @param alpha Numerical value designating the amount of transparency to
#'    use for plotting tracks.
#' @param color Character string giving the color to use to plot the tracks.
#'
#' @return Returns a ggplot object with plotting data for the storm tracks
#'    of the selected storms. This object can be printed directly or added
#'    on to with other ggplot commands.
#'
#' @export
#'
#' @examples
#' map_tracks(storms = "Sandy-2012")
#' map_tracks(storms = "Floyd-1999", plot_points = FALSE)
#' a <- map_tracks(storms = "Sandy-2012")
#' b <- map_tracks(storms = "Floyd-1999", plot_object = a, plot_points = FALSE)
#' b
#'
#' @importFrom dplyr %>%
map_tracks <- function(storms, plot_object = NULL,
                      padding = 2,
                      plot_points = TRUE,
                      alpha = 1,
                      color = "firebrick"){
        if(is.null(plot_object)){
                plot_object <- default_map()
        }

        map_data <- plot_object$data
        map_dim <- apply(map_data[ , c("long", "lat")],
                         MARGIN = 2,
                         function(x) range(x) + c(-1, 1) * padding)
        tracks <- hurricaneexposuredata::hurr_tracks %>%
                dplyr::select_(~ latitude, ~ longitude, ~ storm_id) %>%
                dplyr::filter_(~ as.character(storm_id) %in% storms &
                              longitude > map_dim[1, 1] &
                              longitude < map_dim[2, 1] &
                              latitude > map_dim[1, 2] &
                              latitude < map_dim[2, 2])
        out <- plot_object +
                        ggplot2::geom_path(data = tracks,
                                           ggplot2::aes_(x = ~ longitude,
                                                        y = ~ latitude,
                                                        group = ~ storm_id),
                                           alpha = alpha,
                                           color = color)

        if(plot_points){
                out <- out + ggplot2::geom_point(data = tracks,
                                                    ggplot2::aes_(x = ~ longitude,
                                                                 y = ~ latitude,
                                                                 group = ~ storm_id),
                                                    alpha = alpha)
        }
        return(out)
}

#' Map counties
#'
#' @param storm Character string giving the name of the storm to plot (e.g.,
#'    "Floyd-1999")
#' @param metric Character string giving the metric to plot. Current options are
#'    \code{"distance"} (default) and \code{"rainfall"}. These options are used
#'    to customize the color palette and scale of the choropleth map produced
#'    by this function.
#' @inheritParams county_rain
#'
#' @return This function creates a choropleth map of counties in the eastern
#'    part of the United States, showing distance from a storm track or total
#'    rainfall over a given window of one or more days.
#'
#' @examples
#' floyd_map <- map_counties("Floyd-1999", metric = "rainfall",
#'                            days_included = c(-1, 0, 1))
#' floyd_map
#'
#' beryl_map <- map_counties("Beryl-1988", metric = "wind")
#'
#' @export
#'
#' @importFrom dplyr %>%
map_counties <-function(storm, metric = "distance",
                        days_included = c(-1, 0, 1)){
        if(metric == "distance"){
                map_data <- filter_storm_data(storm = storm,
                                              output_vars = c("fips",
                                                              "storm_dist")) %>%
                        dplyr::rename_(region = ~ fips, value = ~ storm_dist)
        } else if(metric == "rainfall"){
                map_data <- filter_storm_data(storm = storm, include_rain = TRUE,
                                              days_included = days_included,
                                              output_vars = c("fips",
                                                              "tot_precip")) %>%
                        dplyr::rename_(region = ~ fips, value = ~ tot_precip)
        } else if (metric == "wind") {
                map_data <- filter_wind_data(storm = storm,
                                             output_vars = c("fips",
                                                             "max_sust")) %>%
                        dplyr::rename_(region = ~ fips, value = ~ max_sust)
        } else{
                stop("`metric` must be either `distance`, `rainfall`, or `wind`")
        }
        map_data <- map_data %>%
                dplyr::mutate_(region = ~ as.numeric(region)) %>%
                dplyr::tbl_df()
        out <- hurr_choroplethr(map_data, metric = metric)
        return(out$render())
}

#' Map counties with rain exposure
#'
#' Map counties as "exposed" or "unexposed" based on the criteria that the
#' storm came within a given distance (specified by \code{dist_limit}) of
#' the county's population mean center and a certain amount of rain
#' \code{rain_limit} fell during a specified window of days (\code{days_included}).
#'
#' @inheritParams county_distance
#' @inheritParams county_rain
#' @inheritParams map_counties
#'
#' @examples
#'\dontrun{
#' floyd_map <- map_rain_exposure(storm = "Floyd-1999", rain_limit = 50,
#'                                dist_limit = 100)
#' floyd_map
#'
#' allison_map <- map_rain_exposure(storm = "Allison-2001", rain_limit = 20,
#'                                  dist_limit = 100, days_included = 0)
#' map_tracks("Allison-2001", plot_points = FALSE, plot_object = allison_map)
#'}
#' @importFrom dplyr %>%
#'
#' @export
map_rain_exposure <- function(storm, rain_limit, dist_limit,
                              days_included = c(-1, 0, 1)){

        map_data <- filter_storm_data(storm = storm,
                                           days_included = days_included,
                                           include_rain = TRUE,
                                           output_vars = c("fips", "tot_precip",
                                                           "storm_dist")) %>%
                dplyr::mutate_(exposed = ~ tot_precip >= rain_limit &
                                       storm_dist <= dist_limit) %>%
                dplyr::mutate_(value = ~ factor(exposed,
                                                levels = c("FALSE", "TRUE"))) %>%
                dplyr::mutate_(region = ~ as.numeric(fips)) %>%
                dplyr::select_(~ region, ~ value) %>%
                dplyr::tbl_df()

        eastern_states <- c("alabama", "arkansas", "connecticut", "delaware",
                            "district of columbia", "florida", "georgia", "illinois",
                            "indiana", "iowa", "kansas", "kentucky", "louisiana",
                            "maine", "maryland", "massachusetts", "michigan",
                            "mississippi", "missouri", "new hampshire", "new jersey",
                            "new york", "north carolina", "ohio", "oklahoma",
                            "pennsylvania", "rhode island", "south carolina",
                            "tennessee", "texas", "vermont", "virginia",
                            "west virginia", "wisconsin")

        out <- choroplethr::CountyChoropleth$new(map_data)
        out$set_zoom(eastern_states)
        out$ggplot_scale <- ggplot2::scale_fill_manual(name = "",
                                                       values = c("white",
                                                                  "navy"),
                                                       labels = c("Unexposed",
                                                                  "Exposed"))
        return(out$render())
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
#'
#' @return Plots a map showing whether eastern US counties were exposed or
#'    unexposed to a specific storm based on a distance criterion.
#'
#' @examples
#'
#' floyd_map <- map_distance_exposure(storm = "Floyd-1999", dist_limit = 75)
#' floyd_map
#'
#' allison_map <- map_distance_exposure(storm = "Allison-2001",
#'                                      dist_limit = 75)
#' map_tracks("Allison-2001", plot_points = FALSE, plot_object = allison_map)
#'
#' @importFrom dplyr %>%
#'
#' @export
map_distance_exposure <- function(storm, dist_limit){

        map_data <- filter_storm_data(storm = storm,
                                      output_vars = c("fips", "storm_dist")) %>%
                dplyr::mutate_(exposed = ~ storm_dist <= dist_limit) %>%
                dplyr::mutate_(value = ~ factor(exposed,
                                                levels = c("FALSE", "TRUE"))) %>%
                dplyr::mutate_(region = ~ as.numeric(fips)) %>%
                dplyr::select_(~ region, ~ value) %>%
                dplyr::tbl_df()

        eastern_states <- c("alabama", "arkansas", "connecticut", "delaware",
                            "district of columbia", "florida", "georgia", "illinois",
                            "indiana", "iowa", "kansas", "kentucky", "louisiana",
                            "maine", "maryland", "massachusetts", "michigan",
                            "mississippi", "missouri", "new hampshire", "new jersey",
                            "new york", "north carolina", "ohio", "oklahoma",
                            "pennsylvania", "rhode island", "south carolina",
                            "tennessee", "texas", "vermont", "virginia",
                            "west virginia", "wisconsin")

        out <- choroplethr::CountyChoropleth$new(map_data)
        out$set_zoom(eastern_states)
        out$ggplot_scale <- ggplot2::scale_fill_manual(name = "",
                                                       values = c("white",
                                                                  "forestgreen"),
                                                       labels = c("Unexposed",
                                                                  "Exposed"))
        return(out$render())
}

#' Map counties with wind exposure
#'
#' Map counties as "exposed" or "unexposed" based on the criterion that the
#' storm had a certain wind speed (specified by \code{wind_limit}, in meters
#' per second).
#'
#' @inheritParams filter_wind_data
#' @inheritParams county_rain
#'
#' @return Plots a map showing whether eastern US counties were exposed or
#'    unexposed to a specific storm based on a wind criterion.
#'
#' @examples
#'
#' beryl_map <- map_wind_exposure(storm = "Beryl-1988", wind_limit = 15)
#' beryl_map
#' map_tracks("Beryl-1988", plot_points = FALSE, plot_object = beryl_map)
#'
#' @importFrom dplyr %>%
#'
#' @export
map_wind_exposure <- function(storm, wind_limit){

        map_data <- filter_wind_data(storm = storm,
                                      output_vars = c("fips", "max_gust",
                                                      "max_sust")) %>%
                dplyr::mutate_(exposed = ~ max_sust >= wind_limit) %>%
                dplyr::mutate_(value = ~ factor(exposed,
                                                levels = c("FALSE", "TRUE"))) %>%
                dplyr::mutate_(region = ~ as.numeric(fips)) %>%
                dplyr::select_(~ region, ~ value) %>%
                dplyr::tbl_df()

        eastern_states <- c("alabama", "arkansas", "connecticut", "delaware",
                            "district of columbia", "florida", "georgia", "illinois",
                            "indiana", "iowa", "kansas", "kentucky", "louisiana",
                            "maine", "maryland", "massachusetts", "michigan",
                            "mississippi", "missouri", "new hampshire", "new jersey",
                            "new york", "north carolina", "ohio", "oklahoma",
                            "pennsylvania", "rhode island", "south carolina",
                            "tennessee", "texas", "vermont", "virginia",
                            "west virginia", "wisconsin")

        out <- choroplethr::CountyChoropleth$new(map_data)
        out$set_zoom(eastern_states)
        out$ggplot_scale <- ggplot2::scale_fill_manual(name = "",
                                                       values = c("white",
                                                                  "darkorange"),
                                                       labels = c("Unexposed",
                                                                  "Exposed"))
        return(out$render())
}

#' Create a map customized for this package
#'
#' This function creates a county choropleth map customized for displaying
#' hurricane exposure. It provides a wrapper for the \code{CountyChoropleth}
#' function from the \code{choroplethr} package, with customization for the
#' purposes of the maps created for this package.
#'
#' @param map_data A dataframe with columns with FIPS numbers (in numeric
#'    class) for all counties in the eastern US (\code{region}) and the
#'    exposure value (\code{value})
#' @inheritParams map_counties
#'
#' @return A \code{choroplethr} object. To plot the map, use the \code{render}
#'    method.
#'
#' @details The function only maps counties in states likely to be exposed
#' to Atlantic basin tropical storms.
hurr_choroplethr <- function(map_data, metric = "distance"){

        if(metric == "rainfall"){
                breaks <- seq(0, 200, by = 25)
                palette_name <- "Blues"
                exposure_legend <- "Rainfall (mm)"
        } else if(metric == "distance"){
                breaks <- seq(0, 200, by = 25)
                palette_name <- "Greens"
                exposure_legend <- "Distance (km)"
        } else if(metric == "wind"){
                breaks <- c(0, seq(15, 45, by = 5))
                palette_name <- "Reds"
                exposure_legend <- "Wind speed (m / s)"
        }

        exposure_palette <- RColorBrewer::brewer.pal(length(breaks) - 2,
                                                     name = palette_name)
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

        eastern_states <- c("alabama", "arkansas", "connecticut", "delaware",
                            "district of columbia", "florida", "georgia", "illinois",
                            "indiana", "iowa", "kansas", "kentucky", "louisiana",
                            "maine", "maryland", "massachusetts", "michigan",
                            "mississippi", "missouri", "new hampshire", "new jersey",
                            "new york", "north carolina", "ohio", "oklahoma",
                            "pennsylvania", "rhode island", "south carolina",
                            "tennessee", "texas", "vermont", "virginia",
                            "west virginia", "wisconsin")

        out <- choroplethr::CountyChoropleth$new(map_data)
        out$set_zoom(eastern_states)
        out$ggplot_scale <- ggplot2::scale_fill_manual(name = exposure_legend,
                                                       values = exposure_palette)
        return(out)
}
