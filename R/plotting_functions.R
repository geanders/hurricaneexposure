#' Create a default map with eastern US states
#'
#' Creates a ggplot object with the underlying map of all states in the
#' eastern section of the US that might be prone to hurricane-related
#' exposure. Other lines and points can be added to the output using
#' `ggplot2` plotting functions.
#'
#' @return A ggplot object that maps the states of the Eastern United States
#'
#' @export
#'
#' @examples
#' default_map()
default_map <- function(){
        map_data <- ggplot2::map_data("state")
        map_data <- dplyr::filter(map_data,
                                  region %in% c("alabama", "arkansas",
                                                "connecticut", "delaware",
                                                "district of columbia", "florida",
                                                "georgia", "illinois", "indiana",
                                                "iowa", "kansas", "kentucky", "louisiana",
                                                "maine", "maryland", "massachusetts",
                                                "michigan", "mississippi",
                                                "missouri", "new hampshire", "new jersey",
                                                "new york", "north carolina", "ohio",
                                                "oklahoma", "pennsylvania", "rhode island",
                                                "south carolina", "tennessee", "texas",
                                                "vermont", "virginia", "west virginia",
                                                "wisconsin"))
        out <- ggplot2::ggplot(map_data, ggplot2::aes(x = long, y = lat, group = group)) +
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
#' @param storm_status TRUE / FALSE indicator of whether to use colors to
#'    indicate storm status (Hurricane, Tropical Storm, Tropical Depression,
#'    or Other) at each time point.
#' @param padding Numerical value giving the number of degrees to add to the
#'    outer limits of the plot object (or default map if `plot_object` is
#'    left as NULL) when cropping hurricane tracks.
#' @param plot_points TRUE / FALSE indicator of whether to include points,
#'    as well as lines, when plotting the hurricane tracks.
#' @param alpha Numerical value designating the amount of transparency to
#'    use for plotting tracks.
#'
#' @return Returns a ggplot object with plotting data for the storm tracks
#'    of the selected storms. This object can be printed directly or added
#'    on to with other ggplot commands.
#'
#' @export
#'
#' @examples
#' map_tracks(storms = "Sandy-2012")
#' map_tracks(storms = "Floyd-1999", storm_status = FALSE,
#'    plot_points = FALSE)
#' a <- map_tracks(storms = "Sandy-2012")
#' b <- map_tracks(storms = "Floyd-1999", storm_status = FALSE,
#'                 plot_object = a, plot_points = FALSE)
#' b
#'
#' @importFrom dplyr %>%
map_tracks <- function(storms, plot_object = NULL,
                      storm_status = TRUE,
                      padding = 2,
                      plot_points = TRUE,
                      alpha = 1){
        if(is.null(plot_object)){
                plot_object <- default_map()
        } else {
                map_data <- plot_object$data
        }
        map_dim <- apply(map_data[ , c("long", "lat")],
                         MARGIN = 2,
                         function(x) range(x) + c(-1, 1) * padding)
        tracks <- dplyr::select(hurr_tracks, date, status,
                                latitude, longitude, storm_id) %>%
                dplyr::filter(as.character(storm_id) %in% storms &
                              longitude > map_dim[1, 1] &
                              longitude < map_dim[2, 1] &
                              latitude > map_dim[1, 2] &
                              latitude < map_dim[2, 2])
        if(storm_status){
                out <- plot_object +
                        ggplot2::geom_path(data = tracks,
                                           ggplot2::aes(x = longitude, y = latitude,
                                                        group = storm_id,
                                                        color = status),
                                           alpha = alpha)
        } else {
                out <- plot_object +
                        ggplot2::geom_path(data = tracks,
                                           ggplot2::aes(x = longitude, y = latitude,
                                                        group = storm_id),
                                           alpha = alpha)
        }

        if(plot_points){
                if(storm_status){
                        out <- out +
                                ggplot2::geom_point(data = tracks,
                                                    ggplot2::aes(x = longitude, y = latitude,
                                                                 group = storm_id,
                                                                 color = status),
                                                    alpha = alpha)
                } else {
                        out <- out +
                                ggplot2::geom_point(data = tracks,
                                                    ggplot2::aes(x = longitude, y = latitude,
                                                                 group = storm_id),
                                                    alpha = alpha)
                }
        }
        return(out)
}

#' Map counties
#'
#' @export
#'
#' @importFrom dplyr %>%
map_counties <-function(storm, metric = "closest distance"){
        if(metric == "closest distance"){
                metric_df <- closest_dist
                metric_df$value <- metric_df$storm_dist
        } else if(metric == "weekly rainfall"){
                metric_df <- storm_rains
                metric_df$value <- metric_df$tot_precip
        } else{
                stop("`metric` must be either `closest distance` or `weekly rainfall`")
        }
        map_data <- dplyr::filter(metric_df,
                                  storm_id == storm) %>%
                mutate(region = as.numeric(fips)) %>%
                select(region, value)
        out <- choroplethr::county_choropleth(map_data,
                                              state_zoom = c("alabama", "arkansas",
                                                       "connecticut", "delaware",
                                                       "district of columbia", "florida",
                                                       "georgia", "illinois", "indiana",
                                                       "iowa", "kansas", "kentucky", "louisiana",
                                                       "maine", "maryland", "massachusetts",
                                                       "michigan", "mississippi",
                                                       "missouri", "new hampshire", "new jersey",
                                                       "new york", "north carolina", "ohio",
                                                       "oklahoma", "pennsylvania", "rhode island",
                                                       "south carolina", "tennessee", "texas",
                                                       "vermont", "virginia", "west virginia",
                                                       "wisconsin"))
}
