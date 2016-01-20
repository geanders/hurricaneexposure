#' Create a default map with eastern US states
#'
#' Creates a ggplot object with the underlying map of all states in the
#' eastern section of the US that might be prone to hurricane-related
#' exposure. Other lines and points can be added to the output using
#' `ggplot2` plotting functions.
#'
#' @return A ggplot object that maps the states of the Eastern United States
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

map_tracks <- function(storms, plot_object = NULL,
                      storm_status = TRUE,
                      padding = 2,
                      plot_points = TRUE,
                      alpha = 1){
        if(is.null(plot_object)){
                plot_object <- default_map()
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
