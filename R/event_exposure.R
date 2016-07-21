#' Find events for storms by county
#'
#' @note Of the event types that this function can pull, only "tornado" and
#'    "wind" are available for years before 1996. Therefore, this function will
#'    pull listed tornado events or wind events for all years, but for any other
#'    event type, you should not use a start year prior to 1996, as events of
#'    other types were not recorded in the database before 1996.
#'
#' @examples
#' county_events(counties = c("37031", "37053", "37055"),
#'               start_year = 1988, end_year = 1999,
#'               event_type = "flood")
#'
#' @export
county_events <- function(counties, start_year, end_year, event_type){
        events <- hurricaneexposuredata::storm_events

        event_years <- as.numeric(gsub(".+-", "", names(events)))
        events <- events[event_years >= start_year & event_years <= end_year]
        events <- events[lapply(events, nrow) > 0]
        events <- purrr::map2(events, names(events), ~ cbind(.x, storm_id = .y))

        events <- lapply(events, function(x) subset(x, x$fips %in% counties))

        events <- do.call("rbind", events) %>%
                dplyr::mutate(flood = grepl("Flood", events),
                              tornado = grepl("Tornado", events) ,
                              tropical_storm = grepl("Hurricane", events) |
                                      grepl("Tropical Storm", events) |
                                      grepl("Tropical Depression", events),
                              wind = tropical_storm | grepl("Wind", events))
        events <- events[events[ , event_type], c("fips", "storm_id")]
        return(events)
}

#' Map county-level exposure based on reported events
#'
#' @examples
#' map_event_exposure(storm_id = "Floyd-1999", event_type = "flood")
#' map_event_exposure(storm_id = "Floyd-1999", event_type = "tornado")
#' map_event_exposure(storm_id = "Floyd-1999", event_type = "wind")
#' map_event_exposure(storm_id = "Floyd-1999", event_type = "tropical_storm")
#'
#' @export
map_event_exposure <- function(storm_id, event_type){
        storm <- storm_id
        storm_year <- gsub("*.+-", "", storm_id)
        counties <- hurricaneexposuredata::closest_dist %>%
                dplyr::filter(storm_id == storm) %>%
                dplyr::select(fips, storm_dist)
        map_data <- county_events(counties = counties$fips,
                                start_year = storm_year,
                                end_year = storm_year,
                                event_type = event_type) %>%
                dplyr::filter(storm_id == storm) %>%
                dplyr::select(fips) %>%
                dplyr::mutate(event = 1) %>%
                dplyr::right_join(counties, by = "fips") %>%
                dplyr::mutate(event = !is.na(event),
                              fips = as.numeric(fips)) %>%
                dplyr::rename(region = fips, value = event) %>%
                dplyr::select(-storm_dist)

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
                                                                  "red"),
                                                       labels = c("Unexposed",
                                                                  "Exposed"))
        return(out$render())
}
