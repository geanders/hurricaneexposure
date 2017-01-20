#' Find events for storms by county
#'
#' @param event_type Character string with the type of event to use to identify
#'    county exposures. Options include \code{"flood"}, \code{"tornado"},
#'    \code{"wind"}, and \code{"tropical storm"}.
#' @inheritParams county_distance
#'
#' @note Of the event types that this function can pull, only "tornado" and
#'    "wind" are available for years before 1996. Therefore, this function will
#'    pull listed tornado events or wind events for all years, but for any other
#'    event type, you should not use a start year prior to 1996, as events of
#'    other types were not recorded in the database before 1996.
#'
#' @examples \dontrun{
#' county_events(counties = c("37031", "37053", "37055"),
#'               start_year = 1988, end_year = 1999,
#'               event_type = "flood")
#' }
#' @importFrom dplyr %>%
#'
#' @export
county_events <- function(counties, start_year, end_year, event_type){

        if(!.pkgglobalenv$has_data){
                stop(paste("To use this function, you must have the",
                           "`hurricaneexposuredata` package installed. See the",
                           "`hurricaneexposure` package vignette for more details."))
        }

        events <- hurricaneexposuredata::storm_events

        event_years <- as.numeric(gsub(".+-", "", names(events)))
        events <- events[event_years >= start_year & event_years <= end_year]
        events <- events[lapply(events, nrow) > 0]
        events <- purrr::map2(events, names(events), ~ cbind(.x, storm_id = .y))

        events <- lapply(events, function(x) subset(x, x$fips %in% counties))

        events <- do.call("rbind", events) %>%
                dplyr::mutate_(flood = ~ grepl("Flood", events),
                               tornado = ~ grepl("Tornado", events),
                               tropical_storm = ~ grepl("Hurricane", events) |
                                      grepl("Tropical Storm", events) |
                                      grepl("Tropical Depression", events),
                               wind = ~ tropical_storm | grepl("Wind", events))
        events <- events[events[ , event_type], c("fips", "storm_id")] %>%
                dplyr::mutate_(storm_id = ~ as.character(storm_id)) %>%
                dplyr::left_join(hurricaneexposuredata::closest_dist,
                                 by = c("storm_id", "fips"))
        return(events)
}

#' Map county-level exposure based on reported events
#'
#' @param storm_id Character vector with the storm for which to map events
#'    (e.g., \code{"Katrina-2005"})
#' @inheritParams county_distance
#' @inheritParams county_events
#'
#' @examples \dontrun{
#' map_event_exposure(storm_id = "Floyd-1999", event_type = "flood")
#' map_event_exposure(storm_id = "Floyd-1999", event_type = "tornado")
#' map_event_exposure(storm_id = "Floyd-1999", event_type = "wind")
#' map_event_exposure(storm_id = "Floyd-1999", event_type = "tropical_storm")
#' }
#' @export
map_event_exposure <- function(storm_id, event_type){

        if(!.pkgglobalenv$has_data){
                stop(paste("To use this function, you must have the",
                           "`hurricaneexposuredata` package installed. See the",
                           "`hurricaneexposure` package vignette for more details."))
        }

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
                dplyr::mutate_(event = ~ !is.na(event),
                               fips = ~ as.numeric(fips)) %>%
                dplyr::rename_(region = ~ fips, value = ~ event) %>%
                dplyr::select_(quote(-storm_dist))

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
        return(suppressWarnings(out$render()))
}
