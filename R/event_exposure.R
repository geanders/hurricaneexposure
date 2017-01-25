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
#' @examples
#' # Ensure that data package is available before running the example.
#' #  If it is not, see the `hurricaneexposure` package vignette for details
#' # on installing the required data package.
#' if (requireNamespace("hurricaneexposuredata", quietly = TRUE)) {
#'
#' county_events(counties = c("37031", "37053", "37055"),
#'               start_year = 1988, end_year = 1999,
#'               event_type = "flood")
#' }
#' @importFrom dplyr %>%
#'
#' @export
county_events <- function(counties, start_year, end_year, event_type){

        hasData()

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

