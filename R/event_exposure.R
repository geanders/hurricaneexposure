#' Find events for storms by county
#'
#' Takes a list of US counties, based on their 5-digit Federal Information Processing
#' Standard (FIPS) codes, boundaries on the range of years to be considered, and
#' the type of event (e.g., "flood", "tornado") and returns a list of all storms with
#' listings for that type of event for the counties. This output is based on listings in
#' the NOAA Storm Events database. See the help documentation for the \code{storm_events}
#' dataset for more information and references on the data source used by this function.
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
#' #' @return Returns a dataframe with a row for each county-storm pair and with
#'    columns for:
#'    \itemize{
#'      \item{\code{fips}: }{County's 5-digit Federal Information Processing Standard
#'                  (FIPS) code}
#'      \item{\code{storm_id}: }{Unique storm identifier with the storm name and year,
#'                  separated by a hyphen(e.g., "Alberto-1988",
#'                  "Katrina-2005")}
#'     \item{\code{closest_time_utc}: }{Time, in UTC, of the closest approach of the
#'                             storm to the county's population mean center,
#'                             based on storm tracks linearly interpolated to
#'                             15-minute increments.}
#'      \item{\code{storm_dist}: }{Minimum distance (in kilometers) between the storm's
#'                     track and the county's population mean center.}
#'      \item{\code{local_time}: }{Local time of the closest approach of the storm to the
#'                        county's population mean center, based on storm tracks
#'                        linearly interpolated to 15-minute increments.}
#'      \item{\code{closest_date}: }{Date (based on local time) of the closest
#'                          approach of the storm to the county's population
#'                          mean center.}
#'    }
#'
#' @examples
#' # Ensure that data package is available before running the example.
#' #  If it is not, see the `hurricaneexposure` package vignette for details
#' # on installing the required data package.
#' if (requireNamespace("hurricaneexposuredata", quietly = TRUE)) {
#'
#' county_events(counties = c("37031", "37053", "37055"),
#'               start_year = 1996, end_year = 2015,
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

