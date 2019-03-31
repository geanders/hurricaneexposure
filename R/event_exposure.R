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
#' @importFrom rlang .data
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
                dplyr::mutate(flood = grepl("Flood", .data$events),
                               tornado = grepl("Tornado", .data$events),
                               tropical_storm = grepl("Hurricane", .data$events) |
                                      grepl("Tropical Storm", .data$events) |
                                      grepl("Tropical Depression", .data$events),
                               wind = .data$tropical_storm | grepl("Wind", .data$events))
        events <- events[events[ , event_type], c("fips", "storm_id")] %>%
                dplyr::mutate(storm_id = as.character(.data$storm_id)) %>%
                dplyr::left_join(hurricaneexposuredata::closest_dist,
                                 by = c("storm_id", "fips"))
        return(events)
}

#' Hurricane exposure by events for communities
#'
#' This function takes a dataframe with multi-county communities and returns a
#' community-level dataframe of "exposed" storms, based on the type of the event.
#'
#' @inheritParams county_distance
#' @inheritParams county_rain
#' @inheritParams multi_county_rain
#' @inheritParams county_events
#'
#' @return Returns the same type dataframe as \code{county_events},
#'    but with storms listed by community instead of county.
#'
#' @export
#'
#' @examples
#' # Ensure that data package is available before running the example.
#' #  If it is not, see the `hurricaneexposure` package vignette for details
#' # on installing the required data package.
#' if (requireNamespace("hurricaneexposuredata", quietly = TRUE)) {
#'
#' communities <- data.frame(community_name = c(rep("ny", 6), "no", "new"),
#'                          fips = c("36005", "36047", "36061",
#'                                   "36085", "36081", "36119",
#'                                   "22071", "51700"))
#' distance_df <- multi_county_events(communities = communities,
#'                                      start_year = 1995, end_year = 2005,
#'                                      event_type = "flood")
#'}
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#'
#' @export
multi_county_events <- function(communities, start_year, end_year, event_type){

        hasData()

        communities <- dplyr::mutate(communities, fips = as.character(.data$fips))

        events <- hurricaneexposuredata::storm_events

        event_years <- as.numeric(gsub(".+-", "", names(events)))
        events <- events[event_years >= start_year & event_years <= end_year]
        events <- events[lapply(events, nrow) > 0]
        events <- purrr::map2(events, names(events), ~ cbind(.x, storm_id = .y))

        events <- lapply(events, function(x) subset(x, x$fips %in% communities$fips))

        events <- do.call("rbind", events) %>%
                dplyr::mutate(flood = grepl("Flood", .data$events),
                               tornado = grepl("Tornado", .data$events),
                               tropical_storm = grepl("Hurricane", .data$events) |
                                       grepl("Tropical Storm", .data$events) |
                                       grepl("Tropical Depression", .data$events),
                               wind = .data$tropical_storm | grepl("Wind", .data$events))

        events <- events[events[ , event_type], c("fips", "storm_id")] %>%
                dplyr::mutate(storm_id = as.character(.data$storm_id)) %>%
                dplyr::left_join(hurricaneexposuredata::closest_dist,
                                 by = c("storm_id", "fips")) %>%
                dplyr::left_join(communities, by = "fips") %>%
                dplyr::group_by(.data$community_name, .data$storm_id) %>%
                dplyr::summarize(closest_date = dplyr::first(.data$closest_date),
                                 local_time = dplyr::first(.data$local_time),
                                 closest_time_utc = dplyr::first(.data$closest_time_utc))
        return(events)
}

#' Write storm events exposure files
#'
#' This function takes an input of locations (either a vector of county FIPS
#' or a dataframe of multi-county FIPS, with all FIPS listed for each county)
#' and creates a dataframe with storm listings and dates that can be merged with
#' time series of health or other outcomes, giving the dates and exposures for all
#' storms meeting the given storm events criteria.
#'
#' @inheritParams county_events
#' @inheritParams county_distance
#' @inheritParams rain_exposure
#' @param out_dir Character string with the filepath to the directory where the
#'   data will be saved
#' @param out_type Character string with the type of file to save to. Options are
#'   "csv" for a comma-separated file (default) and "rds" for an R object file.
#'
#' @return Writes out a directory with rain exposure files for each county or
#'    community indicated. For more on the columns in this output, see the
#'    documentation for \code{\link{county_rain}} and
#'    \code{\link{multi_county_rain}}.
#'
#' @examples
#' \dontrun{
#' # Ensure that data package is available before running the example.
#' #  If it is not, see the `hurricaneexposure` package vignette for details
#' # on installing the required data package.
#' if (requireNamespace("hurricaneexposuredata", quietly = TRUE)) {
#'
#' # To run this example, you will need to have a directory named "tmp"
#' # as a subdirectory of your home directory.
#'
#' # By county
#' events_exposure(locations = c("22071", "51700"),
#'                 start_year = 1995, end_year = 2005,
#'                 event_type = "flood",
#'                 out_dir = "~/tmp/storms")
#'                 }
#'  }
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#'
#' @export
events_exposure <- function(locations, start_year, end_year,
                            event_type, out_dir, out_type = "csv"){

        if(!dir.exists(out_dir)){
                dir.create(out_dir)
        }

        if("community_name" %in% colnames(locations)){
                df <- multi_county_events(communities = locations,
                                          start_year = start_year,
                                          end_year = end_year,
                                          event_type = event_type) %>%
                        dplyr::rename(loc = .data$community_name) %>%
                        dplyr::ungroup()
        } else {
                df <- county_events(counties = locations,
                                    start_year = start_year,
                                    end_year = end_year,
                                    event_type = event_type) %>%
                        dplyr::rename(loc = .data$fips)
        }
        locs <- as.character(unique(df$loc))

        for(i in 1:length(locs)){
                out_df <- dplyr::filter_(df, ~ loc == locs[i])
                out_file <- paste0(out_dir, "/", locs[i], ".", out_type)
                if(out_type == "rds"){
                        saveRDS(out_df, file = out_file)
                } else if (out_type == "csv"){
                        utils::write.csv(out_df, file = out_file,
                                         row.names = FALSE)
                }

        }
}
