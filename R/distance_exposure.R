#' Hurricane exposure by distance for counties
#'
#' This function takes a list of US counties,based on their 5-digit Federal
#' Information Processing Standard (FIPS) codes, boundaries on
#' the range of years to be considered, and thresholds for distance between each
#' county and the storm track for the county to be considered "exposed" to the
#' storm. Based on these inputs, the function returns a dataframe with the
#' subset of Atlantic basin storms meeting those criteria for each of the listed
#' counties.
#'
#' @param counties Character vector of the five-digit Federal Information
#'    Processing Standard (FIPS) codes for counties for which the user wants to
#'    determine storm exposure.
#' @param start_year Four-digit integer with first year to consider.
#' @param end_year Four-digit integer with last year to consider.
#' @param dist_limit Maximum distance, in kilometers, of how close the storm
#'    track must come to the county's population mean center to classify
#'    the county as "exposed" to the storm.
#'
#' @return Returns a dataframe with a row for each county-storm
#'    pair and with columns for:
#'    \itemize{
#'      \item{\code{storm_id}: }{Unique storm identifier with the storm name and year,
#'                  separated by a hyphen(e.g., "Alberto-1988",
#'                  "Katrina-2005")}
#'      \item{\code{fips}: }{County's 5-digit Federal Information Processing Standard
#'                  (FIPS) code}
#'      \item{\code{closest_date}: }{Date (based on local time) of the closest
#'                          approach of the storm to the county's population
#'                          mean center.}
#'      \item{\code{storm_dist}: }{Minimum distance (in kilometers) between the storm's
#'                     track and the county's population mean center.}
#'      \item{\code{local_time}: }{Local time of the closest approach of the storm to the
#'                        county's population mean center, based on storm tracks
#'                        linearly interpolated to 15-minute increments.}
#'     \item{\code{closest_time_utc}: }{Time, in UTC, of the closest approach of the
#'                             storm to the county's population mean center,
#'                             based on storm tracks linearly interpolated to
#'                             15-minute increments.}
#'    }
#'
#' @details For more information on how distances between counties and storm
#'    tracks are calculated for this function, see the documentation for the
#'    \code{closest_dist} dataset that comes with this package.
#'
#' @note Only counties in states in the eastern half of the United States can
#'    be processed by this function.
#'
#' @examples
#' # Ensure that data package is available before running the example.
#' #  If it is not, see the `hurricaneexposure` package vignette for details
#' # on installing the required data package.
#' if (requireNamespace("hurricaneexposuredata", quietly = TRUE)) {
#'
#'   county_distance(counties = c("22071", "51700"),
#'               start_year = 1995, end_year = 2005,
#'               dist_limit = 75)
#'
#'}
#' @export
#'
#' @importFrom dplyr %>%
county_distance <- function(counties, start_year, end_year, dist_limit){

        distance_df <- filter_storm_data(counties = counties,
                                         year_range = c(start_year, end_year),
                                         distance_limit = dist_limit,
                                         output_vars = c("storm_id", "fips",
                                                         "closest_date",
                                                         "storm_dist",
                                                         "local_time",
                                                         "closest_time_utc"))

        return(distance_df)
}

#' Hurricane exposure by distance for communities
#'
#' This function takes a dataframe with multi-county communities and returns a
#' community-level dataframe of "exposed" storms, based on the shortest distance
#' between the storm's track and the population-based centers of each county
#' in the community.
#'
#' @inheritParams county_distance
#' @inheritParams county_rain
#' @inheritParams multi_county_rain
#'
#' @return Returns the same type dataframe as \code{county_distance},
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
#' communities <- data.frame(commun = c(rep("ny", 6), "no", "new"),
#'                          fips = c("36005", "36047", "36061",
#'                                   "36085", "36081", "36119",
#'                                   "22071", "51700"))
#' distance_df <- multi_county_distance(communities = communities,
#'                                      start_year = 1995, end_year = 2005,
#'                                      dist_limit = 75)
#'}
#' @importFrom dplyr %>%
multi_county_distance <- function(communities, start_year, end_year,
                         dist_limit){

        hasData()

        communities <- dplyr::mutate_(communities, fips = ~ as.character(fips))

        distance_df <- hurricaneexposuredata::closest_dist %>%
                dplyr::mutate_(closest_date =
                                       ~ lubridate::ymd(closest_date)) %>%
                dplyr::filter_(~ fips %in% communities$fips &
                                      lubridate::year(closest_date) >=
                                       start_year &
                                      lubridate::year(closest_date) <=
                                       end_year) %>%
                dplyr::left_join(communities, by = "fips") %>%
                dplyr::group_by_(~ commun, ~ storm_id) %>%
                dplyr::mutate_(min_dist = ~ min(storm_dist)) %>%
                dplyr::filter_(~ min_dist <= dist_limit) %>%
                dplyr::summarize_(closest_date = ~ dplyr::first(closest_date),
                                  local_time = ~ dplyr::first(local_time),
                                  closest_time_utc = ~ dplyr::first(closest_time_utc),
                                 mean_dist = ~ mean(storm_dist),
                                 min_dist = ~ dplyr::first(min_dist))
        return(distance_df)

}

#' Write storm distance exposure files
#'
#' This function takes an input of locations (either a vector of county FIPS
#' or a dataframe of multi-county FIPS, with all FIPS listed for each county)
#' and creates a dataframe with storm listings and dates that can be merged with
#' time series of health or other outcomes, giving the dates and exposures for all
#' storms meeting the given storm distance criteria.
#'
#' @inheritParams county_distance
#' @inheritParams county_rain
#' @inheritParams rain_exposure
#'
#' @return Writes out a directory with rain exposure files for each county or
#'    community indicated. For more on the columns in this output, see the
#'    documentation for \code{\link{county_rain}} and
#'    \code{\link{multi_county_rain}}.
#'
#' @examples \dontrun{
#' # Ensure that data package is available before running the example.
#' #  If it is not, see the `hurricaneexposure` package vignette for details
#' # on installing the required data package.
#' if (requireNamespace("hurricaneexposuredata", quietly = TRUE)) {
#'
#' # By county
#' distance_exposure(locations = c("22071", "51700"),
#'               start_year = 1995, end_year = 2005,
#'               dist_limit = 75,
#'               out_dir = "~/tmp/storms")
#'
#' # For multi-county communities
#' communities <- data.frame(commun = c(rep("ny", 6), "no", "new"),
#'                           fips = c("36005", "36047", "36061",
#'                           "36085", "36081", "36119",
#'                           "22071", "51700"))
#' distance_exposure(locations = communities,
#'               start_year = 1995, end_year = 2005,
#'               dist_limit = 75,
#'               out_dir = "~/tmp/storms")
#' }
#' }
#' @export
#'
#' @importFrom dplyr %>%
distance_exposure <- function(locations, start_year, end_year,
                          dist_limit, out_dir, out_type = "csv"){

        if(!dir.exists(out_dir)){
                dir.create(out_dir)
        }

        if("commun" %in% colnames(locations)){
                df <- multi_county_distance(communities = locations,
                                  start_year = start_year,
                                  end_year = end_year,
                                  dist_limit = dist_limit) %>%
                        dplyr::rename_(loc = ~ commun) %>%
                        dplyr::ungroup()
        } else {
                df <- county_distance(counties = locations,
                                  start_year = start_year,
                                  end_year = end_year,
                                  dist_limit = dist_limit) %>%
                        dplyr::rename_(loc = ~ fips)
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
