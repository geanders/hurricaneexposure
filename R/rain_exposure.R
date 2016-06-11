#' Hurricane exposure by rain for counties
#'
#' This function takes a list of county FIPS, boundaries on
#' starting and ending years, and thresholds for distance
#' and rainfall over the days included and returns a dataframe
#' with the subset of storms meeting those criteria for
#' each of the listed counties.
#'
#' @param counties Character string of the five-digit FIPS codes for
#'    counties for which the user wants to create data
#' @param start_year Four-digit integer with first year to consider.
#' @param end_year Four-digit integer with last year to consider.
#' @param rain_limit Minimum of rainfall, in millimeters, summed across the days
#'    selected to be included, that must be met to consider county
#'    "exposed" to the storm.
#' @param days_included A numeric vector listing the days to include when
#'    calculating total precipitation. Negative numbers are days before the
#'    closest date of the storm to a county. For example,
#'    \code{c(-1, 0, 1)} would calculate rain for a county as the sum of the
#'    rainfall for the day before, the day of, and the day after the date when
#'    the storm center was closest to the county center.
#' @param dist_limit Maximum distance, in kilometers, of how close the storm
#'    track must come to the county's population mean center to classify
#'    the county as "exposed" to the storm.
#'
#' @return Returns a dataframe with a row for each county-storm
#'    pair and with columns giving the storm's ID, county FIPS,
#'    date and time of closest approach of that storm to that county,
#'    distance between the county center and the storm track on
#'    that date and time, and cumulative rainfall for the week-long
#'    period centered at the date of closest approach to the county.
#'
#' @examples
#' \dontrun{
#' county_rain(counties = c("22071", "51700"),
#'             start_year = 1995, end_year = 2005,
#'             rain_limit = 100, dist_limit = 100)
#'}
#'
#' @importFrom dplyr %>%
#'
#' @export
county_rain <- function(counties, start_year, end_year,
                           rain_limit, dist_limit,
                        days_included = c(-1, 0, 1)){

        rain_storm_df <- filter_storm_data(counties = counties,
                                           year_range = c(start_year, end_year),
                                           distance_limit = dist_limit,
                                           rain_limit = rain_limit,
                                           include_rain = TRUE,
                                           days_included = days_included,
                                           output_vars = c("storm_id", "fips",
                                                           "closest_date",
                                                           "storm_dist",
                                                           "tot_precip",
                                                           "local_time",
                                                           "closest_time_utc"))
        return(rain_storm_df)
}

#' Hurricane exposure by rain for communities
#'
#' This function takes a dataframe with multi-county communities and returns a
#' community-level dataframe of "exposed" storms, based on the average distance
#' between the storm's track and the population-based centers of each county
#' in the community and the given threshold of rainfall, summed over the days
#' included in the rainfall measurement.
#'
#' @param communities A dataframe with the FIPS codes for all counties within
#'    each community. It must include columns with a column identifier
#'    (\code{commun}) and with the FIPS codes of counties included in each
#'    community (\code{fips}). See the example code.
#' @inheritParams county_rain
#'
#' @return Returns the same type dataframe as \code{county_rain},
#'    but with storms listed by community instead of county.
#'
#' @export
#'
#' @examples
#' communities <- data.frame(commun = c(rep("ny", 6), "no", "new"),
#'                          fips = c("36005", "36047", "36061",
#'                                   "36085", "36081", "36119",
#'                                   "22071", "51700"))
#' rain_storm_df <- multi_county_rain(communities = communities,
#'                                    start_year = 1995, end_year = 2005,
#'                                    rain_limit = 100, dist_limit = 100)
#'
#' @importFrom dplyr %>%
multi_county_rain <- function(communities, start_year, end_year,
                         rain_limit, dist_limit,
                         days_included = c(-1, 0, 1)){

        communities <- dplyr::mutate_(communities, fips = ~ as.character(fips))

        dots <- stats::setNames(list(lazyeval::interp(~ lubridate::ymd(x),
                                                      x = quote(closest_date))),
                                "closest_date")
        rain_storm_df <- hurricaneexposure::closest_dist %>%
                dplyr::mutate_(.dots = dots) %>%
                dplyr::filter_(~ fips %in% communities$fips &
                                      lubridate::year(closest_date) >= start_year &
                                      lubridate::year(closest_date) <= end_year) %>%
                dplyr::left_join(communities, by = "fips") %>%
                dplyr::left_join(hurricaneexposure::rain,
                                 by = c("storm_id", "fips")) %>%
                dplyr::filter_(~ lag %in% days_included) %>%
                dplyr::group_by_(~ storm_id, ~ fips) %>%
                dplyr::summarize_(closest_date = ~ dplyr::first(closest_date),
                                  local_time = ~ dplyr::first(local_time),
                                  closest_time_utc = ~ dplyr::first(closest_time_utc),
                                 storm_dist = ~ dplyr::first(storm_dist),
                                 commun = ~ dplyr::first(commun),
                                 tot_precip = ~ sum(precip)) %>%
                dplyr::ungroup() %>%
                dplyr::group_by_(~ commun, ~ storm_id) %>%
                dplyr::mutate_(max_rain = ~ max(tot_precip),
                              min_dist = ~ min(storm_dist)) %>%
                dplyr::filter_(~ max_rain >= rain_limit &
                                      min_dist <= dist_limit) %>%
                dplyr::summarize_(closest_date = ~ dplyr::first(closest_date),
                                  local_time = ~ dplyr::first(local_time),
                                  closest_time_utc = ~ dplyr::first(closest_time_utc),
                                 mean_dist = ~ mean(storm_dist),
                                 mean_rain = ~ mean(tot_precip),
                                 max_rain = ~ dplyr::first(max_rain),
                                 min_dist = ~ dplyr::first(min_dist))
        return(rain_storm_df)

}

#' Write storm rain exposure files
#'
#' This function takes an input of locations (either a vector of county FIPS
#' or a dataframe of multi-county FIPS, with all FIPS listed for each county)
#' and creates time series dataframes that can be merged with health time series,
#' giving the dates and exposures for all storms meeting the given rainfall and
#' storm distance criteria.
#'
#' @param locations Either a vector of FIPS county codes, for county-level
#'    output, or a dataframe with columns for community identifier (\code{commun})
#'    and associated FIPS codes (\code{fips}), for multi-county community output
#' @param out_dir Character string giving the pathname of the directory in which
#'    to write output. This directory should already exist on your computer.
#' @param out_type Character string giving the type of output files you'd like.
#'    Options are \code{"csv"} (default) and \code{"rds"}.
#' @inheritParams county_rain
#'
#' @return Writes out a directory with rain exposure files for each county or
#'    community indicated. For more on the columns in this output, see the
#'    documentation for \code{\link{county_rain}} and
#'    \code{\link{multi_county_rain}}.
#'
#' @examples
#' \dontrun{
#' #' # By county
#' rain_exposure(locations = c("22071", "51700"),
#'               start_year = 1995, end_year = 2005,
#'               rain_limit = 100, dist_limit = 100,
#'               out_dir = "~/tmp/storms")
#'
#' # For multi-county communities
#' communities <- data.frame(commun = c(rep("ny", 6), "no", "new"),
#'                           fips = c("36005", "36047", "36061",
#'                           "36085", "36081", "36119",
#'                           "22071", "51700"))
#' rain_exposure(locations = communities,
#'               start_year = 1995, end_year = 2005,
#'               rain_limit = 100, dist_limit = 100,
#'               out_dir = "~/tmp/storms")
#' }
#'
#' @importFrom dplyr %>%
#'
#' @export
rain_exposure <- function(locations, start_year, end_year,
                          rain_limit, dist_limit,
                          days_included = c(-1, 0, 1),
                          out_dir, out_type = "csv"){

        if(!dir.exists(out_dir)){
                dir.create(out_dir)
        }

        if("commun" %in% colnames(locations)){
                df <- multi_county_rain(communities = locations,
                                  start_year = start_year,
                                  end_year = end_year,
                                  rain_limit = rain_limit,
                                  dist_limit = dist_limit,
                                  days_included = days_included) %>%
                        dplyr::rename_(loc = ~ commun)
        } else {
                df <- county_rain(counties = locations,
                                  start_year = start_year,
                                  end_year = end_year,
                                  rain_limit = rain_limit,
                                  dist_limit = dist_limit,
                                  days_included = days_included) %>%
                        dplyr::rename_(loc = ~ fips)
        }
        locs <- as.character(unique(df$loc))

        for(i in 1:length(locs)){
                out_df <- df %>%
                        dplyr::filter_(~ loc == locs[i]) %>%
                        dplyr::mutate_(date = ~ substring(closest_date, 1, 8)) %>%
                        dplyr::select_('-closest_date', '-loc')
                out_file <- paste0(out_dir, "/", locs[i], ".", out_type)
                if(out_type == "rds"){
                        saveRDS(out_df, file = out_file)
                } else if (out_type == "csv"){
                        utils::write.csv(out_df, file = out_file, row.names = FALSE)
                }

        }
}
