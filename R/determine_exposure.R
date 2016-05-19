#' Create subset of storms for FIPS
#'
#' This function takes a list of county FIPS, boundaries on
#'    starting and ending years, and thresholds for distance
#'    and the week-window rainfall and returns a dataframe
#'    with the subset of storms meeting those criteria for
#'    each of the listed counties.
#'
#' @param counties Character string of the FIPS codes for
#'    counties to create data for
#' @param start_year Four-digit integer with first year to consider.
#' @param end_year Four-digit integer with last year to consider.
#' @param rain_limit Minimum of rainfall for the week-long period
#'    centered at the storm's closest date to consider county
#'    "exposed" to the storm.
#' @param days_included A numeric vector listing the days to include when
#'    calculating total precipitation. Negative numbers are days before the
#'    closest date of the storm to a county. For example,
#'    \code{c(-1, 0, 1)} would calculate rain for a county as the sum of the
#'    rainfall for the day before, the day of, and the day after the date when
#'    the storm center was closest to the county center.
#' @param dist_limit Maximum distance for the closest distance
#'    between the county center and the storm track to consider
#'    the county "exposed" to the storm.
#'
#' @return Returns a dataframe with a row for each county-storm
#'    pair and with columns giving the storm's ID, county FIPS,
#'    date and time of closest approach of that storm to that county,
#'    distance between the county center and the storm track on
#'    that date and time, and cumulative rainfall for the week-long
#'    period centered at the date of closest approach to the county.
#'
#' @examples
#' county_rain(counties = c("22071", "51700"),
#'             start_year = 1995, end_year = 2005,
#'             rain_limit = 100, dist_limit = 100)
#'
#' @export
#'
#' @importFrom dplyr %>%
county_rain <- function(counties, start_year, end_year,
                           rain_limit, dist_limit,
                        days_included = c(-1, 0, 1)){

        all_days <- c("b3", "b2", "b1", "0", "a1", "a2", "a3")
        days_included <- all_days[(days_included + 4)]
        days_included <- paste("day", days_included, sep = "_")

        rain_storm_df <- dplyr::filter(closest_dist,
                            fips %in% counties &
                                    lubridate::year(closest_date) >= start_year &
                                    lubridate::year(closest_date) <= end_year &
                                    storm_dist <= dist_limit) %>%
                dplyr::left_join(precip_file,
                                 by = c("storm_id", "fips")) %>%
                tidyr::gather(key, value, -storm_id, -fips,
                              -closest_date, -storm_dist) %>%
                dplyr::filter(key %in% days_included) %>%
                dplyr::group_by(storm_id, fips) %>%
                dplyr::summarize(closest_date = first(closest_date),
                                 storm_dist = first(storm_dist),
                                 tot_precip = sum(value)) %>%
                dplyr::ungroup() %>%
                dplyr::filter(tot_precip >= rain_limit)
        return(rain_storm_df)
}

#' Create multi-county community exposures
#'
#' This function takes a rain-exposure dataframe for counties,
#'    as created by the \code{rain_storms} function, as well as
#'    a list object with the counties in each multi-county
#'    community, and returns a community-level dataframe of
#'    "exposed" storms, based on the rainfall and distance
#'    thresholds used to create the rain-exposure dataframe.
#'
#' @param communities A dataframe with the FIPS codes for all counties within
#'    each community
#' @inheritParams county_rain
#'
#' @return Returns the same type dataframe as \code{rain_storms},
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

        communities <- dplyr::mutate(communities, fips = as.character(fips))

        all_days <- c("b3", "b2", "b1", "0", "a1", "a2", "a3")
        days_included <- all_days[(days_included + 4)]
        days_included <- paste("day", days_included, sep = "_")

        rain_storm_df <- dplyr::filter(closest_dist,
                                       fips %in% communities$fips &
                                       lubridate::year(closest_date) >= start_year &
                                       lubridate::year(closest_date) <= end_year) %>%
                dplyr::left_join(communities, by = "fips") %>%
                dplyr::left_join(precip_file,
                                 by = c("storm_id", "fips")) %>%
                tidyr::gather(key, value, -storm_id, -fips,
                              -closest_date, -storm_dist, -commun) %>%
                dplyr::filter(key %in% days_included) %>%
                dplyr::group_by(storm_id, fips) %>%
                dplyr::summarize(closest_date = first(closest_date),
                                 storm_dist = first(storm_dist),
                                 commun = first(commun),
                                 tot_precip = sum(value)) %>%
                dplyr::ungroup() %>%
                dplyr::group_by(commun, storm_id) %>%
                dplyr::mutate(max_rain = max(tot_precip),
                              min_dist = min(storm_dist)) %>%
                dplyr::filter(max_rain >= rain_limit &
                                      min_dist <= dist_limit) %>%
                dplyr::summarize(closest_date = first(closest_date),
                                 mean_dist = mean(storm_dist),
                                 mean_rain = mean(tot_precip),
                                 max_rain = first(max_rain),
                                 min_dist = first(min_dist))
        return(rain_storm_df)

}

#' Create storm exposure time series files
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
#' @inheritParams county_rain
#'
#' @examples
#' # By county
#' rain_exposure(locations = c("22071", "51700"),
#'               start_year = 1995, end_year = 2005,
#'               rain_limit = 100, dist_limit = 100, out_dir = "~/tmp/storms")
#'
#' # For multi-county communities
#' communities <- data.frame(commun = c(rep("ny", 6), "no", "new"),
#'                           fips = c("36005", "36047", "36061",
#'                           "36085", "36081", "36119",
#'                           "22071", "51700"))
#' rain_exposure(locations = communities,
#'               start_year = 1995, end_year = 2005,
#'               rain_limit = 100, dist_limit = 100, out_dir = "~/tmp/storms")
#'
#' @export
#'
#' @importFrom dplyr %>%
rain_exposure <- function(locations, start_year, end_year,
                          rain_limit, dist_limit,
                          days_included = c(-1, 0, 1),
                          out_dir){

        if("commun" %in% colnames(locations)){
                df <- multi_county_rain(communities = locations,
                                  start_year = start_year,
                                  end_year = end_year,
                                  rain_limit = rain_limit,
                                  dist_limit = dist_limit,
                                  days_included = c(-1, 0, 1)) %>%
                        dplyr::rename(loc = commun)
        } else {
                df <- county_rain(counties = locations,
                                  start_year = start_year,
                                  end_year = end_year,
                                  rain_limit = rain_limit,
                                  dist_limit = dist_limit,
                                  days_included = c(-1, 0, 1)) %>%
                        dplyr::rename(loc = fips)
        }
        locs <- as.character(unique(df$loc))

        start_date <- as.Date(paste0(start_year, "0101"),
                              format = "%Y%m%d")
        end_date <- as.Date(paste0(end_year, "1231"),
                              format = "%Y%m%d")
        for(i in 1:length(locs)){
                locs_df <- dplyr::filter(df, loc == locs[i]) %>%
                        dplyr::mutate(date = format(closest_date,
                                             "%Y%m%d")) %>%
                        dplyr::mutate(date = as.Date(date, "%Y%m%d"))

                out_df <- data.frame(date = seq(start_date,
                                                end_date,
                                                by = "days")) %>%
                        dplyr::full_join(locs_df, by = c("date")) %>%
                        dplyr::select(-loc, -closest_date) %>%
                        dplyr::mutate(storm = as.numeric(!is.na(storm_id)))
                out_df$storm_id[is.na(out_df$storm_id)] <- "none"

                out_file <- paste0(out_dir, "/", locs[i], ".rds")
                saveRDS(out_df, out_file)
        }
}
