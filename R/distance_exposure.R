#' Hurricane exposure by distance for counties
#'
#' This function takes a list of county FIPS, boundaries on
#' starting and ending years, and thresholds for distance
#' and returns a dataframe with the subset of storms meeting those criteria
#' for each of the listed counties.
#'
#' @param counties Character string of the five-digit FIPS codes for
#'    counties for which the user wants to create data
#' @inheritParams county_rain
#'
#' @return Returns a dataframe with a row for each county-storm
#'    pair and with columns giving the storm's ID, county FIPS,
#'    date and time of closest approach of that storm to that county, and
#'    distance between the county center and the storm track on
#'    that date and time.
#'
#' @examples
#' county_distance(counties = c("22071", "51700"),
#'             start_year = 1995, end_year = 2005,
#'             dist_limit = 75)
#'
#' @export
#'
#' @importFrom dplyr %>%
county_distance <- function(counties, start_year, end_year, dist_limit){

        dots <- stats::setNames(list(lazyeval::interp(~ lubridate::ymd_hm(x),
                                                      x = quote(closest_date))),
                                    "closest_date")

        distance_df <- hurricaneexposure::closest_dist %>%
                dplyr::mutate_(.dots = dots) %>%
                dplyr::filter_(~ fips %in% counties &
                                    lubridate::year(closest_date) >= start_year &
                                    lubridate::year(closest_date) <= end_year &
                                    storm_dist <= dist_limit)
        return(distance_df)
}

#' Hurricane exposure by distance for communities
#'
#' This function takes a dataframe with multi-county communities and returns a
#' community-level dataframe of "exposed" storms, based on the average distance
#' between the storm's track and the population-based centers of each county
#' in the community.
#'
#' @inheritParams county_rain
#' @inheritParams multi_county_rain
#'
#' @return Returns the same type dataframe as \code{county_distance},
#'    but with storms listed by community instead of county.
#'
#' @export
#'
#' @examples
#' communities <- data.frame(commun = c(rep("ny", 6), "no", "new"),
#'                          fips = c("36005", "36047", "36061",
#'                                   "36085", "36081", "36119",
#'                                   "22071", "51700"))
#' distance_df <- multi_county_distance(communities = communities,
#'                                      start_year = 1995, end_year = 2005,
#'                                      dist_limit = 75)
#'
#' @importFrom dplyr %>%
multi_county_distance <- function(communities, start_year, end_year,
                         dist_limit){

        communities <- dplyr::mutate_(communities, fips = ~ as.character(fips))

        distance_df <- hurricaneexposure::closest_dist %>%
                dplyr::mutate_(closest_date =
                                       ~ lubridate::ymd_hm(closest_date)) %>%
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
                                 mean_dist = ~ mean(storm_dist),
                                 min_dist = ~ dplyr::first(min_dist))
        return(distance_df)

}

#' Write storm distance exposure files
#'
#' This function takes an input of locations (either a vector of county FIPS
#' or a dataframe of multi-county FIPS, with all FIPS listed for each county)
#' and creates time series dataframes that can be merged with health time series,
#' giving the dates and exposures for all storms meeting the given
#' storm distance criteria.
#'
#' @inheritParams county_rain
#' @inheritParams rain_exposure
#'
#' @return Writes out a directory with rain exposure files for each county or
#'    community indicated. For more on the columns in this output, see the
#'    documentation for \code{\link{county_rain}} and
#'    \code{\link{multi_county_rain}}.
#'
#' @examples
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
#'
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
                        dplyr::rename_(loc = ~ commun)
        } else {
                df <- county_distance(counties = locations,
                                  start_year = start_year,
                                  end_year = end_year,
                                  dist_limit = dist_limit) %>%
                        dplyr::rename_(loc = ~ fips)
        }
        locs <- as.character(unique(df$loc))

        for(i in 1:length(locs)){
                out_df <- dplyr::filter_(df, ~ loc == locs[i]) %>%
                        dplyr::mutate_(date = ~ format(closest_date,
                                                       "%Y%m%d")) %>%
                        dplyr::select_('-closest_date', '-loc')
                out_file <- paste0(out_dir, "/", locs[i], ".", out_type)
                if(out_type == "rds"){
                        saveRDS(out_df, file = out_file)
                } else if (out_type == "csv"){
                        utils::write.csv(out_df, file = out_file,
                                         row.names = FALSE)
                }

        }
}
