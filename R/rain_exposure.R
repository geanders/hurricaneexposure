#' Hurricane exposure by rain for counties
#'
#' This function takes a list of US counties, based on their 5-digit Federal
#' Information Processing Standard (FIPS) codes, boundaries on
#' the range of years to be considered, thresholds for distance between each
#' county and the storm track, as well as minimum rainfall over a certain time
#' window for the county to be considered "exposed" to the
#' storm. Based on these inputs, the function returns a dataframe with the
#' subset of Atlantic basin storms meeting those criteria for each of the listed
#' counties.
#'
#' @param rain_limit Minimum of rainfall, in millimeters, summed across the days
#'    selected to be included (\code{days_included}), that must fall in a
#'    county for the county to be classified as "exposed" to the storm.
#' @param days_included A numeric vector listing the days to include when
#'    calculating total precipitation. Negative numbers are days before the
#'    closest date of the storm to a county. For example,
#'    \code{c(-1, 0, 1)} would calculate rain for a county as the sum of the
#'    rainfall for the day before, the day of, and the day after the date when
#'    the storm center was closest to the county center. Values can range from
#'    -5 to 3 (i.e., at most, you can calculate the total rainfall from five days
#'    to three days after the day when the storm is closest to the
#'    county).
#' @inheritParams county_distance
#'
#' @return Returns a dataframe with a row for each county-storm pair and with
#'    columns for:
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
#'      \item{\code{tot_precip}: }{Cumulative rainfall, in millimeters, in the
#'                                 county for the days selected using the
#'                                 \code{days_included} option.}
#'      \item{\code{local_time}: }{Local time of the closest approach of the storm to the
#'                        county's population mean center, based on storm tracks
#'                        linearly interpolated to 15-minute increments.}
#'     \item{\code{closest_time_utc}: }{Time, in UTC, of the closest approach of the
#'                             storm to the county's population mean center,
#'                             based on storm tracks linearly interpolated to
#'                             15-minute increments.}
#'    }
#'
#' @references
#'
#' Al-Hamdan MZ, Crosson WL, Economou SA, Estes MG, Estes SM, Hemmings SN,
#' Kent ST, Puckette M, Quattrochi DA, Rickman DL, Wade GM, McClure LA, 2014.
#' Environmental public health applications using remotely sensed data.
#' Geocarto International 29(1):85-98.
#'
#' North America Land Data Assimilation System (NLDAS) Daily Precipitation
#' years 1979-2011 on CDC WONDER Online Database, released 2012.
#' \url{http://wonder.cdc.gov/wonder/help/Precipitation.html}
#'
#' Rui H, Mocko D, 2014. README Document for North America Land Data
#' Assimilation System Phase 2 (NLDAS-2) Products. Goddard Earth Sciences
#' Data and Information Services Center.
#'
#' @examples
#' # Ensure that data package is available before running the example.
#' #  If it is not, see the `hurricaneexposure` package vignette for details
#' # on installing the required data package.
#' if (requireNamespace("hurricaneexposuredata", quietly = TRUE)) {
#'
#' county_rain(counties = c("22071", "51700"),
#'             start_year = 1995, end_year = 2005,
#'             rain_limit = 100, dist_limit = 100)
#' }
#' @importFrom dplyr %>%
#'
#' @export
county_rain <- function(counties, start_year, end_year,
                           rain_limit, dist_limit,
                        days_included = c(-2, -1, 0, 1)){

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
#' This function takes a dataframe with multi-county communities (see example for
#' the proper format) and returns a community-level dataframe of storms to which
#' the community was exposed, based on the average distance
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
#' @return Returns a dataframe with a row for each county-storm pair and with
#'    columns for:
#'    \itemize{
#'      \item{\code{commun}: }{Each community's unique id}
#'      \item{\code{storm_id}: }{Unique storm identifier with the storm name and year,
#'                  separated by a hyphen(e.g., "Alberto-1988",
#'                  "Katrina-2005")}
#'      \item{\code{closest_date}: }{Date (based on local time) of the closest
#'                          approach of the storm to the county's population
#'                          mean center.}
#'      \item{\code{local_time}: }{Local time of the closest approach of the storm to the
#'                        county's population mean center, based on storm tracks
#'                        linearly interpolated to 15-minute increments.}
#'     \item{\code{closest_time_utc}: }{Time, in UTC, of the closest approach of the
#'                             storm to the county's population mean center,
#'                             based on storm tracks linearly interpolated to
#'                             15-minute increments.}
#'      \item{\code{mean_dist}: }{Average of the minimum distance (in kilometers)
#'                                between the storm's track and the population
#'                                mean centers of all the counties in the
#'                                community.}
#'      \item{\code{mean_rain}: }{Average of cumulative rainfall, in millimeters,
#'                                in the counties in the community for the days
#'                                selected using the \code{days_included} option.}
#'     \item{\code{min_dist}: }{The smallest minimum distance (in kilometers)
#'                                between the storm's track and the population
#'                                mean centers of any of the counties in the
#'                                community.}
#'     \item{\code{max_rain}: }{The maximum cumulative rainfall, in millimeters,
#'                                in any of the counties in the community for
#'                                the days selected using the \code{days_included}
#'                                option.}
#'    }
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
#' rain_storm_df <- multi_county_rain(communities = communities,
#'                                    start_year = 1995, end_year = 2005,
#'                                    rain_limit = 100, dist_limit = 100)
#' }
#' @references
#'
#' Al-Hamdan MZ, Crosson WL, Economou SA, Estes MG, Estes SM, Hemmings SN,
#' Kent ST, Puckette M, Quattrochi DA, Rickman DL, Wade GM, McClure LA, 2014.
#' Environmental public health applications using remotely sensed data.
#' Geocarto International 29(1):85-98.
#'
#' North America Land Data Assimilation System (NLDAS) Daily Precipitation
#' years 1979-2011 on CDC WONDER Online Database, released 2012.
#' \url{http://wonder.cdc.gov/wonder/help/Precipitation.html}
#'
#' Rui H, Mocko D, 2014. README Document for North America Land Data
#' Assimilation System Phase 2 (NLDAS-2) Products. Goddard Earth Sciences
#' Data and Information Services Center.
#'
#' @importFrom dplyr %>%
#'
#' @export
multi_county_rain <- function(communities, start_year, end_year,
                         rain_limit, dist_limit,
                         days_included = c(-2, -1, 0, 1)){

        hasData()

        communities <- dplyr::mutate_(communities, fips = ~ as.character(fips))

        dots <- stats::setNames(list(lazyeval::interp(~ lubridate::ymd(x),
                                                      x = quote(closest_date))),
                                "closest_date")
        rain_storm_df <- hurricaneexposuredata::closest_dist %>%
                dplyr::mutate_(.dots = dots) %>%
                dplyr::filter_(~ fips %in% communities$fips &
                                      lubridate::year(closest_date) >= start_year &
                                      lubridate::year(closest_date) <= end_year) %>%
                dplyr::left_join(communities, by = "fips") %>%
                dplyr::left_join(hurricaneexposuredata::rain,
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
#' or a dataframe of multi-county FIPS, with all FIPS listed for each county;
#' see examples) and creates time series dataframes with the dates and exposures
#' for all storms meeting the given rainfall and storm distance criteria. These
#' exposure time series can then be merged with other time series (e.g.,
#' community-specific daily counts of health outcomes).
#'
#' @param locations Either a vector of FIPS county codes, for county-level
#'    output, or a dataframe with columns for community identifier (\code{commun})
#'    and associated FIPS codes (\code{fips}), for multi-county community output.
#'    See the examples for the proper format for this argument.
#' @param out_dir Character string giving the pathname of the directory in which
#'    to write output. This directory should already exist on your computer.
#' @param out_type Character string giving the type of output files you'd like.
#'    Options are \code{"csv"} (default) and \code{"rds"}.
#' @inheritParams county_rain
#'
#' @return This function writes out rain exposure files for each county or
#'    community indicated to the specified output directory (\code{out_dir}).
#'    For more details on the columns in the output files, see the
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
#' }
#' @importFrom dplyr %>%
#'
#' @export
rain_exposure <- function(locations, start_year, end_year,
                          rain_limit, dist_limit,
                          days_included = c(-2, -1, 0, 1),
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
                        dplyr::rename_(loc = ~ commun) %>%
                        dplyr::ungroup()
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
                        dplyr::filter_(~ loc == locs[i])
                out_file <- paste0(out_dir, "/", locs[i], ".", out_type)
                if(out_type == "rds"){
                        saveRDS(out_df, file = out_file)
                } else if (out_type == "csv"){
                        utils::write.csv(out_df, file = out_file, row.names = FALSE)
                }

        }
}
