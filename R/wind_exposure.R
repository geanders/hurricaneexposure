#' Hurricane exposure by wind for counties
#'
#' This function takes a list of US counties, based on their 5-digit Federal
#' Information Processing Standard (FIPS) codes, boundaries on
#' the range of years to be considered, and thresholds for wind speed (in meters
#' per second) (or, alternatively, duration of winds at or above 20 m / s in minutes)
#' for each  county to be considered "exposed" to the
#' storm. Based on these inputs, the function returns a dataframe with the
#' subset of Atlantic basin storms meeting those criteria for each of the listed
#' counties.
#'
#' @inheritParams county_distance
#' @inheritParams filter_wind_data
#'
#' @return Returns a dataframe with a row for each county-storm
#'    pair and with columns for:
#'    \itemize{
#'      \item{\code{storm_id}: }{Unique storm identifier with the storm name and year,
#'                  separated by a hyphen(e.g., "Alberto-1988",
#'                  "Katrina-2005")}
#'      \item{\code{fips}: }{County's 5-digit Federal Information Processing Standard
#'                  (FIPS) code}
#'      \item{\code{max_sust}: }{Maximum sustained wind speed (in m / s)}
#'      \item{\code{max_gust}: }{Maximum gust wind speed (in m / s)}
#'      \item{\code{sust_dur}: }{Minutes sustained wind speed was 20 m / s or higher}
#'      \item{\code{gust_dur}: }{Minutes gust wind speed was 20 m / s or higher (only
#'          returned if the modeled winds are requested using \code{wind_source})}
#'    }
#'
#' @details For more information on how wind speeds are modeled in this data,
#'    see the documentation for the \code{stormwindmodel} R package.
#' @note Only counties in states in the eastern half of the United States can
#'    be processed by this function. Winds are modeled at 15-minute increments, so
#'    all duration estimates (\code{sust_dur} and \code{gust_dur} in the output)
#'    will be divisible by 15.
#'
#' @examples
#' # Ensure that data package is available before running the example.
#' #  If it is not, see the `hurricaneexposure` package vignette for details
#' # on installing the required data package.
#' if (requireNamespace("hurricaneexposuredata", quietly = TRUE)) {
#'
#' county_wind(counties = c("22071", "51700"),
#'             start_year = 1988, end_year = 2005,
#'             wind_limit = 20, wind_var = "vmax_sust")
#' }
#' @export
#'
#' @importFrom dplyr %>%
county_wind <- function(counties, start_year, end_year, wind_limit,
                        wind_var = "vmax_sust", wind_source = "modeled"){

        hasData()

        output_vars <- c("storm_id", "fips", "vmax_sust", "vmax_gust", "sust_dur")
        if(wind_source == "modeled") {
                output_vars <- c(output_vars, "gust_dur")
        }

        wind_df <- filter_wind_data(counties = counties, wind_var = wind_var,
                                         year_range = c(start_year, end_year),
                                         wind_limit = wind_limit,
                                         wind_source = wind_source,
                                         output_vars = output_vars) %>%
                dplyr::left_join(hurricaneexposuredata::closest_dist,
                                 by = c("storm_id", "fips"))

        return(wind_df)
}

#' Hurricane exposure by wind for communities
#'
#' This function takes a dataframe with multi-county communities and returns a
#' community-level dataframe of "exposed" storms, based on the highest of the
#' maximum sustained wind speed for each county in the community.
#'
#' @inheritParams county_wind
#' @inheritParams county_distance
#' @inheritParams filter_wind_data
#' @inheritParams multi_county_rain
#'
#' @return Returns the same type dataframe as \code{county_rain},
#'    but with storms listed by community instead of county.
#'
#' @note This function currently will only input a threshold for the sustained wind
#'    metric. If you would like to use gust winds or duration of winds, you will need
#'    to use the \code{county_wind} function to pull storms and aggregate to the
#'    multi-county community level yourself.
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
#' wind_df <- multi_county_wind(communities = communities,
#'                                      start_year = 1988, end_year = 2005,
#'                                      wind_limit = 20)
#' }
#' @importFrom dplyr %>%
multi_county_wind <- function(communities, start_year, end_year,
                              wind_limit){

        hasData()

        communities <- dplyr::mutate_(communities, fips = ~ as.character(fips))

        wind_df <- hurricaneexposuredata::storm_winds %>%
                dplyr::mutate_(year = ~ gsub("*.+-", "", storm_id)) %>%
                dplyr::filter_(~ fips %in% communities$fips &
                                       year >= start_year &
                                       year <= end_year) %>%
                dplyr::left_join(communities, by = "fips") %>%
                dplyr::left_join(hurricaneexposuredata::closest_dist,
                                 by = c("storm_id", "fips")) %>%
                dplyr::group_by_(~ commun, ~ storm_id) %>%
                dplyr::mutate_(max_wind = ~ max(vmax_sust),
                               min_dist = ~ min(storm_dist)) %>%
                dplyr::filter_(~ max_wind >= wind_limit) %>%
                dplyr::summarize_(closest_date = ~ dplyr::first(closest_date),
                                  local_time = ~ dplyr::first(local_time),
                                  closest_time_utc = ~ dplyr::first(closest_time_utc),
                                  mean_dist = ~ mean(storm_dist),
                                  mean_wind = ~ mean(vmax_sust),
                                  min_dist = ~ dplyr::first(min_dist),
                                  max_wind = ~ dplyr::first(max_wind))
        return(wind_df)
}

#' Write storm wind exposure files
#'
#' This function takes an input of locations (either a vector of county FIPS
#' or a dataframe of multi-county FIPS, with all FIPS listed for each county)
#' and creates time series dataframes that can be merged with health time series,
#' giving the dates and exposures for all storms meeting the given
#' storm wind criteria.
#'
#' @param wind_limit Vector giving the wind speed (in m / s) to use as a
#'    threshold for classifying a county as "exposed" to a specific storm.
#' @inheritParams county_distance
#' @inheritParams county_rain
#' @inheritParams rain_exposure
#' @inheritParams filter_wind_data
#'
#' @return Writes out a directory with rain exposure files for each county or
#'    community indicated. For more on the columns in this output, see the
#'    documentation for \code{\link{county_wind}} and
#'    \code{\link{multi_county_wind}}.
#'
#' @note This function allows you to use different wind variables (sustained winds, which
#'    is the default; gust winds; duration of sustained winds; duration of gust winds)
#'    when pulling exposures by county. However, if pulling multi-county communities,
#'    currently only the sustained winds metric can be used with this function.
#'
#' @examples \dontrun{
#' # Ensure that data package is available before running the example.
#' #  If it is not, see the `hurricaneexposure` package vignette for details
#' # on installing the required data package.
#' if (requireNamespace("hurricaneexposuredata", quietly = TRUE)) {
#'
#' # By county
#' wind_exposure(locations = c("22071", "51700"),
#'               start_year = 1988, end_year = 2005,
#'               wind_limit = 10,
#'               out_dir = "~/tmp/storms")
#'
#' # For multi-county communities
#' communities <- data.frame(commun = c(rep("ny", 6), "no", "new"),
#'                           fips = c("36005", "36047", "36061",
#'                           "36085", "36081", "36119",
#'                           "22071", "51700"))
#' wind_exposure(locations = communities,
#'               start_year = 1988, end_year = 2005,
#'               wind_limit = 10,
#'               out_dir = "~/tmp/storms")
#'}
#'}
#' @export
#'
#' @importFrom dplyr %>%
wind_exposure <- function(locations, start_year, end_year,
                              wind_limit, wind_var, out_dir, out_type = "csv"){

        if(!dir.exists(out_dir)){
                dir.create(out_dir)
        }

        if("commun" %in% colnames(locations)){
                df <- multi_county_wind(communities = locations,
                                        start_year = start_year,
                                        end_year = end_year,
                                        wind_limit = wind_limit) %>%
                        dplyr::rename_(loc = ~ commun) %>%
                        dplyr::ungroup()
        } else {
                df <- county_wind(counties = locations,
                                  start_year = start_year,
                                  end_year = end_year,
                                  wind_limit = wind_limit) %>%
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

