#' Filter hurricane datasets
#'
#' This function is a helper function for many of the the other functions in
#' this package that measure exposure.
#'
#' @param counties A character vector listing all 5-digit county FIPS codes
#'    for a subset of counties.
#' @param storm A character string giving the storm ID (e.g., "Floyd-1999")
#' @param year_range A numeric vector of length two with the starting and
#'    ending year to subset to.
#' @param distance_limit A numeric vector of length one giving the maxmimum
#'    distance (in kilometers) to use in the filter
#' @param rain_limit A numeric vector of length one giving the minimum
#'    rain (in millimeters) to use in the filter
#' @param include_rain A logical specifying whether to pull in rain data to use
#'    in the filter or give as output (default is FALSE)
#' @param days_included A numeric vector giving the lag numbers for days to
#'    include when calculating the total rain over the storm period (e.g.,
#'    \code{c(-1, 0, 1, 2)} would calculate the rain from the day before the
#'    storm until two days after the storm). Values in this vector cannot be
#'    lower than -3 or higher than 3.
#' @param output_vars A character vector listing all the columns to include
#'    in the output.
#'
#' @return A dataframe with storms filtered based on the input criteria to the
#'    function. Columns in the output will vary depending on the user's
#'    selections for the \code{output_vars} argument.
#'
#' @examples
#' # Ensure that data package is available before running the example.
#' #  If it is not, see the `hurricaneexposure` package vignette for details
#' # on installing the required data package.
#' if (requireNamespace("hurricaneexposuredata", quietly = TRUE)) {
#'
#' filter_storm_data(counties = c("22071", "51700"), year_range = c(1988, 2011),
#'                   distance_limit = 250, rain_limit = 150,
#'                  include_rain = TRUE, days_included = c(-1, 0, 1),
#'                  output_vars = c("fips", "storm_id", "closest_date",
#'                                  "storm_dist", "tot_precip"))
#' filter_storm_data(storm = "Floyd-1999", include_rain = TRUE,
#'                  days_included = c(-1, 0, 1),
#'                  output_vars = c("fips", "tot_precip"))
#' }
#' @import data.table
#'
#' @export
filter_storm_data <- function(counties = NULL, storm = NULL, year_range = NULL,
                              distance_limit = NULL, rain_limit = NULL,
                              include_rain = FALSE, days_included = NULL,
                              output_vars = c("fips")){

        hasData()

        closest_dist <- data.table::data.table(hurricaneexposuredata::closest_dist)

        if(!is.null(counties)){
                closest_dist <- closest_dist[get("fips") %in% counties]
        }

        if(!is.null(storm)){
                closest_dist <- closest_dist[get("storm_id") == storm]
        }

        if(!is.null(year_range)){
                closest_dist <- closest_dist[ , .(storm_id,
                                                  fips,
                                                  closest_date, storm_dist,
                                                  local_time,
                                                  closest_time_utc,
                                                year = substring(get("closest_date"),
                                                                 1, 4)), ][
                                         get("year") %in%
                                                 year_range[1]:year_range[2]
                                                                 ]
        }

        if(!is.null(distance_limit)){
                closest_dist <- closest_dist[get("storm_dist") <=
                                                     distance_limit]
        }

        if(include_rain){
                rain <- data.table::data.table(hurricaneexposuredata::rain)
                rain <- rain[get("lag") %in% days_included]
                rain <- rain[ , .(tot_precip = sum(get("precip"))),
                              by = .(fips, storm_id)]
                closest_dist <- merge(closest_dist, rain, all.x = TRUE,
                                      by = c("storm_id", "fips"))
                if(!is.null(rain_limit)){
                        closest_dist <- closest_dist[get("tot_precip") >=
                                                             rain_limit]
                }
        }

        closest_dist <- closest_dist[ , .SD, , .SDcols = output_vars]
        return(closest_dist)
}

#' Filter hurricane wind dataset
#'
#' This function is a helper function for many of the the other functions in
#' this package that measure wind exposure.
#'
#' @inheritParams filter_storm_data
#' @param wind_limit A numeric vector of length one giving the minimum
#'    wind speed (in meters per second) to use in the filter
#' @param wind_var A character string giving the wind variable to use. Choices
#'    are "vmax_sust" (maximum sustained winds; default) or "vmax_gust" (maximum
#'    gust winds).
#'
#' @return A dataframe with storms filtered based on the input criteria to the
#'    function. Columns in the output will vary depending on the user's
#'    selections for the \code{output_vars} argument.
#'
#' @examples
#' # Ensure that data package is available before running the example.
#' #  If it is not, see the `hurricaneexposure` package vignette for details
#' # on installing the required data package.
#' if (requireNamespace("hurricaneexposuredata", quietly = TRUE)) {
#'
#' filter_wind_data(counties = c("22071", "51700"), year_range = c(1988, 2011),
#'                  wind_limit = 20,
#'                  output_vars = c("fips", "storm_id", "vmax_sust"))
#' }
#' @import data.table
#'
#' @export
filter_wind_data <- function(counties = NULL, storm = NULL, year_range = NULL,
                             wind_limit = NULL, output_vars = "fips",
                             wind_var = "vmax_sust"){

        hasData()

        storm_winds <- data.table::data.table(hurricaneexposuredata::storm_winds)

        if(!is.null(counties)){
                storm_winds <- storm_winds[get("fips") %in% counties]
        }

        if(!is.null(storm)){
                storm_winds <- storm_winds[get("storm_id") == storm]
        }

        if(!is.null(year_range)){
                storm_winds <- storm_winds[ , .(storm_id,
                                                fips,
                                                vmax_gust = get("vmax_gust"),
                                                vmax_sust = get("vmax_sust"),
                                                year = gsub("*.+-", "", get("storm_id"))), ][
                                                        get("year") %in%
                                                                year_range[1]:year_range[2]
                                                        ]
        }

        if(!is.null(wind_limit)){
                if(wind_var == "vmax_sust"){
                        storm_winds <- storm_winds[get("vmax_sust") >=
                                                           wind_limit]
                } else if (wind_var == "vmax_gust"){
                        storm_winds <- storm_winds[get("vmax_gust") >=
                                                           wind_limit]
                }
        }

        storm_winds <- storm_winds[ , .SD, , .SDcols = output_vars]
        return(storm_winds)
}

