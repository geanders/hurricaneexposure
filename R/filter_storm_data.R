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
#'
#' @examples
#' filter_storm_data(counties = c("22071", "51700"), year_range = c(1988, 2011),
#'                   distance_limit = 250, rain_limit = 150,
#'                  include_rain = TRUE, days_included = c(-1, 0, 1),
#'                  output_vars = c("fips", "storm_id", "closest_date",
#'                                  "storm_dist", "tot_precip"))
#' filter_storm_data(storm = "Floyd-1999", include_rain = TRUE,
#'                  days_included = c(-1, 0, 1),
#'                  output_vars = c("fips", "tot_precip"))
#'
#' @export
filter_storm_data <- function(counties = NULL, storm = NULL, year_range = NULL,
                              distance_limit = NULL, rain_limit = NULL,
                              include_rain = FALSE, days_included = NULL,
                              output_vars = c("fips")){

        closest_dist <- data.table::data.table(hurricaneexposure::closest_dist)

        if(!is.null(counties)){
                closest_dist <- closest_dist[fips %in% counties]
        }

        if(!is.null(storm)){
                closest_dist <- closest_dist[storm_id == storm]
        }

        if(!is.null(year_range)){
                closest_dist <- closest_dist[ , .(storm_id, fips,
                                                  closest_date, storm_dist,
                                                year = substring(closest_date,
                                                                 1, 4)), ][
                                         year %in% year_range[1]:year_range[2]
                                                                 ]
        }

        if(!is.null(distance_limit)){
                closest_dist <- closest_dist[storm_dist <= distance_limit]
        }

        if(include_rain){
                rain <- data.table::data.table(hurricaneexposure::rain)
                rain <- rain[lag %in% days_included]
                rain <- rain[ , .(tot_precip = sum(precip)),
                              by = .(fips, storm_id)]
                closest_dist <- merge(closest_dist, rain, all.x = TRUE,
                                      by = c("storm_id", "fips"))
                if(!is.null(rain_limit)){
                        closest_dist <- closest_dist[tot_precip >= rain_limit]
                }
        }

        closest_dist <- closest_dist[ , .SD, , .SDcols = output_vars]
        return(closest_dist)
}
