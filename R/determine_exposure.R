# counties <- c("12011", "12025", "12033", "12097")
# start_year <- 1995
# end_year <- 2005
# rain_limit <- 20
# dist_limit <- 100
# out_dir <- "~/tmp/hurr_rain"

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
exposed_storms <- function(counties, start_year, end_year,
                           rain_limit, dist_limit){
        df <- dplyr::filter(closest_dist,
                            fips %in% counties &
                                    year(closest_date) >= start_year &
                                    year(closest_date) <= end_year &
                                    storm_dist <= dist_limit) %>%
                dplyr::left_join(storm_rains,
                                 by = c("storm_id", "fips")) %>%
                dplyr::filter(tot_precip >= rain_limit)
        return(df)
}

rain_exposure <- function(counties, start_year, end_year,
                          rain_limit, dist_limit,
                          out_dir){

        df <- exposed_storms(counties = counties,
                             start_year = start_year,
                             end_year = end_year,
                             rain_limit = rain_limit,
                             dist_limit = dist_limit)

        start_date <- as.Date(paste0(start_year, "0101"),
                              format = "%Y%m%d")
        end_date <- as.Date(paste0(end_year, "0101"),
                              format = "%Y%m%d")
        for(i in 1:length(df$fips)){
                fips_df <- filter(df, fips == fips[i]) %>%
                        mutate(date = format(closest_date,
                                             "%Y%m%d")) %>%
                        mutate(date = as.Date(date, "%Y%m%d"))

                out_df <- data.frame(date = seq(start_date,
                                                end_date,
                                                by = "days")) %>%
                        dplyr::full_join(fips_df, by = c("date")) %>%
                        dplyr::select(-fips, -closest_date) %>%
                        dplyr::mutate(storm = as.numeric(!is.na(storm_id)))
                out_df$storm_id[is.na(out_df$storm_id)] <- "none"

                out_file <- paste0(out_dir, "/", df$fips[i], ".rds")
                saveRDS(out_df, out_file)
        }
}
