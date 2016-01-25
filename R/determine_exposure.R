# counties <- c("12011", "12025", "12033", "12097")
# start_year <- 1995
# end_year <- 2005
# rain_limit <- 20
# dist_limit <- 100
# out_dir <- "~/tmp/hurr_rain"

rain_exposure <- function(counties, start_year, end_year,
                          rain_limit, dist_limit,
                          out_dir){
        df <- dplyr::filter(closest_dist,
                            fips %in% counties &
                              year(closest_date) >= start_year &
                              year(closest_date) <= end_year &
                              storm_dist <= dist_limit) %>%
                dplyr::left_join(storm_rains,
                                 by = c("storm_id", "fips")) %>%
                dplyr::filter(tot_precip >= rain_limit)

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
