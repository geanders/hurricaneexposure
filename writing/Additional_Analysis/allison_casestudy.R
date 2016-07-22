#Create data frame for Hurricane Allison (2001) with 2 weeks of precipitation data
library(hurricaneexposuredata)
data("closest_dist")
library(dplyr)
library(tidyr)
library(lubridate)
library(data.table)
library(ggplot2)
library(choroplethrMaps)
check_dates <- dplyr::select(closest_dist, -storm_dist) %>%
        dplyr::mutate(closest_date = ymd(closest_date)) %>%
        dplyr::rename(day_0 = closest_date) %>%
        dplyr::mutate(fips = as.integer(fips),
                      day_0 = day_0 + days(0),
                      day_b1 = day_0 - days(1),
                      day_b2 = day_0 - days(2),
                      day_b3 = day_0 - days(3),
                      day_b4 = day_0 - days(4),
                      day_b5 = day_0 - days(5),
                      day_b6 = day_0 - days(6),
                      day_b7 = day_0 - days(7),
                      day_b8 = day_0 - days(8),
                      day_b9 = day_0 - days(9),
                      day_b10 = day_0 - days(10),
                      day_a1 = day_0 + days(1),
                      day_a2 = day_0 + days(2),
                      day_a3 = day_0 + days(3),
                      day_a4 = day_0 + days(4),
                      day_a5 = day_0 + days(5),
                      day_a6 = day_0 + days(6),
                      day_a7 = day_0 + days(7),
                      day_a8 = day_0 + days(8),
                      day_a9 = day_0 + days(9),
                      day_a10 = day_0 + days(10)) %>%
        dplyr::select(storm_id, fips, day_b10, day_b9, day_b8, day_b7, day_b6, day_b5, day_b4, day_b3, day_b2, day_b1,
                      day_0, day_a1, day_a2, day_a3, day_a4, day_a6, day_a7, day_a8, day_a9, day_a10) %>%
        tidyr::gather(key = lag, value = day, -storm_id, -fips) %>%
        dplyr::mutate(day = as.numeric(format(day, "%Y%m%d")))

all_dates <- unique(check_dates$day)
all_fips <- unique(check_dates$fips) # has Miami as "12086", and is still in check_dates here
all_fips <- c(all_fips, as.integer(12025))
check_dates[check_dates$fips == 12086, "fips"] <- 12025

allison <- data.table::fread(
        #"/Users/brookeanderson/Documents/CSU2016/hurricaneproject/hurricaneexposuredata/data-raw/nasa_precip_export_2.txt",
        "data-raw/nasa_precip_export_2.txt",
                             header = TRUE,
                             select = c("county", "year_month_day", "precip", "precip_max")) %>%
        dplyr::filter(county %in% all_fips,
                      year_month_day %in% all_dates) %>%
        dplyr::rename(fips = county, day = year_month_day) %>%
        dplyr::right_join(data.table(check_dates),
                          by = c("fips" = "fips", "day" = "day")) %>%
        dplyr::filter(!is.na(precip) & !is.na(precip_max)) %>%
        dplyr::select(-day) %>%
        dplyr::arrange(storm_id, fips) %>%
        dplyr::select(fips, storm_id, lag, precip, precip_max) %>%
        dplyr::mutate(fips = sprintf("%05d", fips),
                      lag = gsub("day_", "", lag),
                      lag = gsub("b", "-", lag),
                      lag = gsub("a", "", lag),
                      lag = as.numeric(lag)) %>%
        dplyr::filter(storm_id == "Allison-2001")
allison[allison$fips == 12025, "fips"] <- 12086

#Create function to plot out total precipitation for Allison using different lag days (out to -10 and 10)

filter_precip_data <- function(df = df, counties = NULL,
                              distance_limit = NULL, rain_limit = NULL, include_rain = TRUE,
                              days_included = NULL,
                              output_vars = c("fips", "tot_precip")){

        closest_dist <- data.table::data.table(hurricaneexposuredata::closest_dist)

        if(!is.null(counties)){
                closest_dist <- closest_dist[get("fips") %in% counties]
        }

        if(!is.null(distance_limit)){
                closest_dist <- closest_dist[get("storm_dist") <=
                                                     distance_limit]
        }

        if(include_rain == TRUE){
                rain <- data.table::data.table(df)
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

storm_rain <-function(df, days_included = c(-2, -1, 0, 1)){
                map_data <- filter_precip_data(df = df,
                                               rain_limit = 0,
                                               days_included = days_included,
                                               output_vars = c("fips",
                                                               "tot_precip")) %>%
                        #filter_storm_data(storm = storm, include_rain = TRUE,
                                              #days_included = days_included,
                                              #output_vars = c("fips",
                                                              #"tot_precip")) %>%
                        dplyr::rename_(region = ~ fips, value = ~ tot_precip)
        map_data <- map_data %>%
                dplyr::mutate_(region = ~ as.numeric(region)) %>%
                dplyr::tbl_df()
        out <- hurricaneexposure:::hurr_choroplethr(map_data, metric = "rainfall")
        return(out$render())
}

#Create plots of Allison Precipitation

setwd("writing/Additional_Analysis/")
pdf("allison_plot.pdf")
my_lags <- c(-10, -9, -8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
for(lags in my_lags){
        a <- storm_rain(allison, days_included = lags) +
                ggtitle(paste0("Precipitation for Hurricane Allison\n at lag ", lags))
        a <- map_tracks(storm = "Allison-2001",
                        plot_object = a, plot_points = FALSE)
        print(a)
}
dev.off()

