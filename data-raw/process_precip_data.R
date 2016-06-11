data("closest_dist")
library(dplyr)
library(tidyr)
library(lubridate)
library(data.table)

check_dates <- select(closest_dist, -storm_dist) %>%
        mutate(closest_date = ymd(closest_date)) %>%
        rename(day_0 = closest_date) %>%
        mutate(fips = as.integer(fips),
               day_0 = day_0 + days(0),
               day_b1 = day_0 - days(1),
               day_b2 = day_0 - days(2),
               day_b3 = day_0 - days(3),
               day_a1 = day_0 + days(1),
               day_a2 = day_0 + days(2),
               day_a3 = day_0 + days(3)) %>%
        select(storm_id, fips, day_b3, day_b2, day_b1,
               day_0, day_a1, day_a2, day_a3) %>%
        gather(key = lag, value = day, -storm_id, -fips) %>%
        mutate(day = as.numeric(format(day, "%Y%m%d")))

all_dates <- unique(check_dates$day)
all_fips <- unique(check_dates$fips) # has Miami as "12086", and is still in check_dates here
all_fips <- c(all_fips, as.integer(12025))
check_dates[check_dates$fips == 12086, "fips"] <- 12025

## Read and process precipitation data
rain <- fread("data-raw/nasa_precip_export_2.txt",
                      # nrows = 500000,
                      header = TRUE,
                      select = c("county", "year_month_day", "precip", "precip_max")) %>%
        filter(county %in% all_fips,
               year_month_day %in% all_dates) %>%
        rename(fips = county, day = year_month_day) %>%
        right_join(data.table(check_dates),
                   by = c("fips" = "fips", "day" = "day")) %>%
        filter(!is.na(precip) & !is.na(precip_max)) %>%
        select(-day) %>%
        arrange(storm_id, fips) %>%
        select(fips, storm_id, lag, precip, precip_max) %>%
        mutate(fips = sprintf("%05d", fips),
               lag = gsub("day_", "", lag),
               lag = gsub("b", "-", lag),
               lag = gsub("a", "", lag),
               lag = as.numeric(lag))
rain[rain$fips == 12025, "fips"] <- 12086
use_data(rain, overwrite = TRUE)
