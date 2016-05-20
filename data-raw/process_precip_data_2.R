load("data/closest_distance.Rdata")
library(dplyr)
library(tidyr)
library(lubridate)
library(data.table)

check_dates <- select(closest_distance, -storm_dist) %>%
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
all_fips <- unique(check_dates$fips) #has Miami as "12086", and is still in check_dates here
all_fips <- c(all_fips, as.integer(12025))
check_dates[check_dates$fips == 12086, "fips"] <- 12025
## Read and process precipitation data
precipitation_file <- fread("data-raw/nasa_precip_export_2.txt",
                      #nrows = 500000,
                      header = TRUE,
                      select = c("county", "year_month_day", "precip")) %>%
        filter(county %in% all_fips,
               year_month_day %in% all_dates) %>%
        rename(fips = county, day = year_month_day) %>%
        right_join(data.table(check_dates),
                   by = c("fips" = "fips", "day" = "day")) %>%
        filter(!is.na(precip)) %>%
        select(-day) %>%
        #spread(key = lag, value = precip) %>%
        arrange(storm_id, fips) %>%
        select(fips, storm_id, lag, precip) %>%
        mutate(fips = sprintf("%05d", fips))
precipitation_file[precipitation_file$fips == 12025, "fips"] <- 12086
save(precipitation_file, file = "data/precipitation_file.rda")
