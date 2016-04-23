load("data/closest_dist.rda")
library(dplyr)
library(tidyr)
library(lubridate)
library(data.table)

check_dates <- select(closest_dist, -storm_dist) %>%
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
all_fips <- unique(check_dates$fips)

## Read and process precipitation data
precip_file <- fread("data-raw/nasa_precip_export_2.txt",
                          # nrows = 500000,
                     header = TRUE,
                     select = c("county", "year_month_day", "precip")) %>%
        filter(county %in% all_fips,
               year_month_day %in% all_dates) %>%
        rename(fips = county, day = year_month_day) %>%
        right_join(data.table(check_dates),
                  by = c("fips" = "fips", "day" = "day")) %>%
        filter(!is.na(precip)) %>%
        select(-day) %>%
        spread(key = lag, value = precip) %>%
        arrange(storm_id, fips) %>%
        select(fips, storm_id, day_b3, day_b2, day_b1, day_0,
               day_a1, day_a2, day_a3) %>%
        mutate(fips = sprintf("%05d", fips))
save(precip_file, file = "data/precip_file.rda")
