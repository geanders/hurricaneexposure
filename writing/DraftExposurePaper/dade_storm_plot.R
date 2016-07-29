options("noaakey" = Sys.getenv("noaakey"))
library(countyweather)
library(tidyr)
library(dplyr)
library(hurricaneexposure)
library(hurricaneexposuredata)
library(lubridate)
fips <- "12086"
county_timeseries(fips, percent_coverage = 0,
                  date_min = "1988-01-01", date_max = "2011-12-31",
                  var = "PRCP", #out_directory = "~/frances_ex/"
                  out_directory = "~/Documents/hurricaneexposure/writing/DraftExposurePaper/dade_data/")
frances_dir <- "~/Documents/hurricaneexposure/writing/DraftExposurePaper/dade_data/"
county_weather <- readRDS(paste0(frances_dir, fips, ".rds"))
county_weather <- county_weather %>%
        filter(date %in% seq(from = as.Date("1988-01-01"), to = as.Date("2011-12-31"), by = 1))

county_weather$fips <- fips
dade_rain <- county_rain(counties = fips, start_year = 1988, end_year = 2011,
                         rain_limit = 0, dist_limit = 1000) %>%
        filter(fips == "12086")
