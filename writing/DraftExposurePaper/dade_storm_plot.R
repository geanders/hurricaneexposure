fips <- "12086"
county_timeseries(fips, percent_coverage = 0,
                  date_min = "1988-01-01", date_max = "2011-12-31",
                  var = "PRCP", #out_directory = "~/frances_ex/"
                  out_directory = "~/Documents/hurricaneexposure/writing/DraftExposurePaper/dade_data/")
frances_dir <- "~/Documents/hurricaneexposure/writing/DraftExposurePaper/dade_data/"
county_weather <- readRDS(paste0(frances_dir, fips, ".rds"))
county_weather <-
