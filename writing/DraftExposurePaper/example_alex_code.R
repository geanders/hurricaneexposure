options("noaakey" = Sys.getenv("noaakey"))
library(countyweather)
library(hurricaneexposure)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(lubridate)

alex_fips <- c("48355", "48061", "48215", "48057", "12087")
county_timeseries(alex_fips, percent_coverage = 0,
                  date_min = "1988-01-01", date_max = "2011-12-31",
                  var = "PRCP", out_directory = "~/alex_ex/"
                  #out_directory = "~/Documents/hurricaneexposure/writing/DraftExposurePaper/alex_ex/"
)

# Check that it worked...
list.files("~/alex_ex")
#list.files("~/Documents/hurricaneexposure/writing/DraftExposurePaper/alex_ex/")

# Do some plots
#alex_dir <- "~/Documents/hurricaneexposure/writing/DraftExposurePaper/alex_ex/"
alex_dir <- "~/alex_ex/"
alex_counties <- gsub(".rds", "", list.files(alex_dir))

alex_ave <- vector("list", length = length(alex_counties))
names(alex_ave) <- alex_counties

for(x in alex_counties){
        county_weather <- readRDS(paste0(alex_dir, x, ".rds"))#$averaged
        county_weather <- county_weather %>%
                filter(date %in% seq(from = as.Date("2010-06-01"), to = as.Date("2010-07-01"), by = 1))
                       #year(date) == 2010 & month(date) == 6 | month(date) == 7)
        county_weather$fips <- x
        alex_ave[[x]] <- county_weather
}
alex_ave <- do.call("rbind", alex_ave)

alex_rain <- county_rain(counties = alex_counties, start_year = 2010,
                        end_year = 2010, rain_limit = 0, dist_limit = 1000) %>%
        filter(storm_id == "Alex-2010")

library(ggthemes)
alex_ave %>%
        ggplot(aes(x = date, y = prcp, color = prcp_reporting)) +
        geom_line() + geom_point() +
        geom_segment(data = alex_rain,
                     aes(x = ymd(closest_date) - ddays(2),
                         xend = ymd(closest_date) + ddays(1),
                         y = tot_precip, yend = tot_precip), color = "red") +
        facet_wrap(~ fips, ncol = 2) +
        theme_few()
# Note: different scales on this one-- cumulative rain over four days for
# `alex_rain`, `alex_ave` is one day at a time

alex_ave %>%
        left_join(alex_rain, by = "fips") %>%
        group_by(fips) %>%
        filter(ymd(closest_date) - ddays(2) <= date &
                       date <= ymd(closest_date) + ddays(1)) %>%
        summarize(monitor_rain = sum(prcp),
                  tot_precip = first(tot_precip),
                  prcp_reporting = mean(prcp_reporting)) %>%
        ggplot(aes(x = monitor_rain, y = tot_precip)) +
        geom_abline(aes(intercept = 0, slope = 1), color = "gray", alpha = 0.5) +
        geom_point(aes(size = prcp_reporting), alpha = 0.5) +
        geom_text(aes(label = fips)) +
        theme_few() +
        scale_size_continuous(guide = "none") +
        xlab("Rainfall (mm) based on \naveraged county monitors") +
        ylab("Rainfall (mm) based on \nNLDAS-2 county data") +
        ggtitle("Monitor versus NLDAS rainfall estimates \nfor Hurricane Alex (2010)")
#only showing one FIPS, but all FIPS are present in both alex_rain and alex_ave datasets
