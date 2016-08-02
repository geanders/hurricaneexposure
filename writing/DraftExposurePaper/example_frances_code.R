options("noaakey" = Sys.getenv("noaakey"))
library(countyweather)
library(hurricaneexposure)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(lubridate)

frances_fips <- c("12099", "12086", "13311", "37011", "12121")
county_timeseries(frances_fips, percent_coverage = 0,
                  date_min = "1988-01-01", date_max = "2011-12-31",
                  var = "PRCP", #out_directory = "~/frances_ex/"
                  out_directory = "~/Documents/hurricaneexposure/writing/DraftExposurePaper/frances_ex/"
)

# Check that it worked...
list.files("~/frances_ex")
#list.files("~/Documents/hurricaneexposure/writing/DraftExposurePaper/frances_ex/")

# Do some plots
#frances_dir <- "~/Documents/hurricaneexposure/writing/DraftExposurePaper/frances_ex/"
frances_dir <- "~/frances_ex/"
frances_counties <- gsub(".rds", "", list.files(frances_dir))

frances_ave <- vector("list", length = length(frances_counties))
names(frances_ave) <- frances_counties

for(x in frances_counties){
        county_weather <- readRDS(paste0(frances_dir, x, ".rds"))#$averaged
        county_weather <- county_weather %>%
                filter(date %in% seq(from = as.Date("2004-08-20"), to = as.Date("2004-09-11"), by = 1))
        #year(date) == 2010 & month(date) == 6 | month(date) == 7)
        county_weather$fips <- x
        frances_ave[[x]] <- county_weather
}
frances_ave <- do.call("rbind", frances_ave)

frances_rain <- county_rain(counties = frances_counties, start_year = 2004,
                         end_year = 2004, rain_limit = 0, dist_limit = 1000) %>%
        filter(storm_id == "Frances-2004")

library(ggthemes)
frances_ave %>%
        ggplot(aes(x = date, y = prcp, color = prcp_reporting)) +
        geom_line() + geom_point() +
        geom_segment(data = frances_rain,
                     aes(x = ymd(closest_date) - ddays(2),
                         xend = ymd(closest_date) + ddays(1),
                         y = tot_precip, yend = tot_precip), color = "red") +
        facet_wrap(~ fips, ncol = 2) +
        theme_few()
# Note: different scales on this one-- cumulative rain over four days for
# `frances_rain`, `frances_ave` is one day at a time

frances_ave %>%
        left_join(frances_rain, by = "fips") %>%
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
        ggtitle("Monitor versus NLDAS rainfall estimates \nfor Hurricane Frances (2004)")
#same FIPS and same problem here. Ill look into it
