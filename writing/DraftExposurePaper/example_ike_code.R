county_timeseries(ike_fips, percent_coverage = 0,
                  date_min = "1988-01-01", date_max = "2011-12-31",
                  var = "PRCP")

ike_dir <- "~/tmp/ike_ex/"
ike_counties <- gsub(".rds", "", list.files(ike_dir))

ike_ave <- vector("list", length = length(ike_counties))
names(ike_ave) <- ike_counties

for(x in ike_counties){
        county_weather <- readRDS(paste0(ike_dir, x, ".rds"))$averaged
        county_weather <- county_weather %>%
                filter(year(date) == 2008 & month(date) == 9)
        county_weather$fips <- x
        ike_ave[[x]] <- county_weather
}
ike_ave <- do.call("rbind", ike_ave)

ike_rain <- county_rain(counties = ike_counties, start_year = 2008,
                        end_year = 2008, rain_limit = 0, dist_limit = 1000) %>%
        filter(storm_id == "Ike-2008")

library(ggthemes)
ike_ave %>%
        ggplot(aes(x = date, y = prcp, color = prcp_reporting)) +
        geom_line() + geom_point() +
        geom_segment(data = ike_rain,
                     aes(x = ymd(closest_date) - ddays(2),
                         xend = ymd(closest_date) + ddays(1),
                         y = tot_precip, yend = tot_precip), color = "red") +
        facet_wrap(~ fips, ncol = 2) +
        theme_few()
# Note: different scales on this one-- cumulative rain over four days for
# `ike_rain`, `ike_ave` is one day at a time

ike_ave %>%
        left_join(ike_rain, by = "fips") %>%
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
        ggtitle("Monitor versus NLDAS rainfall estimates \nfor Hurricane Ike (2008)")
