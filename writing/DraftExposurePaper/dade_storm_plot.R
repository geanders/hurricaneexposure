options("noaakey" = Sys.getenv("noaakey"))
library(countyweather)
library(tidyr)
library(dplyr)
library(hurricaneexposure)
library(hurricaneexposuredata)
library(lubridate)
library(data.table)
library(ggplot2)
library(ggthemes)
data("closest_dist")


# Create a new dataset for precip with lag -5 to 3 for fips 12086 only

check_dates <- dplyr::select(closest_dist, -storm_dist) %>%
        dplyr::mutate(closest_date = ymd(closest_date)) %>%
        dplyr::rename(day_0 = closest_date) %>%
        dplyr::mutate(fips = as.integer(fips),
                      day_0 = day_0 + days(0),
                      day_b1 = day_0 - days(1),
                      day_b2 = day_0 - days(2),
                      day_a1 = day_0 + days(1),
                      day_a2 = day_0 + days(2),
                      day_a3 = day_0 + days(3)) %>%
        dplyr::select(storm_id, fips, day_b2, day_b1,
                      day_0, day_a1, day_a2, day_a3) %>%
        tidyr::gather(key = lag, value = day, -storm_id, -fips) %>%
        dplyr::rename(date = day) %>%
        dplyr::filter(fips == "12086") %>%
        dplyr::mutate(fips = as.character(fips))
        #dplyr::mutate(day = as.numeric(format(day, "%Y%m%d")))

all_dates <- unique(check_dates$date)
all_fips <- unique(check_dates$fips) # has Miami as "12086", and is still in check_dates here

#create timeseries for Dade precipitation data from the `countyweather` package

fips <- "12086"
# I've commented this out because we're saving the data, so we only need
# to run it once.
# county_timeseries(fips, percent_coverage = 0,
#                   date_min = "1988-01-01", date_max = "2011-12-31",
#                   var = "PRCP",
#                   out_directory = "dade_data/")
#                   #out_directory = "~/Documents/hurricaneexposure/writing/DraftExposurePaper/dade_data/")

# miami_dade_dir <- "~/Documents/hurricaneexposure/writing/DraftExposurePaper/dade_data/"
dade_dir <- "dade_data/"
dade_weather <- readRDS(paste0(dade_dir, fips, ".rds"))

dade_weather <- dade_weather$averaged %>%
        filter(date %in% seq(from = as.Date("1988-01-01"), to = as.Date("2011-12-31"), by = 1))

dade_weather$fips <- fips
dade_weather <- as.data.frame(dade_weather)

dade_weather <- dade_weather %>%
        dplyr::filter(date %in% all_dates) %>%
        dplyr::right_join(data.table(check_dates),
                          by = c("fips" = "fips", "date" = "date")) %>%
        dplyr::filter(!is.na(prcp)) %>%
        dplyr::arrange(storm_id, fips) %>%
        dplyr::mutate(fips = as.integer(fips)) %>%
        dplyr::mutate(fips = sprintf("%05d", fips),
                      lag = gsub("day_", "", lag),
                      lag = gsub("b", "-", lag),
                      lag = gsub("a", "", lag),
                      lag = as.numeric(lag))

lag_sum <- function(counties = NULL, county_weather, year_range = NULL,
                              distance_limit = NULL, rain_limit = NULL,
                              include_rain = FALSE, days_included = NULL,
                              output_vars = c("fips", "cw_precip")){

        closest_dist <- data.table::data.table(hurricaneexposuredata::closest_dist)

        if(!is.null(counties)){
                closest_dist <- closest_dist[get("fips") %in% counties]
        }

        if(!is.null(year_range)){
                closest_dist <- closest_dist[ , .(storm_id,
                                                  fips,
                                                  closest_date, storm_dist,
                                                  local_time,
                                                  closest_time_utc,
                                                  year = substring(get("closest_date"),
                                                                   1, 4)), ][
                                                                           get("year") %in%
                                                                                   year_range[1]:year_range[2]
                                                                           ]
        }

        if(!is.null(distance_limit)){
                closest_dist <- closest_dist[get("storm_dist") <=
                                                     distance_limit]
        }

        if(include_rain){
                df <- data.table::data.table(county_weather)
                df <- df[get("lag") %in% days_included]
                df <- df[ , .(cw_precip = sum(get("prcp"))),
                              by = .(fips, storm_id)]
                closest_dist <- merge(closest_dist, df, all.x = TRUE,
                                      by = c("storm_id", "fips"))
                if(!is.null(rain_limit)){
                        closest_dist <- closest_dist[get("cw_precip") >=
                                                             rain_limit]
                }
        }

        closest_dist <- closest_dist[ , .SD, , .SDcols = output_vars]
        return(closest_dist)
}

countyweather_rain <- function(counties, county_weather, start_year, end_year,
                        rain_limit, dist_limit,
                        days_included = c(-2, -1, 0, 1)){

        county_rain_storm_df <- lag_sum(counties = counties,
                                           county_weather = county_weather,
                                           year_range = c(start_year, end_year),
                                           distance_limit = dist_limit,
                                           rain_limit = rain_limit,
                                           include_rain = TRUE,
                                           days_included = days_included,
                                           output_vars = c("storm_id", "fips",
                                                           "closest_date",
                                                           "storm_dist",
                                                           "cw_precip",
                                                           "local_time",
                                                           "closest_time_utc"))
        rain_storm_df <- filter_storm_data(counties = counties,
                                           year_range = c(start_year, end_year),
                                           distance_limit = dist_limit,
                                           rain_limit = rain_limit,
                                           include_rain = TRUE,
                                           days_included = days_included,
                                           output_vars = c("storm_id", "fips",
                                                           "closest_date",
                                                           "storm_dist",
                                                           "tot_precip",
                                                           "local_time",
                                                           "closest_time_utc"))
        rain_df <- merge(county_rain_storm_df, rain_storm_df, all.x = TRUE,
                         by = c("fips", "storm_id", "closest_date", "storm_dist",
                                "local_time", "closest_time_utc"))
        return(rain_df)
}

miami_rain <- countyweather_rain(counties = "12086", county_weather = dade_weather,
                                 start_year = 1988, end_year = 2011,
                                 rain_limit = 0, dist_limit = 1000)
miami_plot <- ggplot(miami_rain, aes(x = cw_precip, y = tot_precip)) +
        geom_hline(aes(yintercept = 75), color = "lightgray") +
        geom_vline(aes(xintercept = 75), color = "lightgray") +
        geom_abline(aes(intercept = 0, slope = 1), color = "gray", alpha = 0.5) +
        # geom_point(aes(size = prcp_reporting), alpha = 0.2) +
        geom_point(alpha = 0.5) +
        # geom_text(aes(x = monitor_rain + 100, label = storm_id)) +
        xlim(c(0, 275)) + ylim(c(0, 275)) +
        theme_few() +
        scale_size_continuous(guide = "none") +
        xlab("Rainfall (mm) based on \naveraged county monitors") +
        ylab("Rainfall (mm) based on \nNLDAS-2 county data") +
        ggtitle("Monitor versus NLDAS rainfall estimates \nfor Miami-Dade County, FL, by storm")

