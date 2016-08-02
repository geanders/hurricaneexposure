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
                      day_b3 = day_0 - days(3),
                      day_b4 = day_0 - days(4),
                      day_b5 = day_0 - days(5),
                      day_a1 = day_0 + days(1),
                      day_a2 = day_0 + days(2),
                      day_a3 = day_0 + days(3)) %>%
        dplyr::select(storm_id, fips, day_b5, day_b4, day_b3, day_b2, day_b1,
                      day_0, day_a1, day_a2, day_a3) %>%
        tidyr::gather(key = lag, value = day, -storm_id, -fips) %>%
        dplyr::mutate(day = as.numeric(format(day, "%Y%m%d")))

all_dates <- unique(check_dates$day)
all_fips <- unique(check_dates$fips) # has Miami as "12086", and is still in check_dates here
all_fips <- c(all_fips, as.integer(12025))
check_dates[check_dates$fips == 12086, "fips"] <- 12025

dade_rain <- data.table::fread(
        #"/Users/joshuaferreri/Documents/nasa_precip_export_2.txt",
        "~/Documents/CSU2016/hurricaneproject/hurricaneexposuredata/data-raw/nasa_precip_export_2.txt",
        header = TRUE,
        select = c("county", "year_month_day", "precip", "precip_max")) %>%
        dplyr::filter(county %in% all_fips,
                      year_month_day %in% all_dates) %>%
        dplyr::rename(fips = county, day = year_month_day) %>%
        dplyr::right_join(data.table(check_dates),
                          by = c("fips" = "fips", "day" = "day")) %>%
        dplyr::filter(!is.na(precip) & !is.na(precip_max)) %>%
        dplyr::arrange(storm_id, fips) %>%
        dplyr::select(fips, storm_id, day, lag, precip, precip_max) %>%
        dplyr::mutate(fips = sprintf("%05d", fips),
                      lag = gsub("day_", "", lag),
                      lag = gsub("b", "-", lag),
                      lag = gsub("a", "", lag),
                      lag = as.numeric(lag)) %>%
        dplyr::filter(fips == "12025")
dade_rain[dade_rain$fips == 12025, "fips"] <- 12086
dade_rain <- dade_rain %>%
        rename(date = day) %>%
        mutate(date = as.character(date)) %>%
        mutate(date = as.Date(date, format = "%Y%m%d"))

#create timeseries for Dade precipitation data from the `countyweather` package

fips <- "12086"
county_timeseries(fips, percent_coverage = 0,
                  date_min = "1988-01-01", date_max = "2011-12-31",
                  var = "PRCP",
                  out_directory = "dade_data/")
                  #out_directory = "~/Documents/hurricaneexposure/writing/DraftExposurePaper/dade_data/")

# miami_dade_dir <- "~/Documents/hurricaneexposure/writing/DraftExposurePaper/dade_data/"
dade_dir <- "dade_data/"
county_weather <- readRDS(paste0(dade_dir, fips, ".rds"))

county_weather <- county_weather %>%
        filter(date %in% seq(from = as.Date("1988-01-01"), to = as.Date("2011-12-31"), by = 1))

county_weather$fips <- fips
#dade_rain <- county_rain(counties = fips, start_year = 1988, end_year = 2011,
 #                        rain_limit = 0, dist_limit = 1000) %>%
  #      filter(fips == "12086")
#dade_rain <- dade_rain %>%
 #       rename(date = closest_date)
  #      left_join(dade_rain, by = date)

#Join `dade_rain` and `county_weather` datasets by date and create a plot comparing rain by storm (not fips)

county_weather %>%
        inner_join(dade_rain, "date") %>%
        group_by(storm_id) %>%
        filter(lag == -3 | lag == -2 | lag == -1 |lag == 0 | lag == 1) %>%
        arrange(storm_id) %>%
        #filter(ymd(closest_dist$closest_date) - ddays(2) <= date &
         #              date <= ymd(closest_dist$closest_date) + ddays(1)) %>%
        summarize(monitor_rain = sum(prcp),
                  tot_precip = sum(precip),
                  prcp_reporting = mean(prcp_reporting)) %>%
        ggplot(aes(x = monitor_rain, y = tot_precip)) +
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
        ggtitle("Monitor versus NLDAS rainfall estimates \nfor Miami-Dade county by Storm")

lag_sum <- function(counties = NULL, county_weather, df2, year_range = NULL,
                              distance_limit = NULL, rain_limit = NULL,
                              include_rain = FALSE, days_included = NULL,
                              output_vars = c("fips", "cw_precip")){

        county_weather <- as.data.frame(county_weather)
        df <- inner_join(county_weather, df2, by = "date") %>%
                arrange(storm_id)
        df <- rename(df, fips = fips.x)

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
                df <- data.table::data.table(df)
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

countyweather_rain <- function(counties, county_weather, df2, start_year, end_year,
                        rain_limit, dist_limit,
                        days_included = c(-2, -1, 0, 1)){

        county_rain_storm_df <- lag_sum(counties = counties,
                                           county_weather = county_weather,
                                           df2 = df2,
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


