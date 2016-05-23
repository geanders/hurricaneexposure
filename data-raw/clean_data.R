library(dplyr)
library(sp)
library(maps)
library(lubridate)
# Read in and clean up `county_centers`
county_centers <-
        read.csv("/Users/brookeanderson/Documents/Hopkins\ Postdoc/hurricanes/DetermineCountyStormDates/CenPop2010_Mean_CO.txt", header = TRUE, skip = 2)

colnames(county_centers) <- c("state_fips", "county_fips", "county_name",
                              "state_name", "population", "latitude",
                              "longitude")
county_centers$fips <- paste0(sprintf("%02d",county_centers$state_fips),
                              sprintf("%03d",county_centers$county_fips))
county_centers <- county_centers[ , c(1:2, 8, 3:7)]
save(county_centers, file = "data/county_centers.Rdata")

# Bring in latest version of extended hurricane tracks
hurr_tracks <- read.fwf("http://rammb.cira.colostate.edu/research/tropical_cyclones/tc_extended_best_track_dataset/ebtrk_atlc_1988_2014.txt",
                       widths = c(7, 10, 2, 2, 3, 5, 5, 7, 3, 5,
                                  4, 4, 5, 3, 4, 3, 3, 3,
                                  4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1),
                       na.strings = "-99")
colnames(hurr_tracks) <- c("storm_id", "storm_name", "month", "day",
                          "hour", "year", "latitude", "longitude",
                          "max_wind", "min_pressure", "rad_max_wind",
                          "eye_diameter", "pressure_1", "pressure_2",
                          paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
                          paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
                          paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
                          "storm_type", "distance_to_land", "final")
hurr_tracks <- select(hurr_tracks, -storm_id) %>%
        mutate(storm_id = paste(gsub(" ", "", storm_name), year, sep = "-"),
               month = sprintf("%02d", month),
               day = sprintf("%02d", day),
               hour = sprintf("%02d", hour),
               minute = "00",
               longitude = -1 * longitude) %>%
        rename(wind = max_wind) %>%
        select(storm_id, year, month, day, hour, minute, latitude, longitude,
               wind)
hurr_tracks <- unite_(hurr_tracks, "date",
                      c("year", "month", "day", "hour", "minute"),
                      sep = "")
use_data(hurr_tracks)

