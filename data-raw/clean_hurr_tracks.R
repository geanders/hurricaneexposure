library(dplyr)
library(sp)
library(maps)
library(lubridate)

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
format_longitude <- function(lon){
        lon <- as.character(lon)
        names(lon) <- NULL
        lon <- sapply(lon, function (x)  sub("^\\s+", "", x))
        out <- strsplit(lon, " ")
        out <- sapply(out, function(x) x[[1]])
        out <- -1 * as.numeric(out)
        return(out)
}
hurr_tracks <- select(hurr_tracks, -storm_id) %>%
        mutate(storm_name = as.character(storm_name),
               storm_name = paste0(substr(storm_name, 1, 1),
                                  substr(tolower(storm_name), 2,
                                         nchar(storm_name))),
               storm_id = paste(gsub(" ", "", storm_name), year, sep = "-"),
               longitude = format_longitude(longitude),
               month = sprintf("%02d", month),
               day = sprintf("%02d", day),
               hour = sprintf("%02d", hour),
               minute = "00") %>%
        rename(wind = max_wind) %>%
        select(storm_id, year, month, day, hour, minute, latitude, longitude,
               wind)
hurr_tracks <- tidyr::unite_(hurr_tracks, "date",
                      c("year", "month", "day", "hour", "minute"),
                      sep = "")
use_data(hurr_tracks, overwrite = TRUE)

