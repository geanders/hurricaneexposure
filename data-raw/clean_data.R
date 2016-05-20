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

# Load and clean up hurricane tracking data, `hurr.tracks`
load("/Users/brookeanderson/Documents/Hopkins\ Postdoc/hurricanes/HurricaneTracks.r")
hurr_tracks <- do.call("rbind", hurr.tracks)
hurr_tracks$storm_id <- rownames(hurr_tracks)
hurr_tracks$storm_id <- sub("\\..*" ,"", hurr_tracks$storm_id)
rownames(hurr_tracks) <- NULL
hurr_tracks <- mutate(hurr_tracks,
                      status = ifelse(status %in% c("TD", "TS", "HU"),
                                      status, "OT"),
                      status = factor(status,
                                      levels = c("OT",
                                                 "TD",
                                                 "TS",
                                                 "HU"),
                                      labels = c("Other",
                                                 "Tropical depression",
                                                 "Tropical storm",
                                                 "Hurricane")),
                      latitude = as.numeric(latitude),
                      longitude = -1 * as.numeric(longitude),
                      wind = as.numeric(wind),
                      storm_id = factor(storm_id))
save(hurr_tracks, file = "data/hurr_tracks.Rdata")

# Bring in latest version of extended hurricane tracks
ext_tracks <- read.fwf("http://rammb.cira.colostate.edu/research/tropical_cyclones/tc_extended_best_track_dataset/ebtrk_atlc_1988_2014.txt",
                       widths = c(7, 10, 2, 2, 3, 5, 5, 7, 3, 5,
                                  4, 4, 5, 3, 4, 3, 3, 3,
                                  4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1),
                       na.strings = "-99")
colnames(ext_tracks) <- c("storm_id", "storm_name", "month", "day",
                          "hour", "year", "latitude", "longitude",
                          "max_wind", "min_pressure", "rad_max_wind",
                          "eye_diameter", "pressure_1", "pressure_2",
                          paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
                          paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
                          paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
                          "storm_type", "distance_to_land", "final")
save(ext_tracks, file = "data/hurr_tracks.Rdata")

#Bring in closest distance data with Miami and DC Fips

load("/Users/joshuaferreri/Documents/HurricaneTracks.r")
study.states <- c('maine', 'new hampshire', 'vermont',                'massachusetts', 'connecticut', 'new york', 'rhode island',
                  'new jersey', 'delaware', 'pennsylvania', 'maryland',                'virginia', 'north carolina', 'south carolina', 'georgia',          'florida', 'alabama', 'mississippi', 'louisiana', 'texas',            'west virginia', 'arkansas', 'tennessee', 'kentucky',
                  'oklahoma', 'missouri', 'illinois', 'kentucky', 'indiana',
                  'michigan', 'ohio', 'wisconsin', 'kansas', 'iowa', 'district of columbia')
data(state.fips)
study.state.fips <- state.fips$fips[state.fips$polyname %in%
                                            study.states]
study.state.fips <- c(study.state.fips, 25, 26, 36, 37, 51)
county.centers <-
        read.csv("/Users/joshuaferreri/Documents/CenPop2010_Mean_CO.txt", header = TRUE, skip = 2)
county.centers <- subset(county.centers,
                         STATEFP %in% study.state.fips)
closest.storm.dates <- vector("list", length(hurr.tracks))
names(closest.storm.dates) <- names(hurr.tracks)

for(hurr in names(hurr.tracks)){
        print(hurr)
        ex <- hurr.tracks[[hurr]]
        ex$latitude <- as.numeric(ex$latitude)
        ex$longitude <- -1 * as.numeric(ex$longitude)
        county.dists <- spDists(as.matrix(county.centers[,c("LONGITUDE", "LATITUDE")]),
                                as.matrix(ex[,c("longitude", "latitude")]),
                                longlat = TRUE)
        min.locs <- apply(county.dists, 1, which.min)
        min.dists <- apply(county.dists, 1, min)
        closest.storm.dates[[hurr]] <- data.frame(
                county.fips = county.centers$COUNTYFP,
                state.fips = county.centers$STATEFP,
                county.lat = county.centers$LATITUDE,
                county.long = county.centers$LONGITUDE,
                closest.date = ex$date[min.locs],
                storm.lat = ex$latitude[min.locs],
                storm.long = ex$longitude[min.locs],
                storm.dist = min.dists,
                strength = ex$status[min.locs])
        closest.storm.dates[[hurr]]$fips <- paste0(
                formatC(closest.storm.dates[[hurr]]$state.fips, width = 2,
                        format = "d", flag = "0"),
                formatC(closest.storm.dates[[hurr]]$county.fips, width = 3,
                        format = "d", flag = "0"))
}
save(closest.storm.dates,
     file = "/Users/joshuaferreri/Documents/ClosestStormDates.Rdata")
load("/Users/joshuaferreri/Documents/ClosestStormDates.Rdata")
closest_distance <- do.call("rbind", closest.storm.dates)
closest_distance <- closest_distance[ , c("fips", "closest.date", "storm.dist")]
closest_distance$storm_id <- rep(names(closest.storm.dates),
                             each = nrow(closest.storm.dates[[1]]))
rownames(closest_distance) <- NULL
closest_distance <- closest_distance[ , c("storm_id", "fips",
                                  "closest.date", "storm.dist")]
colnames(closest_distance)[3:4] <- c("closest_date", "storm_dist")
closest_distance$closest_date <- ymd_hm(closest_distance$closest_date)
save(closest_distance, file = "data/closest_distance.Rdata")

# Bring in closest distance data
#load("/Users/brookeanderson/Documents/Hopkins\ Postdoc/hurricanes/DetermineCountyStormDates/ClosestStormDates.Rdata")
#closest_dist <- do.call("rbind", closest.storm.dates)
#closest_dist <- closest_dist[ , c("fips", "closest.date", "storm.dist")]
#closest_dist$storm_id <- rep(names(closest.storm.dates),
 #                           each = nrow(closest.storm.dates[[1]]))
#rownames(closest_dist) <- NULL
#closest_dist <- closest_dist[ , c("storm_id", "fips",
 #                                 "closest.date", "storm.dist")]
#colnames(closest_dist)[3:4] <- c("closest_date", "storm_dist")
#closest_dist$closest_date <- ymd_hm(closest_dist$closest_date)
#save(closest_dist, file = "data/closest_dist.Rdata")

# Bring in rain exposure data
load("/Users/brookeanderson/Documents/Hopkins\ Postdoc/hurricanes/DetermineCountyStormDates/StormRainTots.Rdata")
storm_rains <- do.call("rbind", storm.rain.tots)
storm_rains$storm_id <- rep(names(storm.rain.tots),
                            each = nrow(storm.rain.tots[[1]]))
rownames(storm_rains) <- NULL
storm_rains <- storm_rains[ , c("storm_id", "fips", "tot.precip")]
colnames(storm_rains) <- c("storm_id", "fips", "tot_precip")
save(storm_rains, file = "data/storm_rains.Rdata")

