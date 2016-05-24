library(sp)
library(dplyr)
library(lubridate)

data(county_centers, package = "hurricaneexposure")
data(hurr_tracks, package = "hurricaneexposure")

# install_github("geanders/stormwindmodel")
library(stormwindmodel)

calc_closest_dist <- function(this_storm = "Floyd-1999"){
        print(this_storm)
        storm_tracks <- subset(hurr_tracks, storm_id == this_storm)
        # Linearly impute tracks to every 15 minutes
        storm_tracks <- create_full_track(hurr_track = storm_tracks,
                                          tint = 0.25) %>%
                mutate(lon = -1 * lon)

        # Calculate distance from county center to storm path
        storm_county_distances <- spDists(
                as.matrix(county_centers[,c("longitude", "latitude")]),
                as.matrix(storm_tracks[,c("lon", "lat")]),
                longlat = TRUE) # Return distance in kilometers

        min_locs <- apply(storm_county_distances, 1, which.min)
        min_dists <- apply(storm_county_distances, 1, min)

        study_states <- c("Alabama", "Arkansas", "Connecticut", "Delaware",
                          "District of Columbia", "Florida", "Georgia",
                          "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky",
                          "Louisiana", "Maine", "Maryland", "Massachusetts",
                          "Michigan", "Mississippi", "Missouri",
                          "New Hampshire", "New Jersey", "New York", "North Carolina",
                          "Ohio", "Oklahoma", "Pennsylvania", "Rhode Island",
                          "South Carolina", "Tennessee", "Texas", "Vermont",
                          "Virginia", "West Virginia", "Wisconsin")

        closest_dist <- mutate(county_centers,
                               closest_date = storm_tracks$date[min_locs],
                               storm_lat = storm_tracks$lat[min_locs],
                               storm_long = storm_tracks$lon[min_locs],
                               storm_dist = min_dists) %>%
                filter(state_name %in% study_states) %>%
                mutate(closest_date = format(closest_date, "%Y%m%d%H%M"),
                       storm_id = this_storm) %>%
                select(storm_id, fips, closest_date, storm_dist)

        return(closest_dist)
}

# Apply to all hurricane tracks
hurrs <- as.character(unique(hurr_tracks$storm_id))

closest_dist <- lapply(hurrs, calc_closest_dist)
closest_dist <- do.call("rbind", closest_dist)
use_data(closest_dist, overwrite = TRUE)


# # Plotting
# map("state", region = study_states,
#     interior = FALSE,
#     mar = c(0, 0, 0, 0), ylim = c(24, 48), xlim = c(-107, -60))
#
# map("state", region = study_states,
#     boundary = FALSE, col = "gray", add = TRUE)
#
# points(closest_dist$longitude, closest_dist$latitude, pch = ".",
#        col = "gray")
#
# box(col = "lightgray")
#
# with(closest_dist, segments(longitude, latitude,
#                                   storm_long, storm_lat, col = rgb(0, 0, 1, 0.2)))
# points(storm_tracks$lon, storm_tracks$lat,
#        pch = 21, cex = 0.8, bg = "yellow", col = "black", type = "l")
#
# pos_by_day <- subset(storm_tracks, as.POSIXlt(date)$hour == 0)
# with(pos_by_day[7:11,], text(lon, lat, format(date, "%b %d, %Y"),
#                              pos = 4, cex = 0.8))
