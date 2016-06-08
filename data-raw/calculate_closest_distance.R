library(sp)
library(dplyr)
library(lubridate)
library(hurricaneexposure)

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
                as.matrix(storm_tracks[,c("lon", "phi")]),
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
                               storm_lat = storm_tracks$phi[min_locs],
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

library(countytimezones)
closest_dist <- add_local_time(df = closest_dist,
                     datetime_colname = "closest_date",
                     fips = closest_dist$fips,
                     include_tz = FALSE) %>%
        dplyr::rename(closest_time_utc = closest_date,
               closest_date = local_date) %>%
        mutate(closest_time_utc = ymd_hm(closest_time_utc)) %>%
        mutate(closest_time_utc = format(closest_time_utc,
                                         "%Y-%m-%d %H:%M"))

use_data(closest_dist, overwrite = TRUE)


