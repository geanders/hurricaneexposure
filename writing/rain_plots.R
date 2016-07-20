data(closest_dist, package = "hurricaneexposuredata")
all_storms <- unique(closest_dist$storm_id)

plot_both <- function(storm, metric = "distance"){
        print(storm)
        map_1 <- map_counties(storm = storm, metric = metric)
        map_2 <- map_tracks(storms = storm, plot_object = map_1,
                            plot_points = FALSE)
        map_2 <- map_2 + ggplot2::ggtitle(storm)
        return(map_2)
}

plot_events <- function(storm, event_type){
        print(storm)
        map_1 <- map_event_exposure(storm_id = storm,
                                    event_type = event_type)
        map_2 <- map_tracks(storms = storm, plot_object = map_1,
                            plot_points = FALSE)
        map_2 <- map_2 + ggplot2::ggtitle(storm)
        return(map_2)
}

pdf(file = "writing/rain_plots.pdf", width = 7, height = 4)
# Only can run these through 2011
lapply(all_storms[1:125], plot_both, metric = "rainfall")
dev.off()

pdf(file = "writing/distance_plots.pdf", width = 7, height = 4)
lapply(all_storms, plot_both, metric = "distance")
dev.off()

pdf(file = "writing/wind_plots.pdf", width = 7, height = 4)
lapply(all_storms, plot_both, metric = "wind")
dev.off()

pdf(file = "writing/wind_events_plots.pdf", width = 7, height = 4)
lapply(all_storms, plot_events, event_type = "wind")
dev.off()

pdf(file = "writing/tornado_plots.pdf", width = 7, height = 4)
lapply(all_storms, plot_events, event_type = "tornado")
dev.off()

pdf(file = "writing/flood_plots.pdf", width = 7, height = 4)
# only since 1996
lapply(all_storms[31:length(all_storms)], plot_events, event_type = "flood")
dev.off()
