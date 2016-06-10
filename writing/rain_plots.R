all_storms <- unique(closest_dist$storm_id)
# ex_storms <- all_storms[-grep("Unnamed", all_storms)]

plot_both <- function(storm, metric = "distance"){
        map_1 <- map_counties(storm = storm, metric = metric)
        map_2 <- map_tracks(storms = storm, plot_object = map_1,
                            plot_points = FALSE)
        map_2 <- map_2 + ggplot2::ggtitle(storm)
        return(map_2)
}

pdf(file = "writing/rain_plots.pdf", width = 7, height = 4)
lapply(ex_storms[1:100], plot_both, metric = "rainfall")
dev.off()

pdf(file = "writing/distance_plots.pdf", width = 7, height = 4)
lapply(ex_storms[1:100], plot_both, metric = "distance")
dev.off()
