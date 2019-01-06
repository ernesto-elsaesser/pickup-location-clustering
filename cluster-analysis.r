# data set: https://s3.amazonaws.com/nyc-tlc/trip+data/yellow_tripdata_2016-06.csv

plc_load <- function(name) {
  filename <- paste(name,".csv",sep="")
  trips <- read.csv(file=filename, header=TRUE, sep=",")
  cols <- c("pickup_longitude", "pickup_latitude")
  coords <- trips[cols]
  m <- data.matrix(coords)
  return(m)
}

plc_cluster <- function(m, dist, minPts) {
  library("dbscan")
  eps = dist / 111320 # meters per longitudinal degrees (approx.)
  start <- Sys.time()
  db <- dbscan(m, eps, minPts)
  end <- Sys.time()
  db$time <- end-start
  return(db)
}

plc_get_centers <- function(m, db) {
  cs <- matrix(0, 0, 2)
  max <- max(db$cluster)
  for (cl in 1:max) {
    coords <- m[db$cluster == cl, ]
    cs <- rbind(cs, colMeans(coords))
  }
  return(cs)
}

plc_plot <- function(m, db, xlim, ylim, outline) {
  hits <- m[db$cluster != 0, ]
  noise <- m[db$cluster == 0, ]
  plot(outline, xlim=xlim, ylim=ylim)
  points(noise, col = "grey", pch = 20, cex = .5, xlim=xlim, ylim=ylim)
  points(hits, col = "orange", pch = 20, cex = 1.0, xlim=xlim, ylim=ylim)
}

plc_plot_jpg <- function(m, db, xlim, ylim, outline) {
  jpeg(filename = "plot.jpeg", width = 5000, height = 5000, units = "px", quality = 100)
  plc_plot(m, db, xlim, ylim, outline)
  dev.off()
}

plc_plot_svg <- function(m, db, xlim, ylim, outline) {
  library("svglite")
  svglite(file = "plot.svg", width = 10, height = 10)
  plc_plot(m, db, xlim, ylim, outline)
  dev.off()
}

plc_plot_manhattan <- function(m, db) {
  library("tigris")
  options(tigris_use_cache = TRUE)
  manhattan <- area_water("New York", "New York County")
  xlim <- c(-74.13921, -73.80396)
  ylim <- c(40.67144, 40.89032)
  plc_plot(m, db, xlim, ylim, manhattan)
}

plc_plot_upper_manhattan <- function(m, db) {
  library("tigris")
  options(tigris_use_cache = TRUE)
  manhattan <- area_water("New York", "New York County")
  xlim <- c(-73.96431, -73.91659)
  ylim <- c(40.79081, 40.81835)
  plc_plot(m, db, xlim, ylim, manhattan)
}
