library(sf)
library(RColorBrewer)

#Load shape file
shp <- st_read("C:/Users/kforce/Desktop/Big Data 220/Final Project/taxi_zones/taxi_zones.shp")
geom <- shp$geometry

#Load tip data 
tip.data <- read.csv("C:/Users/kforce/Desktop/Big Data 220/Final Project/TipData/TipData.csv", stringsAsFactors = F, header = F)

#Plot tip percentage
num.cols <- 10
tip.col.scale <- colorRampPalette(c("red", "yellow", "green"))(num.cols)
breakpoints <- seq(0, max(tip.data[, 2]), length.out = num.cols)

tip.cols <- c()
for (i in 1:263){
  if (i %in% tip.data[, 1]){
    row.ind <- which(tip.data[, 1] == i)
    which.col <- max(which(tip.data[row.ind, 2] >= breakpoints))
    tip.cols <- c(tip.cols, tip.col.scale[which.col])
  } else {
    tip.cols <- c(tip.cols, "white")
  }
}

plot(geom, col = tip.cols, main = "Average Tip Percentage by Neighborhood")


#Plot by number of trips
num.cols <- 10
trip.col.scale <- colorRampPalette(c("red", "yellow", "green"))(num.cols)
breakpoints <- quantile(tip.data[, 3], probs = seq(0, 1, length.out = num.cols))#seq(0, max(tip.data[, 3]), length.out = num.cols)

trip.cols <- c()
for (i in 1:263){
  if (i %in% tip.data[, 1]){
    row.ind <- which(tip.data[, 1] == i)
    which.col <- max(which(tip.data[row.ind, 3] >= breakpoints))
    trip.cols <- c(trip.cols, trip.col.scale[which.col])
  } else {
    trip.cols <- c(trip.cols, "white")
  }
}

plot(geom, col = trip.cols, main = "Ride Count by Neighborhood")





