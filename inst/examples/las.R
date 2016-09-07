dp <- "E:\\DATA\\Tassie\\LiDAR\\Mt Wellington\\LAS12"
f <- file.path(dp, c("5205247.las", "5215247.las"))

source("R/readLAS.R")
# p2gdal <- "C:/GDAL"
# tfile <- sprintf("%s.tif", tempfile())
# rgbfile <- file.path("..", "wellington_rgb.ecw")
library(raster)
library(dplyr)
d1 <- readLAS(f[1])
d <- data_frame(x = d1[,1], y = d1[,2], z = d1[,3], gpstime = d1[,4], intensity = d1[,5])
d1 <- readLAS(f[2])
d1 <- data_frame(x = d1[,1], y = d1[,2], z = d1[,3], gpstime = d1[,4], intensity = d1[,5])
d <- bind_rows(d, d1); rm(d1)

im <- brick("E:\\DATA\\Tassie\\LiDAR\\Mt Wellington\\reids.tif")
cell <- cellFromXY(im, as.matrix(d[, c("x", "y")]))
rgb <- extract(im, cell)
d$col <- graphics::rgb(d$red, d
d$red <- rgb[,1] / 256
d$green <- rgb[,2] / 256
d$blue <- rgb[,3] / 256

save(d, file = "../d_reidsRGB.RData")

rm(rgb, cell)


load("E:\\DATA\\Tassie\\LiDAR\\Mt Wellington\\d_reidsRGB.RData")
library(dplyr)
library(rgl)

d <- d %>% filter(!is.na(red))
d$col <- rgb(d$red, d$green, d$blue)
d <- select(d, x, y, z, gpstime, intensity, col)
##with(d[seq(1, nrow(d), by = 8), ], plot3d(x, y, z, col = col, size = 0.5))
#with(d[seq(1, nrow(d), by = 8), ], plot3d(x, y, z, col = col, size = 5.5, aspect  = "iso"))
with(d, plot3d(x, y, z, col = col, size = 5.5, aspect  = "iso"))
bg3d(color = "black")
