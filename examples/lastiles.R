fs <- list.files(".", full.names = TRUE, pattern = "las$")

x <- as.numeric(substr(basename(fs), 1, 3)) * 1000
y <- as.numeric(substr(basename(fs), 4, 7)) * 1000
library(graticule)
library(raster)
library(rlas)
library(dismo)
library(rgdal)
library(rgl)

library(RTriangle)
prj <- "+init=EPSG:28355"
gx <- unique(x)
gy <- unique(y)
cx <- x + diff(gx[1:2])/2
cy <- y + diff(gy[1:2])/2

g <- graticule(c(gx, max(gx) + diff(gx[1:2])), c(gy, max(gy) + diff(gy[1:2])), tiles = TRUE)
projection(g) <- prj
g <- g[match(over(SpatialPoints(cbind(cx, cy), proj4string = CRS(prj)), g)$layer, g$layer), ]

g$lasfile <- fs[match(over(SpatialPoints(cbind(cx, cy), proj4string = CRS(prj)), g)$layer, g$layer)]
plot(g, asp = 1)
points(cx, cy)

im <- gmap(g, type = "satellite")
plot(im)
plot(spTransform(g, projection(im)), add = TRUE)

g$lasfile[match(197, g$layer)]
x <- readLAS(g$lasfile[match(197, g$layer)])

rgl.points(x[,1:3])

im2 <- gmap(g[match(197, g$layer), ], type = "satellite", scale = 2)

plot(im2)
ex <- drawExtent()
xx <- cbind(project(project(x[,1:2], prj, inv = TRUE), projection(im2)), x[,3])
asub <- xx[,1] >= xmin(ex) & xx[,1] <= xmax(ex) & xx[,2] >= ymin(ex) & xx[,2] <= ymax(ex)

xx <- xx[asub, ]
xx <- xx[!duplicated(xx[,1:2]), ]

xcols <- im2@legend@colortable[extract(im2, xx[,1:2]) + 1]

tri <- RTriangle::triangulate(pslg(P = xx[, 1:2]))
a <- tetrahedron3d()
a$vb <- t(cbind(tri$P, xx[,3], 1))
a$vb[3,a$vb[3,] > 100] <- 0
a$it <- t(tri$T)


plot3d(t(a$vb), col = xcols, asp = "iso", axes = FALSE)
shade3d(a, col = xcols[a$it])
