fs <- list.files(".", full.names = TRUE, pattern = "las$")

x <- as.numeric(substr(basename(fs), 1, 3)) * 1000
y <- as.numeric(substr(basename(fs), 4, 7)) * 1000
library(graticule)
gx <- unique(x)
gy <- unique(y)
cx <- gx + diff(gx[1:2])/2
cy <- gy + diff(gy[1:2])/2

g <- graticule(c(gx, max(gx) + diff(gx[1:2])), c(gy, max(gy) + diff(gy[1:2])), tiles = TRUE)
g <- g[extract(g, cbind(head(x, -1) + diff(x[1:2])/2, y = head(y, -1) + diff(y[1:2])/2))$poly.ID, ]

cxy <- as.matrix(expand.grid(cx, cy))
cxy <- cxy[!is.na(extract(g, cxy)$poly.ID), ]
plot(g, asp = 1)
points(cxy)


