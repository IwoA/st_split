# The problem
#
# There is a need to split road network with the grid.
# The only function for this I'm aware of is lwgeom::st_split
# This function is quite slow for dense grid (thousands of lines) and many roads (milions)
#
# The challange is how to optimize it or make more 'smarter'



library(sf)
library(dplyr)

pbox <- osmdata::getbb('Poland') |> as.data.frame() #bbox of Poland

set.seed(1234)

linestring <- function(){ # function to create random linestring within the bbox
  x <- sample(seq(pbox$min[1], pbox$max[1], .1), 2)
  y <- sample(seq(pbox$min[2], pbox$max[2], .1), 2)
  l <- as.matrix(cbind(x,y))
  st_linestring(l)
}

n <- 10000 # number of 'roads'
roads <- data.frame(road_id = 1:n, geometry = rep(NA, n))
for (i in 1:n) { # loop creating roads
  roads$geometry[i] <- st_as_text(linestring())
}
roads <- st_as_sf(roads, wkt = "geometry", crs = "EPSG:4326") |>
  sf::st_segmentize(dfMaxLength = 10000) # roads consists of many segments of length 10 km

#### Grid
lines_number <- 2000
x = seq.int(from = pbox$min[1], to = pbox$max[1], length.out = lines_number)

# vertical lines
grid_v <- as.data.frame(x,) |>
  mutate(y = x, from = pbox$min[2], to = pbox$max[2]) |>
  as.matrix()
geom <- apply(grid_v, 1, function(x)  st_as_text(st_linestring(matrix(x, ncol = 2))), simplify = T)
grid_v <- cbind(as.data.frame(grid_v), geom) |>  st_as_sf(wkt = "geom", crs = "EPSG:4326")

# horizontal lines
y = seq.int(from = pbox$min[2], to = pbox$max[2], length.out = lines_number)
grid_h <- as.data.frame(y,) |>
  mutate(x = y, from = pbox$min[1], to = pbox$max[1]) |> select(from, to, x, y) |>
  as.matrix()
geom <- apply(grid_h, 1, function(x)  st_as_text(st_linestring(matrix(x, ncol = 2))), simplify = T)
grid_h <- cbind(as.data.frame(grid_h), geom) |>  st_as_sf(wkt = "geom", crs = "EPSG:4326")

# grid as one object
grid <- st_union(rbind(grid_h, grid_v))

#### Verification
poland = spData::world |>
  filter(name_long == "Poland")

plot(poland$geom, col = "white")
plot(sample(roads$geometry,5), col = "red", add = T)
plot(grid_v$geom[seq(1,lines_number, 100)], col = "lightgreen", add = T)
plot(grid_h$geom[seq(1,lines_number, 100)], col = "lightgreen", add = T)



