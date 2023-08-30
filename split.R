# The problem
#
# There is a need to split road network with the grid.
# The only function for this I'm aware of is lwgeom::st_split
# This function is quite slow for dense grid (thousands of lines) and many roads (milions)
#
# The challange is how to optimize it or make more 'smarter'

library(sf)
library(dplyr)

mkgrid <- function(x, lines_number = 100) {
  # function creates grid based on the bbox of the sf object with defined number of lines
  # inspired by https://grass.osgeo.org/grass82/manuals//v.mkgrid.html
  lines_number <- 100
  pbox <- sf::st_bbox(x)
  pbox <- data.frame(xmin = pbox[1], ymin = pbox[2], xmax = pbox[3], ymax = pbox[4])

  x = seq.int(from = pbox$xmin, to = pbox$xmax, length.out = lines_number)

  # vertical lines
  grid_v <- as.data.frame(x) |>
    mutate(y = x, from = pbox$ymin, to = pbox$ymax) |>
    as.matrix()
  geom <- apply(grid_v, 1, function(x)  st_as_text(st_linestring(matrix(x, ncol = 2))), simplify = T)
  grid_v <- cbind(as.data.frame(grid_v), geom) |>  st_as_sf(wkt = "geom", crs = "EPSG:4326")

  # horizontal lines
  y = seq.int(from = pbox$ymin, to = pbox$ymax, length.out = lines_number)
  grid_h <- as.data.frame(y) |>
    mutate(x = y, from = pbox$xmin, to = pbox$xmax) |> select(from, to, x, y) |>
    as.matrix()
  geom <- apply(grid_h, 1, function(x)  st_as_text(st_linestring(matrix(x, ncol = 2))), simplify = T)
  grid_h <- cbind(as.data.frame(grid_h), geom) |>  st_as_sf(wkt = "geom", crs = "EPSG:4326")

  # grid as one object
  grid <- st_union(rbind(grid_h, grid_v))

  list(grid = grid, grid_h = grid_h, grid_v = grid_v)
}

poznan_pbf_filnename = "~/data/osm/"
list.files(poznan_pbf_filnename)
file.remove(file.path(poznan_pbf_filnename, "bbbike_Poznan.gpkg"))
# osmextract::oe_vectortranslate(poznan_pbf_filnename)
roads_all = osmextract::oe_get("Poznan", extra_tags = "maxspeed", force_vectortranslate = TRUE)

set.seed(1234)

roads = roads_all |>
  sample_n(100)

time_st_segmentize = system.time({
  roads_segmented <- st_as_sf(roads, wkt = "geometry", crs = "EPSG:4326") |>
  sf::st_segmentize(dfMaxLength = 2)
})

#### Verification
# plot(roads_all$geometry[1:5000], col = "red")
# plot(grid_v$geom, col = "lightgreen", add = T)
# plot(grid_h$geom, col = "lightgreen", add = T)

grid <- mkgrid(roads_all)
time_st_split <- system.time({roads_splitted <- lwgeom::st_split(roads, grid$grid)})

rbind(time_st_segmentize, time_st_split)

##### Comparing results
comp_results <- function(x, y){
  x <- st_as_text(x$geometry) |> stringr::str_length()
  y <- st_as_text(y$geometry) |> stringr::str_length()
  z <- ifelse(x - y!=0 ,1,0)
  sum(z)
}

# Number of transformed roads should be the same
comp_results(roads, roads_segmented)
comp_results(roads, roads_splitted)