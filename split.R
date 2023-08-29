# The problem
#
# There is a need to split road network with the grid.
# The only function for this I'm aware of is lwgeom::st_split
# This function is quite slow for dense grid (thousands of lines) and many roads (milions)
#
# The challange is how to optimize it or make more 'smarter'

library(sf)
library(dplyr)

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
  sf::st_segmentize(dfMaxLength = 5) |>  # roads consists of many segments of length 50 m max
  sf::st_cast("LINESTRING") |>  # cast to LINESTRING
  sf::st_collection_extract("LINESTRING") |>  # extract LINESTRING from MULTILINESTRING
})
waldo::compare(roads, roads_segmented)
nrow(roads) / time_st_segmentize[3]
nrow(roads_segmented) / nrow(roads)


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



