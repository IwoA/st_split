# The problem
#
# There is a need to split road network with the grid.
# The only function for this I'm aware of is lwgeom::st_split
# This function is quite slow for dense grid (thousands of lines) and many roads (milions)
#
# The challange is how to optimize it or make more 'smarter'

library(sf)
library(dplyr)

raster_to_grid <- function(raster){
  grd <- stars::st_as_stars(raster) |> st_as_sf() |> st_cast("POINT") |> tibble::rownames_to_column() |>
    filter(!rowname %in% as.character(1:(nrows*ncols))) |>  # removes duplicated top-left point
    distinct(geometry) |> # removes duplicated points
    tibble::rowid_to_column() |>
    group_by(rowid) |> # allows to rowwise execution of st_coordinates
    mutate(x = st_coordinates(geometry)[1],
           y = st_coordinates(geometry)[2]) |>
    ungroup() |>
    mutate(
      position = case_when(
        y == min(y) ~ "top",
        y == max(y) ~ "bottom",
        x == min(x) ~ "left",
        x == max(x) ~ "right")
    ) |>
    filter(!is.na(position)) |> # preserves only points on edges
    group_by(position) |> mutate(id = 1:n()) |> # numbers points in groups for joins
    group_by(id) |>
    mutate(position = case_when( # points must be grouped according to line types not egdes
      position %in% c("top", "bottom") ~ "v",
      position %in% c("left", "right") ~ "h"
    )
    ) |>
    group_by(id, position) |>
    summarise(id = mean(id)) |> # points to multipoint
    st_cast("LINESTRING") # multipoint to linestring
}

roads_all = osmextract::oe_get("Poznan", extra_tags = "maxspeed", force_vectortranslate = TRUE)

set.seed(1234)

roads = roads_all |>
  sample_n(100)

##### Make grid

# Make raster
nrows = 10
ncols = 15
raster <- terra::rast(nrows = nrows, ncols = ncols, extent = sf::st_bbox(roads_all), vals = 1)

# Grid from raster
raster_to_grid(raster)

##### 1. segmentize
time_st_segmentize = system.time({
  roads_segmented <- st_as_sf(roads, wkt = "geometry", crs = "EPSG:4326") |>
  sf::st_segmentize(dfMaxLength = 10)
})


grid <- mkgrid(roads_all)

grd <- stars::st_as_stars(sf::st_bbox(roads_all), nx = 100, ny = 100, values = 1) |>
  st_as_sf(as_points = FALSE) |> st_cast("MULTILINESTRING")

##### 2. st_split
time_st_split <- system.time({roads_splitted <- lwgeom::st_split(tibble::rowid_to_column(roads), grid$grid)|>
  sf::st_collection_extract("LINESTRING") |>
  group_by(rowid) |> summarise(sum = mean(rowid)) |> st_cast("LINESTRING")})


##### 3. split_lines
split_lines <- function(roads, blades_h, blades_v) {
  blades_h <- collapse::rsplit(blades_h, 1:nrow(blades_h)) # data.frame to list
  blades_v <- collapse::rsplit(blades_v, 1:nrow(blades_v)) # data.frame to list
  tmp <- purrr::map(blades_h,\(x) lwgeom::st_split(tibble::rowid_to_column(roads), x)) |> # split roads line by line
    purrr::list_rbind() |> # resulting list to data.frame
    sf::st_as_sf() |> # data.frame to sf object
    sf::st_collection_extract("LINESTRING") |>
    group_by(rowid) |> summarise(sum = mean(rowid)) |> st_cast("MULTILINESTRING") # splitted road segments back to one road

  # the same but with vertical lines
  purrr::map(blades_v,\(x) lwgeom::st_split(tmp, x)) |>
    purrr::list_rbind() |> sf::st_as_sf() |> sf::st_collection_extract("LINESTRING") |>
    group_by(rowid) |> summarise(sum = mean(rowid)) |> st_cast("MULTILINESTRING") # st_cast("LINESTRING") 'disappears' parts of multilines
}

time_split_lines <- system.time({roads_split_lines <- split_lines(roads, grid$grid_h, grid$grid_v)})

##### 4. split_lines2
split_lines2 <- function(x, h, v) {
  blades <- rbind(h, v)
  crosses <- st_filter(x, blades, .predicate = st_crosses)
  others <- filter(roads, !osm_id %in% (crosses$osm_id))
  splitted <- lwgeom::st_split(crosses, blades)
  rbind(splitted, others) |> sf::st_collection_extract("LINESTRING")
}

time_split_lines2 <- system.time({roads_split_lines2 <- split_lines2(roads, grid$grid_h, grid$grid_v)})

rbind(time_st_segmentize, time_st_split, time_split_lines, time_split_lines2)

##### Visual verification

library(tmap)
tmap_mode("view")

tm_shape(st_cast(roads_splitted$geometry[10], 'POINT')) + tm_dots(col = "red", scale = 4) +
  tm_shape(st_cast(roads_split_lines$geometry[10], 'POINT')) + tm_dots(col = "blue", scale = 2) +
  tm_shape(st_cast(roads$geometry[10], 'POINT')) + tm_dots(col = "green") +
  tm_shape(grid$grid_v) + tm_lines(lty = "dotted") +
  tm_shape(grid$grid_h) + tm_lines(lty = "dotted") +
  tm_shape(grid$grid) + tm_lines()