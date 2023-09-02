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
  stars::st_as_stars(raster) |> st_as_sf() |> st_cast("POINT") |> tibble::rownames_to_column() |>
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
  sample_n(500)

##### Make grid

# Make raster
nrows = 20
ncols = 20
raster <- terra::rast(nrows = nrows, ncols = ncols, terra::ext(roads), vals = 1)

# Grid from raster
grid <- raster_to_grid(raster)

##### 1. segmentize
time_st_segmentize = system.time({
  roads_segmented <- st_as_sf(roads, wkt = "geometry", crs = "EPSG:4326") |>
  sf::st_segmentize(dfMaxLength = 10)
})


##### 2. st_split
time_st_split <- system.time({roads_splitted <- lwgeom::st_split(tibble::rowid_to_column(roads), grid)|>
  sf::st_collection_extract("LINESTRING") |>
  group_by(rowid) |> summarise(sum = mean(rowid)) |> st_cast("LINESTRING")})


##### 3. split_lines
split_lines <- function(roads, blades) {
  blades <- collapse::rsplit(blades, 1:nrow(blades)) # data.frame to list
  tmp <- purrr::map(blades,\(x) lwgeom::st_split(tibble::rowid_to_column(roads), x)) |> # split roads line by line
    purrr::list_rbind() |> # resulting list to data.frame
    sf::st_as_sf() |> # data.frame to sf object
    sf::st_collection_extract("LINESTRING") |>
    group_by(rowid) |> summarise(sum = mean(rowid)) |> st_cast("MULTILINESTRING") # splitted road segments back to one road
}

time_split_lines <- system.time({roads_split_lines <- split_lines(roads, grid)})

##### 4. split_lines2
split_lines2 <- function(x, blades) {
  x <- tibble::rowid_to_column(x)
  crosses <- st_filter(x, blades, .predicate = st_crosses)
  others <- collapse::fsubset(x, !rowid %in% (crosses$rowid))
  splitted <- lwgeom::st_split(crosses, blades)
  rbind(splitted, others) |> sf::st_collection_extract("LINESTRING")
}

time_split_lines2 <- system.time({roads_split_lines2 <- split_lines2(roads, grid)})

rbind(time_st_segmentize, time_st_split, time_split_lines, time_split_lines2)

##### Visual verification

library(tmap)
tmap_mode("view")

tm_shape(st_cast(roads_splitted$geometry, 'POINT')) + tm_dots(col = "red", scale = 4) +
  tm_shape(st_cast(roads_split_lines2$geometry, 'POINT')) + tm_dots(col = "blue", scale = 2) +
  tm_shape(st_cast(roads$geometry, 'POINT')) + tm_dots(col = "green") +
  tm_shape(roads$geometry) + tm_lines(col = "green") +
  tm_shape(grid) + tm_lines(lty = "dotted")
