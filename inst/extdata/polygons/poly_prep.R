# Get data from:
# https://hub.glahf.org/datasets/e1489a48819e4db0a252262ed56afbf3_0/explore?location=44.424227%2C-82.856544%2C5.30
library(sf)
library(dplyr)
shore <- sf::st_read(
  "inst/extdata/polygons/Great_Lakes_High_Resolution_Shorelines_/Great_Lakes_High_Resolution_Shorelines_.shp"
)

manip <- function(lake, simple) {
  spat <- shore |>
    dplyr::filter(GNIS_Name == lake) |>
    st_simplify(FALSE, dTolerance = simple)

  nm <- stringr::str_replace(lake, " ", "_") |>
    tolower() |>
    paste0(".gpkg")
  sf::st_write(spat, file.path("inst/extdata/polygons/", nm), append = FALSE)

  spat
}

erie <- manip("Lake Erie", 5)
huron <- manip("Lake Huron", 45)
michigan <- manip("Lake Michigan", 2.5)
ontario <- manip("Lake Ontario", 10)
superior <- manip("Lake Superior", 13)
