#' Add the shared OpenStreetMap basemap
#'
#' @noRd
add_basemap <- function(map) {
  leaflet::addTiles(
    map,
    urlTemplate = "https://tile.openstreetmap.org/{z}/{x}/{y}.png",
    attribution = paste0(
      '&copy; <a href="https://www.openstreetmap.org/copyright">',
      'OpenStreetMap</a> contributors'
    ),
    options = leaflet::tileOptions(maxZoom = 19)
  )
}
