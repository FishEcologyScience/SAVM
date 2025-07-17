#' Invert a Polygon
#' 
#' @param polygon {`sf`}\cr{}
#' Polygon defining land boundaries that needs to be inverted.
#' @param ratio {`numeric [0-1]`}\cr{}
#' Fraction convex, see [sf::st_concave_hull()]. This may need to be tweaked 
#' depending on the form of the polygon to be inverted.
#' 
#' @details 
#' Utility function that inverts a polygon by drawing a concave hull around 
#' `polygon` (see [sf::st_concave_hull()]) and then computes the differences
#' between the polygon and the concave hull (see [sf::st_concave_hull()]).
#'
#' @export 
#' 
#' @examples 
#' \donttest{
#' erie_land <- system.file(
#'  "example", "lake_erie_land", "LkErie_Land_fromGLAF_Water_WGS_Feb2020.shx",
#'   package = "SAVM", mustWork = TRUE
#' ) |> sf::st_read()
#'
#' erie_land |>
#'    sf::st_geometry() |>
#'    plot(col = 1)
#'
#' erie_land |>
#'    sf::st_geometry() |>
#'    invert_polygon() |>
#'    plot(col = 1)
#' }

invert_polygon <- function(polygon, ratio = 0.5) {
    os2 <- sf::sf_use_s2()
    on.exit(suppressMessages(sf::sf_use_s2(os2)))
    suppressMessages(os2 <- sf::sf_use_s2(FALSE))
    sf::st_difference(polygon |> sf::st_concave_hull(ratio = ratio), polygon)
}