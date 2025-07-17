#' Preview grid
#'
#' @param x an object of class `sav_data`.
#'
#' @export
#'
preview_grid <- function(x) {
    UseMethod("preview_grid")
}

#' @describeIn preview_grid  Preview spatail grid.
#'
#' @export
#'
preview_grid.sav_data <- function(x) {
    plot(x$polygon |> sf::st_geometry(), border = 1)
    plot(x$points |> sf::st_geometry(), col = "grey50", pch = 19, add = TRUE)
}