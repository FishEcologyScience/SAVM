#' Helpers
#' @noRd 

path_package  <- function(...) {
    system.file(..., package = "SAVM", mustWork = TRUE)
}

path_model <- function(...) {
    system.file("extdata", "models", ..., package = "SAVM", mustWork = TRUE)
}