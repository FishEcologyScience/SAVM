#' Compile data
#'
#' @export

sav_check_points <- function() {
    cli::cli_inform("Check points")
}

sav_compute_fetch <- function(wind_weighted = FALSE) {
        cli::cli_inform("Calculate fetch")
        cli::cli_inform("Calculate wind-weighted fetch")
}

sav_extract_depth <- function(wind_weighted = FALSE) {
    cli::cli_inform("Extract depth")
}