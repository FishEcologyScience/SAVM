#' Function title
#'
#' Function description
#'
#' @param x,y two numerics to be summed.
#'
#' @export
#'
#' @examples
#' foo(2, 3)
foo <- function(x, y = x) {
    cli::cli_alert_info("useful message")
    x + y + 1.0
}
