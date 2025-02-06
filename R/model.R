sav_pa  <-  function() {
    cli::cli_inform("Apply SAV Presence/Absence")
}

sav_cover <- function() {
    cli::cli_inform("Apply SAV cover model")
}

sav_scrub <- function() {
    cli::cli_inform("Scrub points by limitations")
}

sav_trim <- function() {
    cli::cli_inform("Trim by depth")
}