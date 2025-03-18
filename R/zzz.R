#' Helper functions
#'
#' @noRd
#' 
globalVariables(c("Cover", "PA"))


#---- path helpers work

path_package <- function(...) {
    system.file(..., package = "SAVM", mustWork = TRUE)
}

path_model <- function(...) {
    system.file("extdata", "models", ..., package = "SAVM", mustWork = TRUE)
}


#---- message helpers
# see https://ropensci.org/blog/2024/02/06/verbosity-control-packages/

## alerts

sav_msg_info <- function(..., .envir = parent.frame()) {
    is_verbose_mode <- getOption("savm.verbose", "verbose") == "verbose"
    if (is_verbose_mode) {
        cli::cli_alert_info(..., .envir = .envir)
    } else {
        return()
    }
}

sav_msg_success <- function(..., .envir = parent.frame()) {
    is_verbose_mode <- getOption("savm.verbose", "verbose") == "verbose"
    if (is_verbose_mode) {
        cli::cli_alert_success(..., .envir = .envir)
    } else {
        return()
    }
}

sav_msg_warning <- function(..., .envir = parent.frame()) {
    is_verbose_mode <- getOption("savm.verbose", "verbose") == "verbose"
    if (is_verbose_mode) {
        cli::cli_alert_warning(..., .envir = .envir)
    } else {
        return()
    }
}

sav_msg_danger <- function(..., .envir = parent.frame()) {
    is_verbose_mode <- getOption("savm.verbose", "verbose") == "verbose"
    if (is_verbose_mode) {
        cli::cli_alert_danger(..., .envir = .envir)
    } else {
        return()
    }
}


##---- formal messages
sav_inform <- function(...) {
    is_verbose_mode <- (getOption("savm.verbose", "verbose") == "verbose")
    if (is_verbose_mode) {
        # Options local to this function only; reset on exit!
        rlang::local_options(rlib_message_verbosity = "verbose")
    }
    rlang::inform(...)
}

sav_warn <- function(...) {
    is_verbose_mode <- (getOption("savm.verbose", "verbose") == "verbose")
    if (is_verbose_mode) {
        rlang::local_options(rlib_message_verbosity = "verbose")
        rlang::warn(...)
    }
}

sav_debug_msg <- function(...) {
    is_debug_mode <- (getOption("savm.verbose", "verbose") == "debug")
    if (is_debug_mode) {
        rlang::local_options(rlib_message_verbosity = "verbose")
        rlang::inform(...) # or rlang::warn, cli::cli_alert_info, whatever
    }
}
