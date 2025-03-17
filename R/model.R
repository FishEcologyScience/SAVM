#' Load Random Forest models
#'
#' Load Random Forest models for SAV cover and presence absence.
#'
#' @param type {`character` either `"cover"` or `"pa"`}\cr{} Model type.
#' @param trained_on {`character` either `"depth"`, `"fetch"` or `"depth+fetch"`}\cr{}
#' Explanatory variable(s) used to train SAV cover and presence absence.
#'
#' @return
#' An object of class `randomForest`.
#'
#' @details
#' There are two sets of models available. The first set consists of models
#' predicting the presence or absence of SAV, while the second set focuses on
#' SAV cover. Each set includes three random forest models: one using depth as
#' a predictor, another using fetch, and a third combining both variables. For
#' further details, see Croft-White (2022).
#'
#' @references
#' * Croft-White, M.V., Tang, R., Gardner Costa, J., Doka, S.E., and Midwood, J.
#' D. 2022. Modelling submerged aquatic vegetation presence and percent cover
#' to support the development of a freshwater fish habitat management tool.
#' Can. Tech. Rep. Fish. Aquat. Sci. 3497: vi + 30 p.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' sav_load_model()
#' sav_load_model("pa", "depth+fetch")
#' }
#'

sav_load_model <- function(
    type = c("cover", "pa"),
    trained_on = c("depth", "fetch", "depth+fetch")) {
    type <- match.arg(type)
    trained_on <- match.arg(trained_on)
    path_model(paste0("sav_rf_", type, "_", trained_on, ".rds")) |>
        readRDS()
}


# sav_model <- function(d_inputs, type, cols = NULL) {
#     stopifnot(inherits(d_inputs, "data.frame"))
#     if (is.null(cols)) {
#         n_col <- names(d_inputs)
#     } else {
        
#         d_inputs  |>
#             dplyr::rename(cols)
#     }
# }

# sav_scrub <- function() {
#     cli::cli_inform("Scrub points by limitations")
# }

# sav_trim <- function() {
#     cli::cli_inform("Trim by depth")
# }