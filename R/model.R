#' Load Random Forest models
#'
#' Load Random Forest models for SAV cover and presence absence.
#'
#' @param d_inputs {`data frame`}\cr{} A data frame with all inputs.
#' @param type {`character` either `"cover"` or `"pa"`}\cr{} Model type.
#' @param depth,fetch,substrate,secchi Column specification, see details.
#' @return
#' A data frame  object of class `randomForest`.
#'
#' @details
#' There are two sets of models available. The first set consists of models
#' predicting the presence or absence of SAV, while the second set focuses on
#' SAV cover. Each set includes three random forest models: one using depth as
#' a predictor, another using fetch, and a third combining both variables. For
#' further details, see Croft-White (2022).
#'
#' The selected model for generating predictions is determined by the type 
#' argument, which specifies the output as either cover or presence-absence, 
#' depending on the available predictors. The required input variables—depth, 
#' fetch, substrate, and secchi—must correspond to column names in d_inputs;
#' otherwise, an error is thrown. If neither depth nor fetch is explicitly 
#' provided, the function will attempt to infer them from the column names. 
#' Matching is case-insensitive but must be exact. 
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
#' sav_model(data.frame(depth = c(5, 10)))
#' }
#'
sav_model <- function(d_inputs, type = c("cover", "pa"), depth = NULL, fetch = NULL, substrate = NULL, secchi = NULL) {
    stopifnot(inherits(d_inputs, "data.frame"))
    type <- match.arg(type)

    d_inputs <- d_inputs |>
        rename_if_valid(depth, "Depth") |>
        rename_if_valid(fetch, "Fetch") |>
        rename_if_valid(substrate, "Substrate") |>
        rename_if_valid(secchi, "SecchiDepth")

    if (is.null(depth) && is.null(fetch)) {
        sav_msg_info("Looking for depth and fetch in column names.")
        d_inputs <- d_inputs |>
            rename_if_present("depth", "Depth") |>
            rename_if_present("fetch", "Fetch")
        if (!any(c("Depth", "Fetch") %in% names(d_inputs))) {
            rlang::abort("Either depth or fetch or both must be defined.")
        } else {
            v_col <- c("Fetch", "Depth")[c("Fetch", "Depth") %in% names(d_inputs)]
            sav_msg_info("Found {v_col} in column names.")
        }
    }

    d_predict <- d_inputs[names(d_inputs) %in% c("Fetch", "Depth")]
    ind <- ("Depth" %in% names(d_inputs)) + ("Fetch" %in% names(d_inputs)) * 2
    predictors <- c("depth", "fetch", "depth+fetch")[ind]
    sav_msg_info("Using {type} with {predictors}")
    mod <- sav_load_model(type, predictors)

    out <- cbind(
        d_inputs,
        data.frame(
            Cover = stats::predict(mod, d_predict)
        )
    )
    if (type == "pa") {
        out <- out |>
            dplyr::rename(PA = Cover) |>
            # convert factor to 0/1
            dplyr::mutate(PA = as.character(PA) |> as.integer()) 
    }

    if (!is.null(substrate)) {
        out <- out |>
            dplyr::mutate(Substrate = d_inputs$Substrate)
    }
    if (!is.null(secchi)) {
        out <- out |>
            dplyr::mutate(
                SecchiDepth = (1.14 * log(d_inputs$SecchiDepth) + 1.32)^2
            )
    }

    out
}



sav_load_model <- function(
    type = c("cover", "pa"),
    predictors = c("depth", "fetch", "depth+fetch")) {
    type <- match.arg(type)
    predictors <- match.arg(predictors)
    path_model(paste0("sav_rf_", type, "_", predictors, ".rds")) |>
        readRDS()
}

# valid and rename
rename_if_valid <- function(.data, x, y) {
    if (!is.null(x)) {
        if (!x %in% names(.data)) {
            rlang::abort(paste0("`", x, "` is not a column of `d_inputs`."))
        } else {
            names(.data)[(names(.data) == x)[1L]] <- y
            .data
        }
    } else {
        .data
    }
}

rename_if_present <- function(.data, x, y) {
    # detect column name irrespectively of the case
    col_nm <- names(.data) |> tolower()
    out <- names(.data)[col_nm == x][1L] # take 1st if more than 1
    if (is.na(out)) {
        .data
    } else {
        names(.data)[col_nm == x][1L] <- y
        .data
    }
}
