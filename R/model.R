#' Apply Random Forest models
#'
#' Apply Random Forest models to predict SAV cover and presence/absence, with
#' optional post-hoc processing.
#'
#' @param dat {`data.frame`}\cr{} A `data.frame` containing some or all of the
#' following columns:
#'   - `depth_m`: Numeric, depth in meters.
#'   - `fetch_km`: Numeric, fetch in kilometers.
#'   - `secchi`: Numeric Secchi depth in meters (post_hoc)
#'   - `substrate`: Binary (0 = absent, 1 = present), indicating substrate
#'      limitations. (post_hoc)
#'   - `limitation`: Binary (0 = absent, 1 = present), indicating user-supplied
#'      limitations.
#'  Additionnal columns will be ignored.
#' @param type {`character vector`, either `"cover"` or `"pa"`}\cr{}
#' Model type(s).
#' @param depth,fetch {`character`} Column specification for the predictors, 
#' see *Details*.
#' @param substrate,secchi,limitation Column specification for post_hoc 
#' variables, see *Details*.
#' @param vmax_par {`named list`}\cr{} intercept and slope of the equation from
#' Chambers and Kalff (1985) to compute the maximum depth of plant colonization
#' (Vmax), see *Details* below.
#'
#' @return
#' A data frame containing the input columns along with model predictions.
#'
#' The prediction column names match the values specified in `type`, and contain
#' the raw model outputs (i.e., without post-hoc adjustment).
#'
#' Post-hoc adjusted predictions (see *Details*) are included in additional 
#' columns with the same names as the `type` values, but with the suffix 
#' `_post_hoc`.
#' 
#' If a column `secchi` is present, then two additionnal columns are 
#' returned: `vmax` and `limitation_secchi`, see details for further 
#' explanation.
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
#' fetch, substrate, secchi, limitation—must correspond to column names in
#' `dat`; otherwise, an error is thrown. If neither 'depth' nor 'fetch' is
#' explicitly provided, the function will attempt to infer them from the column
#' names. Matching is case-insensitive and will detect 'depth_m', 'depth',
#' 'fetch_km' and 'fetch'.
#' 
#' If `secchi` is provided, two additional columns are returned:
#' * `vmax`: Predicted maximum colonization depth calculated using the Chambers 
#' and Kalff equation.
#' * `limitation_secchi`: Logical column indicating light limitation. `TRUE` if 
#' `vmax >= depth_m`,  indicating the site is light-limited; `FALSE` otherwise.
#'
#' The regression parameters for the Chambers and Kalff equation can be 
#' adjusted via the `vmax_par` argument. By default, parameters from Model A 
#' (Quebec and international lakes) in Croft-White et al. (2022) are used. To 
#' apply Model B (Quebec lakes only), use:
#'
#' `vmax_par = list(intercept = 1.32, slope = 1.14)`
#' 
#' The post-hoc treatment adjusts raw predictions by setting them to 0 wherever 
#' limitations are present. Possible limitation columns include `substrate`, 
#' `limitation`, and `limitation_secchi` (see descriptions above). These 
#' columns are treated as binary indicators: any value greater than 0 is 
#' interpreted as a limiting condition. If a limitation is detected, the 
#' corresponding prediction is set to 0 in the post-hoc adjusted output.
#' 
#' 
#' @references
#' * Croft-White, M.V., Tang, R., Gardner Costa, J., Doka, S.E., and Midwood, J.
#' D. 2022. Modelling submerged aquatic vegetation presence and percent cover
#' to support the development of a freshwater fish habitat management tool.
#' Can. Tech. Rep. Fish. Aquat. Sci. 3497: vi + 30 p.
#' * Chambers, P.A., and Kalff, J. 1985. Depth Distribution and Biomass of
#' Submerged aquatic macrophyte communities in relation to secchi depth.
#' Can. J. Fish. Aquat. Sci. 42: 701–709
#'
#' @export
#'
#' @examples
#' \donttest{
#' 
#' # basic usage 
#' sav_model(data.frame(depth = c(5, 10)))
#' sav_model(data.frame(depth = c(5, 10), fetch = c(1, 2)), type = "pa")
#' 
#' # using post-hoc tretment 
#' sav_model(
#'  data.frame(
#'   depth = c(5, 10, 5), 
#'   fetch = c(1, 2, 10), 
#'   secchi = c(1, 10, 10), 
#'   substrate = c(TRUE, TRUE, FALSE)
#'  )
#' )
#' }
sav_model <- function(
    dat, type = c("cover", "pa"), depth = NULL,
    fetch = NULL, substrate = NULL, secchi = NULL, limitation = NULL,
    vmax_par = list(intercept = 1.40, slope = 1.33)) {
    sav_stop_if_not(inherits(dat, "data.frame"))

    type <- unique(type)
    if (!all(type %in% c("cover", "pa"))) {
        rlang::abort("`type` value(s) must be 'cover' or 'pa'.")
    }

    # names for the rf models change back towards the end
    main_col_names <- c("Fetch", "Depth")

    dat <- dat |>
        rename_if_valid(fetch, main_col_names[1]) |>
        rename_if_valid(depth, main_col_names[2]) |>
        rename_if_valid(substrate, "substrate") |>
        rename_if_valid(secchi, "secchi") |>
        rename_if_valid(limitation, "limitation")

    if (is.null(depth) && is.null(fetch)) {
        sav_msg_info("Looking for depth and fetch in column names.")
        dat <- dat |>
            rename_if_present("^depth(_m)?$", "Depth") |>
            rename_if_present("^fetch(_km)?$", "Fetch")
        if (!any(main_col_names %in% names(dat))) {
            rlang::abort("Either depth or fetch or both must be defined.")
        } else {
            v_col <- main_col_names[main_col_names %in% names(dat)]
            sav_msg_info("Found {v_col} in column names.")
        }
    }

    dat <- dat[
        names(dat) %in% c(main_col_names, "substrate", "secchi", "limitation")
    ]
    d_predict <- dat[names(dat) %in% main_col_names]
    ind <- ("Depth" %in% names(dat)) + ("Fetch" %in% names(dat)) * 2
    predictors <- c("depth", "fetch", "depth+fetch")[ind]
    sav_msg_info("Using {type} with {predictors}")

    out <- dat
    rownames(out) <- NULL
    if ("pa" %in% type) {
        out$pa <- stats::predict(
            sav_load_model("pa", predictors),
            d_predict
        ) |>
            as.character() |>
            as.integer()
    }
    if ("cover" %in% type) {
        out$cover <- stats::predict(
            sav_load_model("cover", predictors),
            d_predict
        )
    }

    out <- out |>
        rename_if_present("^depth$", "depth_m") |>
        rename_if_present("^fetch$", "fetch_km")


    # Post-hoc

    if ("secchi" %in% names(out)) {
        out$vmax <- (vmax_par$slope * log(out$secchi) + vmax_par$slope)^2
        out <- out |>
            dplyr::relocate(vmax, .after = secchi)
        # create v_max limitation
        if ("depth_m" %in% names(out)) {
            out$limitation_secchi <- out$vmax > out$depth
            out <- out |>
                dplyr::relocate(limitation_secchi, .after = secchi)
        } else {
            sav_warn(
                "A column with depth data required to perform the post-hoc
                treatment with secchi depth."
            )
        }
    }

    if ("pa" %in% names(out)) {
        out$pa_post_hoc <- out$pa
        out <- out |>
            scrub_if_present("limitation", "pa_post_hoc") |>
            scrub_if_present("substrate", "pa_post_hoc") |>
            scrub_if_present("limitation_secchi", "pa_post_hoc")
    }

    if ("cover" %in% names(out)) {
        out$cover_post_hoc <- out$cover
        out <- out |>
            scrub_if_present("pa", "cover_post_hoc") |>
            scrub_if_present("limitation", "cover_post_hoc") |>
            scrub_if_present("substrate", "cover_post_hoc") |>
            scrub_if_present("limitation_secchi", "cover_post_hoc")
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
            rlang::abort(paste0("`", x, "` is not a column of `dat`."))
        } else {
            names(.data)[which(names(.data) == x)[1L]] <- y
        }
    }
    .data
}

rename_if_present <- function(.data, x, y) {
    # detect column name irrespectively of the case
    col_nm <- names(.data) |> tolower()
    out <- names(.data)[grepl(x, col_nm)][1L] # take 1st if more than 1
    if (!is.na(out)) {
        names(.data)[grepl(x, col_nm)][1L] <- y
    }
    .data
}

# with binary only
scrub_if_present <- function(.data, x, y) {
    if (x %in% names(.data)) {
        .data[[y]] <- .data[[y]] * (.data[[x]] > 0) # force binary
    }
    .data
}