#' Apply SAV prediction models
#'
#' Apply SAV prediction models to predict SAV cover and presence/absence, with
#' optional post-hoc processing.
#'
#' @param dat {`data.frame`|`sf`}\cr{} A `data.frame` or a `sf` object 
#' containing some or all of the following columns:
#'   - `depth_m`: Numeric, depth in meters.
#'   - `fetch_km`: Numeric, fetch in kilometers.
#'   - `secchi`: Numeric Secchi depth in meters (post_hoc)
#'   - `substrate`: Binary (0 = absent, 1 = present), indicating substrate
#'      limitations. (post_hoc)
#'   - `limitation`: Binary (0 = absent, 1 = present), indicating user-supplied
#'      limitations.
#'  Additional columns will be ignored.
#' @param method_pa {`character`}\cr{} Statistical method for presence/absence
#' model. One of `"rf"` (random forest), `"gam"` (Generalized Additive Model),
#' or `"lmm"` (Linear Mixed Model). Default is `"rf"`.
#' @param method_cover {`character`}\cr{} Statistical method for cover model.
#' One of `"rf"` (random forest), `"gam"` (Generalized Additive Model), or
#' `"lmm"` (Linear Mixed Model). Default is the same as `method_pa`.
#' @param pa_threshold {`numeric`}\cr{} Probability threshold for converting
#' presence/absence predictions to binary values. Default is 0.5.
#' @param depth,fetch {`character`}\cr{} Column specification for the 
#' predictors, see *Details*.
#' @param substrate,secchi,limitation {`character`}\cr{}Column specification 
#' for post_hoc variables, see *Details*.
#' @param vmax_par {`named list`}\cr{} intercept and slope of the equation from
#' Chambers and Kalff (1985) to compute the maximum depth of plant colonization
#' (Vmax), see *Details* below.
#'
#' @return
#' A data frame (or a sf object) containing the input columns along with model
#' predictions.
#'
#' The following prediction columns are returned:
#' * `pa_pred`: Raw presence/absence probability predictions (0-1).
#' * `pa`: Binary presence/absence classification based on `pa_threshold`.
#' * `cover_pred`: Raw cover predictions (percent).
#' * `cover`: Cover predictions adjusted by presence/absence classification.
#' * `pa_post_hoc`: Presence/absence after post-hoc treatment (accounting for limitations).
#' * `cover_post_hoc`: Cover after post-hoc treatment (accounting for limitations).
#'
#' If a column `secchi` is present, then two additional columns are
#' returned: `vmax` and `limitation_secchi`, see details for further
#' explanation.
#'
#' @details
#' The function applies two types of models: one for predicting presence/absence
#' of SAV and another for predicting SAV cover. Three statistical methods are
#' available: Random Forest (`"rf"`), Generalized Additive Models (`"gam"`), and
#' Linear Mixed Models (`"lmm"`). You can specify different methods for
#' presence/absence and cover predictions using the `method_pa` and `method_cover`
#' parameters. For further details about the models, see Croft-White et al. (2022).
#'
#' The required input variables—depth, fetch, substrate, secchi, limitation—must
#' correspond to column names in `dat`; otherwise, an error is thrown. If neither
#' 'depth' nor 'fetch' is explicitly provided, the function will attempt to infer
#' them from the column names. Matching is case-insensitive and will detect
#' 'depth_m', 'depth', 'fetch_km' and 'fetch'.
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
#' # basic usage
#' sav_model(data.frame(depth = c(5, 10), fetch = c(1, 2)))
#' sav_model(
#'   data.frame(depth = c(5, 10), fetch = c(1, 2)),
#'   method_pa = "lmm"
#' )
#' # using post-hoc treatment
#' sav_model(
#'   dat = data.frame(
#'     depth = c(5, 10, 5),
#'     fetch = c(1, 2, 10),
#'     secchi = c(1, 10, 10),
#'     substrate = c(TRUE, TRUE, FALSE)
#'   )
#' )
#' }
sav_model <- function(
  dat, method_pa = "rf", method_cover = method_pa, pa_threshold = 0.5,
  depth = NULL, fetch = NULL, substrate = NULL, secchi = NULL,
  limitation = NULL, vmax_par = list(intercept = 1.40, slope = 1.33)
) {
  method_pa <- match.arg(method_pa, c("rf", "lmm", "gam"))
  method_cover <- match.arg(method_cover, c("rf", "lmm", "gam"))

  geom <- NULL
  if (inherits(dat, "sf")) {
    geom <- dat |>
      dplyr::select(geometry)
    dat <- dat |>
      sf::st_drop_geometry()
  } else {
    sav_stop_if_not(inherits(dat, "data.frame"))
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
    if (!all(main_col_names %in% names(dat))) {
      rlang::abort("Both depth and fetch must be defined.")
    } else {
      v_col <- main_col_names[main_col_names %in% names(dat)]
    }
  }

  dat <- dat[
    names(dat) %in% c(main_col_names, "substrate", "secchi", "limitation")
  ]
  d_predict <- dat[names(dat) %in% main_col_names]
  ind <- ("Depth" %in% names(dat)) + ("Fetch" %in% names(dat)) * 2

  out <- dat
  rownames(out) <- NULL
  # PA
  ## NB predict() does some magic behind the scenes to find the actual function
  pa_mod <- sav_load_model("pa", method_pa)
  if (method_pa == "rf") {
    tmp <- stats::predict(pa_mod, d_predict, type = "prob")
    out$pa_pred <- tmp[, colnames(tmp) == "1"]
  }
  if (method_pa == "gam") {
    # use logit function
    out$pa_pred <- mgcv::predict.gam(pa_mod, d_predict) |>
      inv_logit()
  }
  if (method_pa == "lmm") {
    out$pa_pred <- stats::predict(pa_mod, d_predict, re.form = NA)
    out$pa_pred[out$pa_pred > 1] <- 1
    out$pa_pred[out$pa_pred < 0] <- 0
  }
  out$pa <- (out$pa_pred > pa_threshold) * 1

  # COVER
  cover_mod <- sav_load_model("cover", method_cover)
  if (method_cover == "rf") {
    out$cover_pred <- stats::predict(cover_mod, d_predict)
  }
  if (method_cover == "lmm") {
    out$cover_pred <- stats::predict(cover_mod, d_predict, re.form = NA)
    out$cover_pred[out$cover > 100] <- 100
    out$cover_pred[out$cover < 0] <- 0
  }
  if (method_cover == "gam") {
    out$cover_pred <- mgcv::predict.gam(cover_mod, d_predict)
    out$cover_pred <- inv_logit(out$cover_pred) * 100
  }
  out$cover <- out$cover_pred * out$pa

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
        "A column with depth data required to perform the post-hoc treatment
        with secchi depth."
      )
    }
  }

  pht_col <- intersect(c("secchi", "limitation", "substrate"), names(out))
  out$pa_post_hoc <- out$pa
  out$cover_post_hoc <- out$cover
  if (length(pht_col)) {
    sav_msg_info("Using {pht_col} for post-hoc treatment.")
  } else {
    sav_msg_warning("No column available for post-hoc treatment.")
  }

  out <- out |>
    scrub_if_present("limitation", "pa_post_hoc") |>
    scrub_if_present("substrate", "pa_post_hoc") |>
    scrub_if_present("limitation_secchi", "pa_post_hoc") |>
    scrub_if_present("limitation", "cover_post_hoc") |>
    scrub_if_present("substrate", "cover_post_hoc") |>
    scrub_if_present("limitation_secchi", "cover_post_hoc")


  if (is.null(geom)) {
    out
  } else {
    cbind(geom, out)
  }
}


#' Load SAV Model
#'
#' Load a pre-trained SAV model for predicting cover or presence/absence.
#'
#' @param type {`character`}\cr{} Model type, either `"cover"` or `"pa"` (presence/absence).
#' @param method {`character`}\cr{} Modeling method: `"rf"` (random forest), `"lmm"`, or `"gam"`.
#'
#' @return A model object (e.g., randomForest, glmm, or gam object) that can be used for predictions.
#'
#' @details
#' This function loads pre-trained models from the package's internal data
#' directory. Models are available for both presence/absence (`"pa"`) and cover
#' prediction (`"cover"`), using three different statistical methods: Random
#' Forest (`"rf"`), Linear Mixed Models (`"lmm"`), and Generalized Additive
#' Models (`"gam"`). All models use both depth and fetch as predictors.
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Load a random forest model for presence/absence
#' model <- sav_load_model("pa", "rf")
#'
#' # Load a cover model using Linear Mixed Model
#' model <- sav_load_model("cover", "lmm")
#'
#' # Load a GAM model for cover
#' model <- sav_load_model("cover", "gam")
#' }
sav_load_model <- function(
  type = c("cover", "pa"),
  method = "rf"
) {
  type <- match.arg(type)
  method <- match.arg(method, c("rf", "lmm", "gam"))
  path <- path_model(paste0(method, "_", type, ".rds"))
  path |> readRDS()
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

inv_logit <- function(x) {
  out <- exp(x) / (1 + exp(x))
  as.vector(out)
}
