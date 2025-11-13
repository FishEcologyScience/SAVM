#' Apply SAV prediction models
#'
#' Apply SAV prediction models to predict SAV cover and presence/absence, with
#' optional post-hoc processing.
#'
#' @param dat {`data.frame`|`sf`}\cr{} A `data.frame` or `sf` object containing
#' depth and fetch data (required), and optionally secchi, substrate, and
#' limitation columns for post-hoc adjustments. Additional columns will be
#' ignored.
#' @param method_pa {`character`}\cr{} Statistical method for presence/absence
#' model. One of `"rf"` (Random Forest), `"gam"` (Generalized Additive Model),
#' or `"lmm"` (Linear Mixed Model). Default is `"rf"`.
#' @param method_cover {`character`}\cr{} Statistical method for cover model.
#' One of `"rf"` (Random Forest), `"gam"` (Generalized Additive Model), or
#' `"lmm"` (Linear Mixed Model). Default is the same as `method_pa`.
#' @param pa_threshold {`numeric`}\cr{} Probability threshold (0-1) for
#' converting presence/absence predictions to binary values. Values below the
#' threshold are classified as absent (0), values at or above as present (1).
#' Default is 0.5.
#' @param depth {`character` (required)}\cr{} Name of the column containing 
#' depth data (in meters). Default is `"depth"`.
#' @param fetch {`character` (required)}\cr{} Name of the column containing 
#' fetch data (in kilometers). Default is `"fetch"`.
#' @param substrate {`character` (optional)}\cr{} Name of the column containing 
#' substrate limitation data (optional). Binary indicator (0 = no limitation, 1 
#' = limited) for substrate constraints. Default is `"substrate"`. Set to 
#' `NULL` to disable substrate-based post-hoc adjustments.
#' @param secchi {`character` (optional)}\cr{} Name of the column containing 
#' Secchi depth data in meters (optional). Used to calculate maximum 
#' colonization depth (Vmax) via the Chambers and Kalff equation. Default is 
#' `"secchi"`. Set to `NULL` to disable Vmax calculations.
#' @param limitation {`character` (optional)}\cr{} Name of the column 
#' containing user-supplied limitation data (optional). Binary indicator (0 = 
#' no limitation, 1 = limited) for additional constraints. Default is 
#' `"limitation"`. Set to `NULL` to disable user-supplied limitation 
#' adjustments.
#' @param vmax_par {`named list` (required `secchi`)}\cr{} Named list with 
#' `intercept` and `slope` parameters for the Chambers and Kalff (1985) 
#' equation to compute maximum depth of plant colonization (Vmax). Default is 
#' `list(intercept = 1.40, slope = 1.33)` (Model A: Quebec + international 
#' lakes). See *Details* for Model B parameters.
#'
#' @return
#' A data frame (or `sf` object) containing the input columns along with model
#' predictions. Column names are standardized to `depth_m` and `fetch_km`.
#'
#' The following prediction columns are always returned:
#' * `pa_pred`: Raw presence/absence probability predictions (0-1).
#' * `pa`: Binary presence/absence classification based on `pa_threshold`.
#' * `cover_pred`: Raw cover predictions (percent, 0-100).
#' * `cover`: Cover predictions adjusted by presence/absence (0 if absent).
#' * `pa_post_hoc`: Presence/absence after post-hoc treatment (0 if any limitation).
#' * `cover_post_hoc`: Cover after post-hoc treatment (0 if any limitation).
#'
#' If Secchi depth data is provided, two additional columns are returned:
#' * `vmax`: Maximum colonization depth calculated via Chambers & Kalff equation.
#' * `limitation_secchi`: Logical indicating light limitation (`TRUE` if depth exceeds vmax).
#'
#' @details
#' # Statistical Methods
#'
#' The function applies two types of models: one for predicting presence/absence
#' of SAV and another for predicting SAV cover. Three statistical methods are
#' available:
#' * **Random Forest (`"rf"`)**: Non-parametric ensemble method, robust to
#'   non-linear relationships. Best for complex patterns.
#' * **Generalized Additive Models (`"gam"`)**: Semi-parametric method with
#'   smooth functions. Good balance between interpretability and flexibility.
#' * **Linear Mixed Models (`"lmm"`)**: Parametric approach accounting for
#'   hierarchical data structure. Useful for grouped data.
#'
#' You can specify different methods for presence/absence and cover predictions
#' using the `method_pa` and `method_cover` parameters. For further details about
#' the models, see Croft-White et al. (2022).
#'
#' # Column Specification
#'
#' **Required predictors (depth and fetch)**: Both depth and fetch data are
#' mandatory. By default, the function looks for columns named `"depth"` and
#' `"fetch"`. If these columns are not found, the function will attempt to
#' auto-detect columns using case-insensitive matching for 'depth_m', 'depth',
#' 'fetch_km', or 'fetch'. If columns cannot be found, an error is thrown. To
#' use custom column names, explicitly specify them via the `depth` and `fetch`
#' parameters.
#'
#' **Optional predictors (substrate, secchi, limitation)**: These columns are
#' optional for post-hoc adjustments. By default, the function looks for columns
#' with these exact names. If a column is not found in the data, that post-hoc
#' adjustment is skipped. To use custom column names, explicitly specify them
#' via the respective parameters. To explicitly disable a post-hoc adjustment
#' even if the column exists, set the parameter to `NULL`.
#'
#' # Post-hoc Treatment
#'
#' Post-hoc adjustments refine raw predictions by setting them to 0 wherever
#' limitations are detected:
#' * **substrate**: Substrate limitation (e.g., unsuitable bottom substrate).
#' * **limitation**: User-supplied limitation data.
#' * **limitation_secchi**: Light limitation calculated from Secchi depth.
#'
#' These columns are treated as binary indicators: any value > 0 indicates a
#' limiting condition. When limitations are present, both `pa_post_hoc` and
#' `cover_post_hoc` are set to 0.
#'
#' # Vmax Calculation
#'
#' When Secchi depth data is provided, maximum colonization depth (Vmax) is
#' calculated using the Chambers and Kalff (1985) equation. The `vmax_par`
#' parameter controls the regression coefficients:
#' * **Model A** (default): `list(intercept = 1.40, slope = 1.33)` - Quebec +
#'   international lakes
#' * **Model B**: `list(intercept = 1.32, slope = 1.14)` - Quebec lakes only
#'
#' Sites where depth exceeds Vmax are flagged as light-limited via the
#' `limitation_secchi` column.
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
#' # Basic usage with auto-detection of column names
#' sav_model(data.frame(depth = c(5, 10), fetch = c(1, 2)))
#'
#' # Using different methods for PA and cover
#' sav_model(
#'   data.frame(depth_m = c(5, 10), fetch_km = c(1, 2)),
#'   method_pa = "rf",
#'   method_cover = "gam"
#' )
#'
#' # Adjusting PA threshold
#' sav_model(
#'   data.frame(depth = c(5, 10), fetch = c(1, 2)),
#'   pa_threshold = 0.7  # More conservative predictions
#' )
#'
#' # Using post-hoc treatment with Secchi and substrate data
#' sav_model(
#'   dat = data.frame(
#'     depth = c(5, 10, 5),
#'     fetch = c(1, 2, 10),
#'     secchi = c(1, 10, 10),
#'     substrate = c(1, 1, 0)
#'   )
#' )
#'
#' # Using custom column names
#' sav_model(
#'   dat = data.frame(
#'     water_depth = c(5, 10),
#'     wave_fetch = c(1, 2)
#'   ),
#'   depth = "water_depth",
#'   fetch = "wave_fetch"
#' )
#'
#' # Disabling post-hoc adjustments by setting to NULL
#' sav_model(
#'   dat = data.frame(
#'     depth = c(5, 10),
#'     fetch = c(1, 2),
#'     substrate = c(1, 0)  # substrate column exists but will be ignored
#'   ),
#'   substrate = NULL  # explicitly disable substrate adjustments
#' )
#'
#' # Using Model B Vmax parameters (Quebec lakes only)
#' sav_model(
#'   dat = data.frame(depth = c(5, 10), fetch = c(1, 2), secchi = c(2, 3)),
#'   vmax_par = list(intercept = 1.32, slope = 1.14)
#' )
#' }
sav_model <- function(
  dat, method_pa = "rf", method_cover = method_pa, pa_threshold = 0.5,
  depth = "depth", fetch = "fetch", substrate = "substrate", secchi = "secchi",
  limitation = "limitation", vmax_par = list(intercept = 1.40, slope = 1.33)
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

  # MAIN PREDICTORS 
  # using upper case for fetch and depth for consistency with model predictors
  dat <- dat |>
    rename_if_present(fetch, "Fetch") |>
    rename_if_present(depth, "Depth")

  if (!"Fetch" %in% colnames(dat)) {
    # extra search
    dat <- dat |> rename_if_present("^Fetch(_km)?$", "Fetch")
    if (!"Fetch" %in% colnames(dat)) {
      rlang::abort("`fetch` must point to an existing column in `dat.")
    }
  }
  if (!"Depth" %in% colnames(dat)) {
    dat <- dat |> rename_if_present("^depth(_m)?$", "Depth")
    if (!"column" %in% colnames(dat)) {
      rlang::abort("`depth` must point to an existing column in `dat`.")
    }
  }

  # POST_HOC PREDICTOR
  pht_col <- c(substrate, secchi, limitation)
  pht_col <- pht_col[pht_col %in% names(dat)]
  if (length(pht_col)) {
    sav_msg_info("Using {cli::col_green(pht_col)} for post-hoc treatment.")
    out <- dat[c("Depth", "Fetch", pht_col)] |>
      rename_if_present(substrate, "substrate") |>
      rename_if_present(secchi, "secchi") |>
      rename_if_present(limitation, "limitation")
  } else {
    sav_msg_warning("No column available for post-hoc treatment.")
    out <- dat[c("Depth", "Fetch")]
  }
  rownames(out) <- NULL
  d_predict <- dat[c("Depth", "Fetch")]
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
    out$limitation_secchi <- out$vmax > out$depth
    out <- out |>
      dplyr::relocate(limitation_secchi, .after = secchi)
  }

  out$pa_post_hoc <- out$pa
  out$cover_post_hoc <- out$cover
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
