#' Assemble data for modeling
#'
#' Combines original point data with available fetch and depth results
#' to create a complete dataset for model application
#'
#' @param app_data Reactive values object containing all data
#' @return sf object with assembled data
#' @noRd
assemble_modeling_data <- function(app_data) {
  # Start with original data
  if (is.null(app_data$original_data)) {
    stop("No original data available", call. = FALSE)
  }

  base_data <- app_data$original_data$points

  # Add fetch results if available
  if (!is.null(app_data$fetch_results) && app_data$fetch_calculated) {
    base_data <- merge_fetch_data(base_data, app_data$fetch_results)
  }

  # Add depth results if available
  if (!is.null(app_data$depth_results) && app_data$depth_extracted) {
    base_data <- merge_depth_data(base_data, app_data$depth_results)
  }

  return(base_data)
}

#' Merge fetch calculation results
#'
#' Safely merges fetch calculation results with point data
#'
#' @param points_data sf object with original point data
#' @param fetch_results List containing fetch calculation results
#' @return sf object with fetch data merged
#' @noRd
merge_fetch_data <- function(points_data, fetch_results) {
  if (is.null(fetch_results$mean_fetch)) {
    warning("Fetch results missing mean_fetch data", call. = FALSE)
    return(points_data)
  }

  # Extract fetch columns we want to merge
  fetch_data <- fetch_results$mean_fetch |>
    sf::st_drop_geometry() |>
    dplyr::select(dplyr::any_of(c("id_point", "fetch_km", "weighted_fetch_km")))

  # Create id_point if it doesn't exist in original data
  if (!"id_point" %in% names(points_data)) {
    points_data$id_point <- seq_len(nrow(points_data))
  }

  # Check input data for presence of fetch
  if ("fetch_km" %in% colnames(points_data)) {
    points_data <- points_data |>
      dplyr::rename(fetch_km_ori = fetch_km)
  }

  # Merge fetch data
  result <- points_data |>
    dplyr::left_join(fetch_data, by = "id_point")

  return(result)
}

#' Merge depth extraction results
#'
#' Safely merges depth extraction results with point data
#'
#' @param points_data sf object with point data
#' @param depth_results List containing depth extraction results
#' @return sf object with depth data merged
#' @noRd
merge_depth_data <- function(points_data, depth_results) {
  if (is.null(depth_results$points_with_depth)) {
    warning("Depth results missing points_with_depth data", call. = FALSE)
    return(points_data)
  }

  # Extract depth column
  depth_data <- depth_results$points_with_depth |>
    sf::st_drop_geometry() |>
    dplyr::select(dplyr::any_of(c("id_point", "depth_m")))

  # Create id_point if it doesn't exist
  if (!"id_point" %in% names(points_data)) {
    points_data$id_point <- seq_len(nrow(points_data))
  }

  # Check input data for presence of depth
  if ("depth_m" %in% colnames(points_data)) {
    points_data <- points_data |>
      dplyr::rename(depth_m_ori = depth_m)
  }

  # Merge depth data
  result <- points_data |>
    dplyr::left_join(depth_data, by = "id_point")

  return(result)
}

#' Get available predictors
#'
#' Determines which predictors are available in the assembled data
#'
#' @param app_data Reactive values object
#' @return Character vector of available predictors
#' @noRd
get_available_predictors <- function(app_data) {
  predictors <- c()

  # Check original data for existing columns
  if (!is.null(app_data$original_data)) {
    original_cols <- names(app_data$original_data$points)
    if (any(grepl("^depth", original_cols, ignore.case = TRUE))) {
      predictors <- c(predictors, "depth")
    }
    if (any(grepl("^fetch", original_cols, ignore.case = TRUE))) {
      predictors <- c(predictors, "fetch")
    }
  }

  # Check calculated results
  if (app_data$fetch_calculated && !is.null(app_data$fetch_results)) {
    predictors <- c(predictors, "fetch")
  }

  if (app_data$depth_extracted && !is.null(app_data$depth_results)) {
    predictors <- c(predictors, "depth")
  }

  return(unique(predictors))
}

#' Get data summary
#'
#' Creates a summary of available data and calculations
#'
#' @param app_data Reactive values object
#' @return List with data summary information
#' @noRd
get_data_summary <- function(app_data) {
  summary_info <- list(
    original_loaded = !is.null(app_data$original_data),
    n_points = if (!is.null(app_data$original_data)) nrow(app_data$original_data$points) else 0,
    fetch_calculated = app_data$fetch_calculated,
    depth_extracted = app_data$depth_extracted,
    model_applied = app_data$model_applied,
    available_predictors = get_available_predictors(app_data),
    fetch_timestamp = app_data$fetch_timestamp,
    depth_timestamp = app_data$depth_timestamp,
    model_timestamp = app_data$model_timestamp
  )

  return(summary_info)
}

#' Validate data compatibility
#'
#' Checks if calculation results are compatible with original data
#'
#' @param app_data Reactive values object
#' @return List with validation results
#' @noRd
validate_data_compatibility <- function(app_data) {
  issues <- c()

  if (is.null(app_data$original_data)) {
    issues <- c(issues, "No original data loaded")
    return(list(valid = FALSE, issues = issues))
  }

  n_original <- nrow(app_data$original_data$points)

  # Check fetch results compatibility
  if (!is.null(app_data$fetch_results)) {
    n_fetch <- nrow(app_data$fetch_results$mean_fetch)
    if (n_fetch != n_original) {
      issues <- c(issues, sprintf("Fetch results have %d points, original has %d", n_fetch, n_original))
    }
  }

  # Check depth results compatibility
  if (!is.null(app_data$depth_results)) {
    n_depth <- nrow(app_data$depth_results$points_with_depth)
    if (n_depth != n_original) {
      issues <- c(issues, sprintf("Depth results have %d points, original has %d", n_depth, n_original))
    }
  }

  return(list(
    valid = length(issues) == 0,
    issues = issues
  ))
}

#' Clear calculation results
#'
#' Safely clears specific calculation results while preserving others
#'
#' @param app_data Reactive values object
#' @param clear_what Character vector of what to clear ("fetch", "depth", "model")
#' @noRd
clear_calculation_results <- function(app_data, clear_what = c("fetch", "depth", "model")) {
  if ("fetch" %in% clear_what) {
    app_data$fetch_results <- NULL
    app_data$fetch_params <- NULL
    app_data$fetch_timestamp <- NULL
    app_data$fetch_calculated <- FALSE
  }

  if ("depth" %in% clear_what) {
    app_data$depth_results <- NULL
    app_data$depth_params <- NULL
    app_data$depth_timestamp <- NULL
    app_data$depth_extracted <- FALSE
  }

  if ("model" %in% clear_what) {
    app_data$model_results <- NULL
    app_data$model_params <- NULL
    app_data$model_timestamp <- NULL
    app_data$model_applied <- FALSE
  }
}
