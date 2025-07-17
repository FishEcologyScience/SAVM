# --------- plot_sav_distribution
# Sample dataset for testing
withr::with_seed(123, {
  test_data <- data.frame(
    depth_m = runif(100, 0, 15),
    fetch_km = runif(100, 0, 15),
    pa_pred = sample(0:1, 100, replace = TRUE),
    cover_pred = runif(100, 0, 100),
    pa_post_hoc = sample(0:1, 100, replace = TRUE),
    cover_post_hoc = runif(100, 0, 100)
  )
})

test_that("Function runs without errors for default parameters", {
  pdf(NULL)
  expect_silent(plot_sav_distribution(test_data))
  dev.off()
})

test_that("Function runs with only PA plots", {
  pdf(NULL)
  expect_silent(plot_sav_distribution(test_data, type = "pa"))
  dev.off()
})

test_that("Function runs with only Cover plots", {
  pdf(NULL)
  expect_silent(plot_sav_distribution(test_data, type = "cover"))
  dev.off()
})

test_that("Function runs with Fetch predictor only", {
  pdf(NULL)
  expect_silent(plot_sav_distribution(test_data, predictors = "fetch"))
  dev.off()
})

test_that("Function runs with Depth predictor only", {
  pdf(NULL)
  expect_silent(plot_sav_distribution(test_data, predictors = "depth"))
  dev.off()
})

test_that("Function runs with post-hoc data", {
  pdf(NULL)
  expect_silent(plot_sav_distribution(test_data, post_hoc = TRUE))
  dev.off()
})

test_that("Function errors when required columns are missing", {
  incomplete_data <- test_data[, !names(test_data) %in% "depth_m"]
  pdf(NULL)
  expect_error(plot_sav_distribution(incomplete_data), "Requested layer `depth` is unavailable in provided data")
  dev.off()
})

test_that("Function handles empty dataset without error", {
  empty_data <- data.frame()
  pdf(NULL)
  expect_error(plot_sav_distribution(empty_data), "Requested post-hoc*")
  dev.off()
})

test_that("Function respects max_depth and max_fetch", {
  pdf(NULL)
  expect_silent(plot_sav_distribution(test_data, max_depth = 10, max_fetch = 5))
  dev.off()
})

test_that("Output is a ggplot object", {
  pdf(NULL)
  plots <- plot_sav_distribution(test_data)
  expect_true(inherits(plots, "ggplot"))
  dev.off()
})

test_that("Function errors if post_hoc columns are missing", {
  bad_data <- test_data[, !names(test_data) %in% c("pa_post_hoc", "cover_post_hoc")]
  expect_error(
    plot_sav_distribution(bad_data, post_hoc = TRUE),
    "Requested post-hoc.*missing"
  )
})

test_that("Function works with only pa and fetch_km", {
  minimal_data <- test_data[, c("pa_pred", "fetch_km")]
  pdf(NULL)
  expect_silent(plot_sav_distribution(minimal_data, type = "pa", predictors = "fetch", post_hoc = FALSE))
  dev.off()
})

test_that("Function works with only cover and depth", {
  data_subset <- test_data[, c("depth_m", "cover_pred", "cover_post_hoc")]
  pdf(NULL)
  expect_silent(plot_sav_distribution(data_subset, type = "cover", predictors = "depth"))
  dev.off()
})

test_that("Function errors on invalid predictor name", {
  pdf(NULL)
  expect_error(plot_sav_distribution(test_data, predictors = "invalid"), "No suitable columns found for plotting.")
  dev.off()
})

test_that("plots have known output", {
  pdf(NULL)
  plots <- plot_sav_distribution(test_data)
  vdiffr::expect_doppelganger("plot_sav_distribution", plots)
  dev.off()
})

test_that("Plot respects max_depth parameter", {
  pdf(NULL)
  plots <- plot_sav_distribution(test_data, max_depth = 10)
  vdiffr::expect_doppelganger("density-max-depth-10", plots)
  dev.off()
})



# --------- plot_sav_density
# Sample dataset for testing
withr::with_seed(123, {
  test_data <- data.frame(
    depth_m = runif(100, 0, 15),
    fetch_km = runif(100, 0, 15),
    pa_pred = sample(0:1, 100, replace = TRUE),
    pa_post_hoc = sample(0:1, 100, replace = TRUE)
  )
})

test_that("Function runs without errors for default parameters", {
  pdf(NULL) # Suppress plot output
  expect_silent(plot_sav_density(test_data))
  dev.off()
})

test_that("Function runs with only Depth predictor", {
  pdf(NULL)
  expect_silent(plot_sav_density(test_data, predictors = "depth"))
  dev.off()
})

test_that("Function runs with only Fetch predictor", {
  pdf(NULL)
  expect_silent(plot_sav_density(test_data, predictors = "fetch"))
  dev.off()
})

test_that("Function runs with post-hoc data", {
  pdf(NULL)
  expect_silent(plot_sav_density(test_data, post_hoc = TRUE))
  dev.off()
})

test_that("Function errors when required PA column is missing", {
  incomplete_data <- test_data[, !names(test_data) %in% c("pa", "pa_post_hoc")]
  expect_error(plot_sav_density(incomplete_data), "Data must contain the specified PA column.")
})

test_that("Function handles empty dataset without error", {
  empty_data <- data.frame()
  expect_error(plot_sav_density(empty_data), "Data must contain the specified PA column.")
})

test_that("Output is a ggplot object", {
  pdf(NULL)
  plots <- plot_sav_density(test_data)
  dev.off()
  expect_true(inherits(plots, "ggplot"))
})

test_that("plots have known output", {
  pdf(NULL)
  plots <- plot_sav_density(test_data)
  vdiffr::expect_doppelganger("plot_sav_density", plots)
  dev.off()
})



# --------- plot_sav_tmap
# Test dataset
withr::with_seed(123, {
  polygon <- sf::st_as_sf(sf::st_sfc(
    sf::st_polygon(list(rbind(
      c(-82, 42), c(-82, 43), c(-81, 43), c(-81, 42), c(-82, 42)
    )))
  ), crs = 4326)

  points <- sf::st_sample(polygon, 100) |>
    sf::st_sf() |>
    dplyr::mutate(
      cover_pred = runif(100, 0, 100),
      pa_pred = sample(0:1, 100, replace = TRUE),
      depth_m = runif(100, 0, 15),
      fetch_km = runif(100, 0, 10),
      pa_post_hoc = sample(0:1, 100, replace = TRUE),
      cover_post_hoc = runif(100, 0, 100)
    )

  study_zone <- list(polygon = polygon, points = points)
})

test_that("plot_sav_tmap runs with defaults", {
  expect_s3_class(plot_sav_tmap(study_zone), "tmap")
})

test_that("plot_sav_tmap runs in static mode", {
  expect_s3_class(plot_sav_tmap(study_zone, interactive = FALSE), "tmap")
})

test_that("plot_sav_tmap works with single layer", {
  expect_s3_class(plot_sav_tmap(study_zone, layers = "pa"), "tmap")
})

test_that("plot_sav_tmap works with post_hoc = TRUE", {
  expect_s3_class(plot_sav_tmap(study_zone, post_hoc = TRUE), "tmap")
})

test_that("plot_sav_tmap errors when requested layer is missing", {
  bad_zone <- study_zone
  bad_zone$points <- dplyr::select(bad_zone$points, -fetch_km)
  expect_error(plot_sav_tmap(bad_zone, layers = "fetch"), "fetch")
})

test_that("plot_sav_tmap returns a tmap object", {
  result <- plot_sav_tmap(study_zone)
  expect_s3_class(result, "tmap")
})

test_that("plot_sav_tmap can export file", {
  tmp <- tempfile(fileext = ".png")
  expect_silent(plot_sav_tmap(study_zone, export_path = tmp, interactive = FALSE))
  expect_true(file.exists(tmp))
  unlink(tmp)
})

test_that("plots have known output", {
  pdf(NULL)
  plots <- plot_sav_tmap(study_zone, layers = "cover", interactive = FALSE)
  vdiffr::expect_doppelganger("plot_sav_tmap", plots)
  dev.off()
})
