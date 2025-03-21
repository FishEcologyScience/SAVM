# --------- plot_sav_distribution
# Sample dataset for testing
withr::with_seed(123, {
test_data <- data.frame(
  depth_m = runif(100, 0, 15),
  fetch_km = runif(100, 0, 15),
  pa = sample(0:1, 100, replace = TRUE),
  cover = runif(100, 0, 100),
  pa_post_hoc = sample(0:1, 100, replace = TRUE),
  cover_post_hoc = runif(100, 0, 100)
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
  incomplete_data <- test_data[, !names(test_data) %in% c("depth_m", "fetch_km")]
  pdf(NULL)
  expect_error(plot_sav_distribution(incomplete_data), "No suitable columns found for plotting.")
  dev.off()
})

test_that("Function handles empty dataset without error", {
  empty_data <- data.frame()
  pdf(NULL)
  expect_error(plot_sav_distribution(empty_data), "No suitable columns found for plotting.")
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

test_that("plots have known output", {
  pdf(NULL)
  plots <- plot_sav_distribution(test_data)
  vdiffr::expect_doppelganger("plot_sav_distribution", plots)
  dev.off()
})

# --------- plot_sav_density
# Sample dataset for testing
set.seed(125)
test_data <- data.frame(
  depth_m = runif(100, 0, 15),
  fetch_km = runif(100, 0, 15),
  pa = sample(0:1, 100, replace = TRUE),
  pa_post_hoc = sample(0:1, 100, replace = TRUE)
)

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
