test_that("sav_load_model() works", {
  expect_error(
    sav_load_model("pap"),
    "'arg' should be one of \"cover\", \"pa\"",
    fixed = TRUE
  )
  expect_s3_class(sav_load_model("pa", "rf"), "randomForest")
})


test_that("sav_load_model() works", {
  withr::with_options(
    list(savm.verbose = "quiet"),
    {
      expect_error(
        sav_model(data.frame(FETCH_km = c(1, 2))),
        "`depth` must point to an existing column in `dat`."
      )
    }
  )
})


df_ok1 <- data.frame(
  depth = c(5, 1, 10),
  fetch = c(1, 3, 2),
  limitation = c(TRUE, FALSE, TRUE)
)

res1 <- structure(
  list(
    depth_m = c(5, 1, 10),
    fetch_km = c(1, 3, 2),
    limitation = c(TRUE, FALSE, TRUE),
    pa_pred = c(0.984, 0.44, 0),
    pa = c(1, 0, 0),
    cover_pred = c(88.3866124814003, 39.3158240240619, 45.7916938687299),
    cover = c(88.3866124814003, 0, 0),
    pa_post_hoc = c(
      1,
      0, 0
    ),
    cover_post_hoc = c(88.3866124814003, 0, 0)
  ),
  row.names = c(
    NA,
    -3L
  ),
  class = "data.frame"
)

res2 <- structure(
  list(
    depth_m = c(5, 1, 10),
    fetch_km = c(1, 3, 2),
    limitation = c(TRUE, FALSE, TRUE),
    pa_pred = c(0.984, 0.44, 0),
    pa = c(1, 1, 0),
    cover_pred = c(85.2773622236459, 58.8615007837882, 41.9112417990861),
    cover = c(85.2773622236459, 58.8615007837882, 0),
    pa_post_hoc = c(1, 0, 0),
    cover_post_hoc = c(85.2773622236459, 0, 0)
  ),
  row.names = c(NA, -3L),
  class = "data.frame"
)

test_that("sav_model() works", {
  withr::with_options(
    list(savm.verbose = "quiet"),
    {
      expect_equal(sav_model(df_ok1), res1)
      expect_equal(
        sav_model(
          df_ok1,
          method_cover = "lmm",
          pa_threshold = 0.4,
          depth = NULL,
          fetch = NULL
        ), res2
      )
    }
  )
})


res3_e <- data.frame(
  depth_m = c(2, 2, 5),
  fetch_km = c(1, 1, 1),
  substrate = c(TRUE, TRUE, FALSE),
  secchi = c(20, 1, 20),
  limitation_secchi = c(TRUE, FALSE, TRUE),
  vmax = c(28.242038767358, 1.7689, 28.242038767358),
  limitation = c(FALSE, TRUE, TRUE),
  pa_pred = c(0.887758272418952, 0.887758272418952, 0.750382647568143),
  pa = c(1, 1, 1),
  cover_pred = c(100, 100, 100),
  cover = c(100, 100, 100),
  pa_post_hoc = c(0, 0, 0),
  cover_post_hoc = c(0, 0, 0)
)

test_that("sav_model() with full post-hoc works", {
  df_ok2 <- data.frame(
    depth_m = c(2, 2, 5),
    fetch_km = c(1, 1, 1),
    substrate = c(TRUE, TRUE, FALSE),
    secchi = c(20, 1, 20),
    custom = c(FALSE, TRUE, TRUE)
  )
  expect_snapshot(
    res3_a <- sav_model(df_ok2, method_pa = "gam", limitation = "custom")
  )
  expect_equal(res3_a, res3_e)
})
