test_that("sav_load_model() works", {
    expect_error(
        sav_load_model("pap"),
        "'arg' should be one of \"cover\", \"pa\"",
        fixed = TRUE
    )
    expect_true(inherits(sav_load_model("pa", "depth+fetch"), "randomForest"))
})


df_ok_1 <- data.frame(depth = c(5, 10))
df_ok_2 <- data.frame(FETCH_km = c(1, 2))
df_ok_3 <- cbind(df_ok_1, df_ok_2)

res1 <- structure(list(depth_m = c(5, 10), pa = 1:0, cover = c(
    85.4316666666667,
    21.144
), pa_post_hoc = 1:0, cover_post_hoc = c(
    85.4316666666667,
    0
)), row.names = c(NA, -2L), class = "data.frame")

res2 <- structure(list(fetch_km = c(1, 2), pa = 0:1, cover = c(
    78.8616666666666,
    98.5113333333333
), pa_post_hoc = 0:1, cover_post_hoc = c(0, 98.5113333333333)), row.names = c(NA, -2L), class = "data.frame")

res3 <- structure(list(depth_m = c(5, 10), fetch_km = c(1, 2), pa = c(
    0L,
    0L
), cover = c(77.1123333333333, 44.6653333333333), pa_post_hoc = c(
    0L,
    0L
), cover_post_hoc = c(0, 0)), row.names = c(NA, -2L), class = "data.frame")


res1_cover <- structure(list(depth_m = c(5, 10), cover = c(
    85.4316666666667,
    21.144
), cover_post_hoc = c(85.4316666666667, 21.144)), row.names = c(
    NA,
    -2L
), class = "data.frame")


res1_pa <- structure(list(depth_m = c(5, 10), pa = 1:0, pa_post_hoc = 1:0), row.names = c(
    NA,
    -2L
), class = "data.frame")


test_that("sav_model() works", {
    withr::with_options(
        list(savm.verbose = "q"),
        {
            expect_equal(sav_model(df_ok_1), res1)
            expect_equal(sav_model(df_ok_2), res2)
            expect_equal(sav_model(df_ok_3), res3)
            #
            expect_equal(sav_model(df_ok_1, type = "cover"), res1_cover)
            expect_equal(sav_model(df_ok_1, type = "pa"), res1_pa)
            #
        }
    )
})


df_not_ok_1 <- data.frame(depth2 = c(5, 10))
test_that("sav_load_model() fails gracefully", {
    expect_error(sav_model("wrong"))
    withr::with_options(
        list(savm.verbose = "q"),
        {
            expect_error(
                sav_model(df_not_ok_1),
                "Either depth or fetch or both must be defined"
            )
            expect_error(
                sav_model(df_ok_1, depth = "depth2"),
                "`depth2` is not a column of `dat`"
            )
        }
    )
})


df_ok_ph_2 <- df_ok_ph_1 <- data.frame(
    depth_m = c(2, 2, 5),
    fetch_km = c(1, 1, 1),
    substrate = c(TRUE, TRUE, FALSE),
    secchi = c(20, 1, 20),
    custom = c(FALSE, TRUE, TRUE)
)
df_ok_ph_2[["depth_m"]] <- NULL

res_ph1a <- structure(list(depth_m = c(2, 2, 5), fetch_km = c(1, 1, 1), substrate = c(
    TRUE,
    TRUE, FALSE
), secchi = c(20, 1, 20), limitation_secchi = c(
    TRUE,
    FALSE, TRUE
), vmax = c(28.242038767358, 1.7689, 28.242038767358), pa = c(1L, 1L, 0L), cover = c(
    62.7603333333334, 62.7603333333334,
    77.1123333333333
), pa_post_hoc = c(1L, 0L, 0L), cover_post_hoc = c(
    62.7603333333334,
    0, 0
)), row.names = c(NA, -3L), class = "data.frame")


# next call
res_ph1b <- res_ph1a
res_ph1b$limitation <- df_ok_ph_2$custom
res_ph1b <- res_ph1b |>
    dplyr::relocate(limitation, .after = vmax)
res_ph1b$pa_post_hoc[1L] <- 0L
res_ph1b$cover_post_hoc[1L] <- 0


test_that("sav_model() works", {
    withr::with_options(
        list(savm.verbose = "warning"),
        {
            expect_equal(sav_model(df_ok_ph_1), res_ph1a)
            expect_equal(sav_model(df_ok_ph_1, limitation = "custom"), res_ph1b)
            expect_warning(sav_model(df_ok_ph_2))
        }
    )
})
