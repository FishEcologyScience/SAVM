test_that("sav_load_model() works", {
    expect_error(
        sav_load_model("pap"),
        "'arg' should be one of \"cover\", \"pa\"",
        fixed = TRUE
    )
    expect_true(inherits(sav_load_model("pa", "depth+fetch"), "randomForest"))
})


df_ok_1 <- data.frame(depth = c(5, 10))
df_ok_2 <- data.frame(FETCH = c(1, 2))
df_ok_3 <- cbind(df_ok_1, df_ok_2)

res_1 <- structure(
    list(Depth = c(5, 10), Cover = c(
        85.4316666666667,
        21.144
    )),
    class = "data.frame", row.names = c("1", "2")
)

res_2 <- structure(
    list(Fetch = c(1, 2), Cover = c(78.8616666666666, 98.5113333333333)),
    class = "data.frame", row.names = c("1", "2")
)

res_3 <- structure(
    list(Depth = c(5, 10), Fetch = c(1, 2), Cover = c(
        77.1123333333333,
        44.6653333333333
    )),
    class = "data.frame", row.names = c(
        "1",
        "2"
    )
)

res1_pa <- structure(
    list(Depth = c(5, 10), PA = 1:0),
    class = "data.frame", row.names = c(
        "1",
        "2"
    )
)

res2_pa <- structure(
    list(Fetch = c(1, 2), PA = 0:1),
    class = "data.frame", row.names = c(
        "1",
        "2"
    )
)
res3_pa <- structure(
    list(Depth = c(5, 10), Fetch = c(1, 2), PA = c(
        0L,
        0L
    )),
    class = "data.frame", row.names = c("1", "2")
)




test_that("sav_model() works", {
    withr::with_options(
        list(savm.verbose = "q"),
        {
            expect_equal(sav_model(df_ok_1), res_1)
            expect_equal(sav_model(df_ok_2), res_2)
            expect_equal(sav_model(df_ok_3), res_3)
            #
            expect_equal(sav_model(df_ok_1, depth = "depth"), res_1)
            #
            expect_identical(sav_model(df_ok_1, type = "pa"), res1_pa)
            expect_identical(sav_model(df_ok_2, type = "pa"), res2_pa)
            expect_identical(sav_model(df_ok_3, type = "pa"), res3_pa)
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
                "`depth2` is not a column of `d_inputs`"
            )
        }
    )
})
