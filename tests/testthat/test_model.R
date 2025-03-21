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

# res1 <- structure(
#     list(Depth = c(5, 10), Cover = c(
#         85.4316666666667,
#         21.144
#     )),
#     row.names = c(NA, -2L), class = "data.frame"
# )

# res2 <- structure(
#     list(Fetch = c(1, 2), Cover = c(78.8616666666666, 98.5113333333333)),
#     row.names = c(NA, -2L), class = "data.frame"
# )

# res3 <- structure(
#     list(Depth = c(5, 10), Fetch = c(1, 2), Cover = c(
#         77.1123333333333,
#         44.6653333333333
#     )),
#     row.names = c(NA, -2L), class = "data.frame"
# )

# res1_b <- structure(
#     list(
#         Depth = c(5, 10), PA = 1:0, Cover = c(85.4316666666667, 21.144)
#     ),
#     row.names = c(NA, -2L), class = "data.frame"
# )

# res2_b <- structure(
#     list(Fetch = c(1, 2), PA = 0:1, Cover = c(
#         78.8616666666666,
#         98.5113333333333
#     )),
#     row.names = c(NA, -2L), class = "data.frame"
# )

# res3_b <- structure(
#     list(
#         Depth = c(5, 10), Fetch = c(1, 2), PA = c(0L, 0L), Cover = c(77.1123333333333, 44.6653333333333)
#     ),
#     row.names = c(NA, -2L), class = "data.frame"
# )


# test_that("sav_model() works", {
#     withr::with_options(
#         list(savm.verbose = "q"),
#         {
#             expect_equal(sav_model(df_ok_1, type = "cover"), res1)
#             expect_equal(sav_model(df_ok_2, type = "cover"), res2)
#             expect_equal(sav_model(df_ok_3, type = "cover"), res3)
#             #
#             expect_equal(sav_model(df_ok_1), res1_b)
#             expect_equal(sav_model(df_ok_2), res2_b)
#             expect_equal(sav_model(df_ok_3), res3_b)
#             #
#             expect_equal(sav_model(df_ok_1, depth = "depth"), res1_b)
#         }
#     )
# })


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
