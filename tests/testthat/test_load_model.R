test_that("sav_load_model() works", {
    expect_error(
        sav_load_model("pap"), 
        "'arg' should be one of \"cover\", \"pa\"",
        fixed = TRUE
        )
    expect_true(inherits(sav_load_model("pa", "depth+fetch"), "randomForest"))
})