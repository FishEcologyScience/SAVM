#--- path
test_that("path helpers work", {
    expect_identical(
        path_package("extdata/models/sav_rf_pa_depth.rds"),
        path_model("sav_rf_pa_depth.rds")
    )
    #
    expect_error(path_model("wrong"))
    expect_error(path_package("wrong"))
})

#--- messages

test_that("alert message helpers works", {
    expect_snapshot(sav_msg_info("info"))
    var1 <- "test"
    expect_snapshot(sav_msg_info("success {var1}"))
    expect_snapshot(sav_msg_success("success"))
    expect_snapshot(sav_msg_warning("warning"))
    expect_snapshot(sav_msg_danger("danger"))
    expect_snapshot(
        withr::with_options(
            list(savm.verbose = "q"),
            sav_msg_info("info")
        )
    )
})

test_that("message helpers should work", {
    expect_snapshot(sav_inform("info"))
    expect_warning(sav_warn("success"))
    expect_snapshot(sav_debug_msg("debug"))
    expect_snapshot(
        withr::with_options(
            list(savm.verbose = "debug"),
            sav_debug_msg("debug")
        )
    )
    expect_error(sav_stop_if_not(1 > 2, "wrong"), "wrong")
})