context("Basic unit tests")

test_that("foo() works", {
    expect_identical(foo(2.0), 5.0)
    expect_identical(foo(2.0, 3.0), 6.0)
})
