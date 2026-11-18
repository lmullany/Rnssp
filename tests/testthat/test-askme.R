context("test-askme")

test_that("askme() works!", {
  testthat::skip_if_not(interactive())
  options(askme = function(...) {
    "password"
  })
  testthat::expect_silent(askme())
})
