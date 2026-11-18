context("test-remove_rmd_template")

test_that("remove_rmd_template() works!", {
  testthat::skip_if(
    Sys.getenv("RUN_RNSSP_NETWORK_TESTS") != "true",
    "Requires live GitHub access; set RUN_RNSSP_NETWORK_TESTS=true to run."
  )
  add_rmd_template("text_mining", restart = FALSE)
  expect_message(remove_rmd_template("text_mining", restart = FALSE))
  expect_error(remove_rmd_template("text_mining", restart = FALSE))
})
