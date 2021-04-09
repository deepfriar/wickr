test_that("tibble of lms", {
  d <- MASS::Cars93
  d <- tidyr::pivot_longer(d, .data$Min.Price:.data$Max.Price, "Y", values_to = "y")
  d <- dplyr::group_by(d, .data$Y)
  d <- dplyr::summarise(d, lm = list(lm(y ~ Origin + Type)))

  expect_identical(sumer(d), tbl_lm)
})
