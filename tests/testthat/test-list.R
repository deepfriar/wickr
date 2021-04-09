test_that("list", {
  expect_identical(sumer(list(lm_iris, lm_iris_rob)), list_iris)
})
