test_that("clm", {
  expect_identical(sumer(ordinal::clm(rating ~ temp + contact + bottle + judge, data = ordinal::wine)), clm_wine)
})
