test_that("glm", {
  expect_identical(sumer(glm(low ~ age + lwt + factor(race) + smoke + ptl + ht + ui  + ftv, binomial, MASS::birthwt)), glm_bwt)
})
