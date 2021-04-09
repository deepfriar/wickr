test_that("betareg", {
  data("GasolineYield", package="betareg")
  data("MockJurors",    package="betareg")

  expect_identical(sumer(betareg::betareg(yield ~ gravity + pressure + temp10 + temp, data = GasolineYield)), br_gas)

  expect_identical(
    sumer(betareg::betareg(confidence ~ factor(verdict) * factor(conflict) | factor(verdict) * factor(conflict), data = MockJurors)),
    br_jury
  )
})
