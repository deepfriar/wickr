test_that("crch", {
  data("GasolineYield", package="betareg")
  data("MockJurors",    package="betareg")

  expect_identical(sumer(crch::crch(yield ~ gravity + pressure + temp10 + temp, data = GasolineYield, left=0, right=1)), cr_gas)

  expect_identical(
    sumer(crch::crch(confidence ~ factor(verdict) * factor(conflict) | factor(verdict) * factor(conflict),
                     data = MockJurors, left = 0, right = 1)),
    cr_jury
  )
})
