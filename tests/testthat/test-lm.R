test_that("lm", {
  expect_identical(sumer(lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data=iris)),
                   lm_iris_simple) # no categorical predictors
  expect_identical(sumer(lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species, data=iris)),
                   lm_iris)
  expect_identical(sumer(lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species, data=iris), vcov.=sandwich::vcovHC),
                   lm_iris_rob)
  expect_identical(wickr::sumer(lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width * Species, data = iris), margins = TRUE),
                   lm_inter)
})
