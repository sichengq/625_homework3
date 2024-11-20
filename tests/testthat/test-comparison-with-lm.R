test_that("my_lm matches base lm output", {
  data(mtcars)

  # Fit models
  fit_my <- my_lm(mpg ~ wt + cyl, data = mtcars)
  fit_base <- lm(mpg ~ wt + cyl, data = mtcars)

  # Compare coefficients
  expect_equal(fit_my$coefficients, coef(fit_base), tolerance = 1e-6)

  # Compare fitted values
  expect_equal(fit_my$fitted.values, fitted(fit_base), tolerance = 1e-6)

  # Compare residuals
  expect_equal(fit_my$residuals, residuals(fit_base), tolerance = 1e-6)
})
