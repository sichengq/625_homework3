test_that("my_lm fits a basic linear model", {
  data(mtcars)
  fit <- my_lm(mpg ~ wt + cyl, data = mtcars)

  # Check class
  expect_s3_class(fit, "lm")

  # Check coefficients
  expect_length(fit$coefficients, 3)  # Intercept, wt, cyl

  # Check residuals length
  expect_equal(length(fit$residuals), nrow(mtcars))

  # Check names of coefficients
  expect_equal(names(fit$coefficients), c("(Intercept)", "wt", "cyl"))
})

test_that("my_lm handles missing data correctly", {
  data(mtcars)
  mtcars$mpg[1] <- NA

  fit <- my_lm(mpg ~ wt + cyl, data = mtcars, na.action = na.omit)

  # Check that missing row is omitted
  expect_equal(length(fit$residuals), nrow(mtcars) - 1)
})
