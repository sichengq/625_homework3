test_that("my_lm handles weights correctly", {
  data(mtcars)
  weights <- rep(1, nrow(mtcars))

  fit <- my_lm(mpg ~ wt + cyl, data = mtcars, weights = weights)

  # Check class
  expect_s3_class(fit, "lm")

  # Check residuals length
  expect_equal(length(fit$residuals), nrow(mtcars))
})

test_that("my_lm handles offsets correctly", {
  data(mtcars)
  offset <- rep(1, nrow(mtcars))

  fit <- my_lm(mpg ~ wt + cyl, data = mtcars, offset = offset)

  # Check offset
  expect_equal(fit$offset, offset)

  # Check residuals length
  expect_equal(length(fit$residuals), nrow(mtcars))
})
