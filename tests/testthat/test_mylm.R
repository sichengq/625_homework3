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


# Simulated dataset
set.seed(123)
n <- 1000
x1 <- rnorm(n)
x2 <- rnorm(n)
y <- 3 + 2 * x1 - 1.5 * x2 + rnorm(n)

simulated_data <- data.frame(y = y, x1 = x1, x2 = x2)

# Fit models using both my_lm and base lm
fit_my_lm <- my_lm(y ~ x1 + x2, data = simulated_data)
fit_base_lm <- lm(y ~ x1 + x2, data = simulated_data)

# Correctness: Compare coefficients, fitted values, and residuals
cat("\nCorrectness Check\n")
all.equal(fit_my_lm$coefficients, coef(fit_base_lm), tolerance = 1e-6)
all.equal(fit_my_lm$fitted.values, fitted(fit_base_lm), tolerance = 1e-6)
all.equal(fit_my_lm$residuals, residuals(fit_base_lm), tolerance = 1e-6)

# Efficiency: Benchmark my_lm vs lm
cat("\nEfficiency Benchmark\n")
library(bench)

# Benchmarking Coefficients Only
cat("\nEfficiency Benchmark\n")
benchmark_results <- bench::mark(
  my_lm = {
    fit <- my_lm(mpg ~ wt + hp + cyl, data = mtcars)
    as.numeric(fit$coefficients)  # Ensure coefficients are a numeric vector
  },
  base_lm = {
    fit <- lm(mpg ~ wt + hp + cyl, data = mtcars)
    as.numeric(coef(fit))  # Ensure coefficients are a numeric vector
  }
)

print(benchmark_results)
# Correctness Check for Coefficients
fit_my <- my_lm(mpg ~ wt + hp + cyl, data = mtcars)
fit_base <- lm(mpg ~ wt + hp + cyl, data = mtcars)

cat("\nCoefficient Comparison\n")
print(all.equal(as.numeric(fit_my$coefficients), as.numeric(coef(fit_base)), tolerance = 1e-6))


test_that("my_lm detects singular fits", {
  # Create a perfectly collinear dataset
  singular_data <- data.frame(
    y = c(1, 2, 3),
    x1 = c(1, 1, 1),
    x2 = c(2, 2, 2)
  )

  # Test singular.ok = FALSE (should throw an error)
  expect_error(
    my_lm(y ~ x1 + x2, data = singular_data, singular.ok = FALSE),
    "Singular fit detected"
  )

  # Test singular.ok = TRUE (should proceed with a warning)
  expect_warning(
    my_lm(y ~ x1 + x2, data = singular_data, singular.ok = TRUE),
    "Singular fit detected"
  )
})

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

