
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

