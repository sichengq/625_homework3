
<!-- README.md is generated from README.Rmd. Please edit that file -->

# homework3

# customlm: A Custom Linear Model Implementation

Sicheng Qian

- [homework3 Package](#homewoek3-package)
  - [Overview](#overview)
  - [Installation](#installation)
  - [Usage](#usage)
  - [Functionality](#functionality)
  - [Comparison with `lm()`](#comparison-with-lm)
  - [Testing](#testing)
  - [License](#license)

# homework3 Package

## Overview

The `homework3` package provides a custom implementation of a linear
regression model fitting function, closely following R’s built-in `lm()`
function. The package demonstrates how to fit linear models using QR
decomposition and other essential components typically used in linear
regression.

This package is designed to provide an educational understanding of the
underlying mechanics behind linear regression fitting, including
calculating coefficients, residuals, fitted values, and degrees of
freedom.

## Installation

You can install the `homewoek3` package from GitHub using the following
command:

``` r
# Install devtools if not already installed
# install.packages("devtools")

# Install the customlm package
devtools::install_github("sichengq/625_homework3")
```

## Usage

Once the package is installed, you can load it and use the `my_lm()`
function to fit linear models.

``` r
library(homework3)

# Simulated data
set.seed(123)
X <- rnorm(100)
Y <- 2 * X + 3 + rnorm(100)
data <- data.frame(X = X, Y = Y)

# Fit the model using my_lm()
model <- my_lm(Y ~ X, data = data)

# Check model coefficients
print(model$coefficients)

# Check residuals
print(model$residuals)

# Check fitted values
print(model$fitted.values)
```

## Functionality

The `my_lm()` function in this package performs linear regression using
the QR decomposition method. The function has several options similar to
the `lm()` function in base R, such as:

- `formula`: A formula object specifying the model to be fitted (e.g.,
  `Y ~ X`).
- `data`: The data frame containing the variables.
- `weights`: Optional weights to apply for weighted least squares
  regression.
- `na.action`: How to handle missing data.
- `model`: If `TRUE`, returns the model frame.
- `x`: If `TRUE`, returns the model matrix (X values).
- `y`: If `TRUE`, returns the response variable (Y values).
- `qr`: If `TRUE`, returns the QR decomposition.
- `singular.ok`: Whether to allow singular fits.

## Comparison with `lm()`

The `my_lm()` function is designed to be functionally equivalent to R’s
built-in `lm()` function, but with the ability to specify additional
low-level options like QR decomposition and a custom interface.

### Example:

``` r
# Fit the model using the built-in lm() function
lm_model <- lm(Y ~ X, data = data)

# Compare coefficients between my_lm() and lm()
print(all.equal(model$coefficients, lm_model$coefficients))

# Compare residuals
print(all.equal(model$residuals, lm_model$residuals))
```

This will confirm that the custom implementation produces the same
results as the standard `lm()` function.

## Testing

Unit tests are included to verify the functionality of the `my_lm`
function using the `testthat` package.

To run the tests:

``` r
devtools::test()
```

## License

This package is licensed under the MIT License. See the `LICENSE` file
for more details.
