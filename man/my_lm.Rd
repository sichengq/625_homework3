% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/my_lm.R
\name{my_lm}
\alias{my_lm}
\title{Fit a Linear Model}
\usage{
my_lm(
  formula,
  data,
  subset = NULL,
  weights = NULL,
  na.action = na.omit,
  method = "qr",
  model = TRUE,
  x = FALSE,
  y = FALSE,
  qr = TRUE,
  singular.ok = TRUE,
  contrasts = NULL,
  offset = NULL,
  ...
)
}
\arguments{
\item{formula}{A formula specifying the linear model.}

\item{data}{A data frame containing the variables in the model.}

\item{subset}{A vector specifying the subset of observations to include.}

\item{weights}{An optional numeric vector of weights for weighted least squares.}

\item{na.action}{A function to specify the action on missing values (default: \code{na.omit}).}

\item{method}{The fitting method; only "qr" is supported.}

\item{model}{Logical; if \code{TRUE}, return the model frame.}

\item{x}{Logical; if \code{TRUE}, return the model matrix.}

\item{y}{Logical; if \code{TRUE}, return the response vector.}

\item{qr}{Logical; if \code{TRUE}, return the QR decomposition.}

\item{singular.ok}{Logical; if \code{TRUE}, allow singular fits.}

\item{contrasts}{A list specifying contrasts for factors in the model.}

\item{offset}{An optional numeric vector of offsets.}

\item{...}{Additional arguments (currently unused).}
}
\value{
An object of class \code{"lm"} containing:
\describe{
\item{coefficients}{A named vector of regression coefficients.}
\item{residuals}{The residuals (response minus fitted values).}
\item{fitted.values}{The fitted mean values.}
\item{rank}{The rank of the fitted model.}
\item{weights}{The weights used for fitting.}
\item{df.residual}{Degrees of freedom for residuals.}
\item{call}{The matched function call.}
\item{terms}{The terms object used.}
\item{contrasts}{The contrasts used.}
\item{offset}{The specified offset.}
\item{x, y, qr, model}{Optional components based on input flags.}
}
}
\description{
This function implements a simplified version of the base \code{lm} function,
providing functionality for linear regression with optional weights, offsets,
and handling for singular fits.
}
\examples{
# Example using the mtcars dataset
data(mtcars)
fit <- my_lm(mpg ~ wt + cyl, data = mtcars)
summary(fit)

}
