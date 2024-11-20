#' Summary Method for my_lm
#'
#' Provides a simple summary for models fitted using `my_lm`.
#'
#' @param object An object of class `"lm"` returned by `my_lm`.
#' @return A summary of the fitted model including coefficients and residual standard error.
#' @examples
#' data(mtcars)
#' fit <- my_lm(mpg ~ wt + cyl, data = mtcars)
#' summary(fit)
#' @export
summary.my_lm <- function(object) {
  if (!inherits(object, "lm")) {
    stop("The input must be an object of class 'lm'.")
  }

  cat("Call:\n")
  print(object$call)

  cat("\nCoefficients:\n")
  coef_table <- data.frame(
    Estimate = object$coefficients,
    row.names = names(object$coefficients)
  )

  # Calculate residual standard error
  residual_se <- sqrt(sum(object$residuals^2) / object$df.residual)

  print(coef_table)
  cat("\nResidual standard error:", residual_se, "on", object$df.residual, "degrees of freedom\n")

  invisible(object)
}

