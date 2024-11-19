#' Fit a Linear Model
#'
#' This function implements a simplified version of the base `lm` function,
#' providing functionality for linear regression with optional weights, offsets,
#' and handling for singular fits.
#'
#' @param formula A formula specifying the linear model.
#' @param data A data frame containing the variables in the model.
#' @param subset A vector specifying the subset of observations to include.
#' @param weights An optional numeric vector of weights for weighted least squares.
#' @param na.action A function to specify the action on missing values (default: `na.omit`).
#' @param method The fitting method; only "qr" is supported.
#' @param model Logical; if `TRUE`, return the model frame.
#' @param x Logical; if `TRUE`, return the model matrix.
#' @param y Logical; if `TRUE`, return the response vector.
#' @param qr Logical; if `TRUE`, return the QR decomposition.
#' @param singular.ok Logical; if `TRUE`, allow singular fits.
#' @param contrasts A list specifying contrasts for factors in the model.
#' @param offset An optional numeric vector of offsets.
#' @param ... Additional arguments (currently unused).
#'
#' @return An object of class `"lm"` containing:
#' \describe{
#'   \item{coefficients}{A named vector of regression coefficients.}
#'   \item{residuals}{The residuals (response minus fitted values).}
#'   \item{fitted.values}{The fitted mean values.}
#'   \item{rank}{The rank of the fitted model.}
#'   \item{weights}{The weights used for fitting.}
#'   \item{df.residual}{Degrees of freedom for residuals.}
#'   \item{call}{The matched function call.}
#'   \item{terms}{The terms object used.}
#'   \item{contrasts}{The contrasts used.}
#'   \item{offset}{The specified offset.}
#'   \item{x, y, qr, model}{Optional components based on input flags.}
#' }
#'
#' @examples
#' # Example using the mtcars dataset
#' data(mtcars)
#' fit <- my_lm(mpg ~ wt + cyl, data = mtcars)
#' summary(fit)
#'
#' @export
#' @importFrom stats .getXlevels model.matrix model.offset
#'   model.response model.weights na.omit setNames



my_lm <- function(formula, data, subset = NULL, weights = NULL,
                  na.action = na.omit, method = "qr", model = TRUE,
                  x = FALSE, y = FALSE, qr = TRUE, singular.ok = TRUE,
                  contrasts = NULL, offset = NULL, ...) {
  if (missing(formula)) stop("Formula must be provided.")
  if (method != "qr") stop("Only 'qr' method is supported.")

  # Prepare the model frame
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset", "weights", "na.action", "offset"),
             names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())

  # Handle missing data
  na_act <- attr(mf, "na.action")
  if (!is.null(na_act)) {
    message("Rows with missing data omitted")
  }

  if (nrow(mf) == 0L) stop("No data to fit the model.")


  # Extract components
  mt <- attr(mf, "terms")
  y_var <- model.response(mf, "numeric")
  x_mat <- model.matrix(mt, mf, contrasts.arg = contrasts)

  # Handle weights and offsets
  weights <- as.vector(model.weights(mf))
  if (is.null(weights)) {
    weights <- rep(1, length(y_var))  # Default to unweighted
  }
  if (any(weights < 0)) stop("Weights must be non-negative.")

  offset <- model.offset(mf)
  if (is.null(offset)) {
    offset <- rep(0, length(y_var))
  } else if (length(offset) != length(y_var)) {
    stop("Invalid offset length: must match the length of the response variable.")
  }

  # Apply weights and offsets
  sqrt_weights <- sqrt(weights)
  y_weighted <- (y_var - offset) * sqrt_weights
  x_weighted <- x_mat * sqrt_weights

  # Perform QR decomposition
  qr_fit <- qr(x_weighted)
  # Perform QR decomposition
  qr_fit <- qr(x_weighted)


  if (qr_fit$rank < ncol(x_mat)) {
    if (!singular.ok) {
      stop("Singular fit detected: predictors are linearly dependent.")
    } else {
      warning("Singular fit detected: proceeding with reduced rank.")
    }
  }

  # Compute coefficients using weighted y
  coefficients <- qr.coef(qr_fit, y_weighted)

  # Standardize outputs to match base lm
  coefficients <- setNames(as.numeric(coefficients), colnames(x_mat))
  fitted_vals <- setNames(as.numeric(x_mat %*% coefficients), rownames(mf))
  residuals <- setNames(as.numeric(y_var - offset - fitted_vals), rownames(mf))

  # Create output object
  fit <- list(coefficients = coefficients,
              residuals = residuals,
              fitted.values = fitted_vals,
              rank = qr_fit$rank,
              weights = weights,
              df.residual = length(y_var) - qr_fit$rank,
              call = match.call(),
              terms = mt,
              contrasts = attr(x_mat, "contrasts"),
              xlevels = .getXlevels(mt, mf),
              offset = offset,
              y = if (y) y_var else NULL,
              x = if (x) x_mat else NULL,
              qr = if (qr) qr_fit else NULL,
              model = if (model) mf else NULL)
  class(fit) <- "lm"
  return(fit)
}
