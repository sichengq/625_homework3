#include <Rcpp.h>
using namespace Rcpp;

// Function to compute QR decomposition and solve for coefficients
// [[Rcpp::export]]
NumericVector qr_lm(const NumericMatrix& X, const NumericVector& y) {
  int n = X.nrow(); // number of rows
  int p = X.ncol(); // number of columns

  // Ensure X and y dimensions are compatible
  if (y.size() != n) {
    stop("The length of y must match the number of rows in X.");
  }

  // QR decomposition
  NumericMatrix Q = clone(X);  // Clone X to avoid modifying the input
  NumericMatrix R(p, p);       // Upper triangular matrix
  NumericVector coefficients(p); // Coefficients vector

  // Perform Gram-Schmidt process
  for (int k = 0; k < p; ++k) {
    // Compute R[k, k]
    double norm = 0.0;
    for (int i = 0; i < n; ++i) {
      norm += Q(i, k) * Q(i, k);
    }
    R(k, k) = sqrt(norm);

    // Normalize the k-th column of Q
    for (int i = 0; i < n; ++i) {
      Q(i, k) /= R(k, k);
    }

    // Compute R[k, j] for j > k
    for (int j = k + 1; j < p; ++j) {
      double dot = 0.0;
      for (int i = 0; i < n; ++i) {
        dot += Q(i, k) * Q(i, j);
      }
      R(k, j) = dot;

      // Update Q(:, j)
      for (int i = 0; i < n; ++i) {
        Q(i, j) -= R(k, j) * Q(i, k);
      }
    }
  }

  // Solve R * coefficients = Q'y
  NumericVector QTy(p, 0.0);
  for (int k = 0; k < p; ++k) {
    for (int i = 0; i < n; ++i) {
      QTy[k] += Q(i, k) * y[i];
    }
  }

  // Back substitution to solve R * coefficients = QTy
  for (int k = p - 1; k >= 0; --k) {
    coefficients[k] = QTy[k];
    for (int j = k + 1; j < p; ++j) {
      coefficients[k] -= R(k, j) * coefficients[j];
    }
    coefficients[k] /= R(k, k);
  }

  return coefficients;
}
