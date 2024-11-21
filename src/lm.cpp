#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector simple_lm(NumericMatrix X, NumericVector y) {
  int n = X.nrow();
  int p = X.ncol();

  NumericVector Xty(p);
  for (int i = 0; i < p; ++i) {
    for (int k = 0; k < n; ++k) {
      Xty[i] += X(k, i) * y[k];
    }
  }

  return Xty;
}
