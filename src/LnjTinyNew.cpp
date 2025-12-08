#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

// ========== UTILITY FUNCTIONS ==========

// [[Rcpp::export]]
NumericMatrix matm_rcpp(NumericMatrix A, NumericMatrix B) {
  int a1 = A.nrow();
  int a2b1 = A.ncol();
  int b2 = B.ncol();
  
  NumericMatrix OUT(a1, b2);
  
  for(int j = 0; j < b2; j++) {
    for(int i = 0; i < a1; i++) {
      OUT(i, j) = 0.0;
      for(int k = 0; k < a2b1; k++) {
        OUT(i, j) += A(i, k) * B(k, j);
      }
    }
  }
  
  return OUT;
}

// [[Rcpp::export]]
IntegerVector exind_rcpp(NumericMatrix Z, NumericMatrix X) {
  int n = X.nrow();
  int p = X.ncol();
  int nmu = Z.nrow();
  
  IntegerVector IND(n);
  
  for(int i = 0; i < n; i++) {
    IND[i] = 0;
    double rmin = 1.0e9;
    
    for(int j = 0; j < nmu; j++) {
      double temp1 = 0.0;
      for(int k = 0; k < p; k++) {
        double diff = X(i, k) - Z(j, k);
        temp1 += diff * diff;
      }
      
      if(temp1 < rmin) {
        rmin = temp1;
        IND[i] = j;
      }
    }
  }
  
  return IND;
}


// [[Rcpp::export]]
double alfunc_rcpp(NumericVector BVEC,
                   NumericMatrix X,
                   NumericVector Y,
                   NumericMatrix M,
                   NumericVector MU,
                   double LAMBDA,
                   double CONST1,
                   double CONST2,
                   int U, int V) {
  
  int n = X.nrow();
  int p = X.ncol();
  int nmu = MU.length();
  
  // CRITICAL FIX: Reshape BVEC to match FORTRAN's column-major storage
  // FORTRAN stores matrix as: column 1, then column 2, etc.
  // This matches R's matrix(bvec, nrow=U+V, ncol=p)
  
  NumericMatrix B(U + V, p);
  
  // Fill B column by column (FORTRAN/R style)
  int idx = 0;
  for(int j = 0; j < p; j++) {
    for(int i = 0; i < (U + V); i++) {
      B(i, j) = BVEC[idx];
      idx++;
    }
  }
  
  // Alternative (more concise):
  // for(int i = 0; i < (U + V); i++) {
  //   for(int j = 0; j < p; j++) {
  //     B(i, j) = BVEC[i + j * (U + V)];
  //   }
  // }
  
  // Z = M %*% B
  NumericMatrix Z = matm_rcpp(M, B);
  
  // Get indices
  IntegerVector IND = exind_rcpp(Z, X);
  
  // Get predictions
  NumericVector PRED(n);
  for(int i = 0; i < n; i++) {
    PRED[i] = MU[IND[i]];
  }
  
  // Calculate loss term
  double temp1 = 0.0;
  for(int i = 0; i < n; i++) {
    double diff = Y[i] - PRED[i];
    temp1 += diff * diff;
  }
  
  double loss = temp1 / CONST1;
  
  // Add penalty term if lambda > 0
  if(LAMBDA > 0.0 && nmu > 2) {
    double temp2 = 0.0;
    for(int j = 0; j < p; j++) {
      for(int i = 1; i < (nmu - 1); i++) {
        double diff = Z(i - 1, j) - 2.0 * Z(i, j) + Z(i + 1, j);
        temp2 += diff * diff;
      }
    }
    loss += LAMBDA * temp2 / CONST2;
  }
  
  return loss;
}
