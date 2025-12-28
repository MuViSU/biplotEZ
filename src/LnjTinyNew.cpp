#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

// Khaled: differences with fortran:
//Optimize alfunc by computing Z directly from BVEC and combining loops

//- Eliminated intermediate B matrix creation by computing Z = M %*% B directly from BVEC vector
//- Combined three separate loops (exind, prediction, loss calculation) into single pass
//- Removed fortran-unused utility functions matm() and exind()
//- Reduces memory allocations and improves cache locality
//- Maintains identical mathematical behavior with better performance

// [[Rcpp::export]]
double alfunc(NumericVector BVEC,
              NumericMatrix X,
              NumericVector Y,
              NumericMatrix M,
              NumericVector MU,
              double LAMBDA,
              double CONST1,
              double CONST2,
              int U, int V) {
  // to remain consistent with the paper, we should set const1 = 1 and lambda = 0.
  int n = X.nrow();
  int p = X.ncol();
  int nmu = MU.length();
  int uv = U + V;
  
  // Compute Z = M %*% B directly from BVEC without creating B
  NumericMatrix Z(nmu, p);
  
  for(int j = 0; j < p; j++) {
    int bvec_offset = j * uv;  // Starting index in BVEC for column j
    for(int i = 0; i < nmu; i++) {
      Z(i, j) = 0.0;
      for(int k = 0; k < uv; k++) {
        Z(i, j) += M(i, k) * BVEC[bvec_offset + k];
      }
    }
  }
  
  // Combined loop: find nearest index, get prediction, and calculate loss
  double temp1 = 0.0;
  
  for(int i = 0; i < n; i++) {
    int closest_idx = 0;
    double rmin = 1.0e9;
    
    // Find closest Z row to X row i
    for(int j = 0; j < nmu; j++) {
      double temp_dist = 0.0;
      for(int k = 0; k < p; k++) {
        double diff = X(i, k) - Z(j, k);
        temp_dist += diff * diff;
      }
      
      if(temp_dist < rmin) {
        rmin = temp_dist;
        closest_idx = j;
      }
    }
    
    // Calculate loss immediately using the closest index
    double diff = Y[i] - MU[closest_idx];
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

//Khaled:
// ========== SIMPLEX SETUP ==========
NumericMatrix varset(NumericVector BVEC, int U, int V, int P, double TAU) {
  int andim = (U + V) * P;
  NumericMatrix AP(andim + 1, andim);
  
  // First row: BVEC unchanged
  for(int j = 0; j < andim; j++) {
    AP(0, j) = BVEC[j];
  }
  
  // Remaining rows: BVEC with TAU added to successive elements
  for(int i = 1; i <= andim; i++) {
    for(int j = 0; j < andim; j++) {
      AP(i, j) = BVEC[j];
    }
    AP(i, i - 1) += TAU;
    //Khaled:
    //Alternative way for AP(i, i - 1).
    //Adding a fixed TAU, regardless of the scale of the BVEC components, is problematic. 
    //It may be better to scale TAU relative to each component to achieve global optimization. 
    //If you want to use the following approach, please uncomment these lines and comment out AP above.
    //If you activate the following code, be careful that TAU cannot be a fixed constant. 
    //In this case, interpret TAU as a relative step size (e.g., TAU = 0.05 means a 5% perturbation), not an absolute one.

    // // Relative perturbation: TAU * |BVEC[i-1]| or TAU if near zero
    // double delta = fabs(BVEC[i - 1]) > 1e-8 ? 
    //                TAU * fabs(BVEC[i - 1]) : TAU;
    // AP(i, i - 1) += delta;
  }
  
  return AP;
}

// ========== SIMPLEX OPERATIONS ==========
double amotry(NumericMatrix& PMAT,       
              NumericVector& YVEK,       
              NumericVector& PSUM,       
              int IHI, 
              double FAC,
              const NumericMatrix& X,     
              const NumericVector& Y,     
              const NumericMatrix& M,     
              const NumericVector& MU,     
              double LAMBDA,             
              double CONST1,              
              double CONST2,              
              int U,                      
              int V){
  //PMAT: The simplex matrix - each row is a vertex (parameter vector)
  //YVEK: Function values at each vertex (loss at each simplex point)
  //PSUM: Sum of all vertices (used to compute centroid efficiently)
  //IHI: Index of the highest (worst) point in the simplex (0-indexed in C++)
  //FAC: Extrapolation factor.
  //All other inputs are for loss function evaluation (passed by value)
  //What thi function do:
  //Extrapolates by a factor FAC through the face of the simplex across 
  //from the highest point, tries it, and replaces the high point if 
  //the new point is better
  int ndim = PMAT.ncol();
  NumericVector PTRY(ndim);
  
  double fac1 = (1.0 - FAC) / ndim;
  double fac2 = fac1 - FAC;
  
  for(int j = 0; j < ndim; j++) {
    PTRY[j] = PSUM[j] * fac1 - PMAT(IHI, j) * fac2;
  }
  
  double ytry = alfunc(PTRY, X, Y, M, MU, LAMBDA, CONST1, CONST2, U, V);
  
  if(ytry < YVEK[IHI]) {
    YVEK[IHI] = ytry;
    for(int j = 0; j < ndim; j++) {
      PSUM[j] = PSUM[j] - PMAT(IHI, j) + PTRY[j];
      PMAT(IHI, j) = PTRY[j];
    }
  }
  
  return ytry;
// Khaled:  pass-by-reference eliminates the need to return PMAT, YVEK, and PSUM,
// since they are modified in place; the calling function only requires ytry for its control-flow logic.
}

// ========== OPTIMIZER ==========

List amoeba(NumericMatrix PMAT, NumericVector YVEK,
                 double FTOL, int ITMAX,
                 NumericMatrix X, NumericVector Y, NumericMatrix M,
                 NumericVector MU, double LAMBDA,
                 double CONST1, double CONST2, int U, int V, double TINY) {
  
  int ndim = PMAT.ncol();
  int mp = PMAT.nrow();
  int iter = 0;
  int erro = 0;
  
  if(ndim > 20) {
    erro = 5;
    return List::create(Named("PMAT") = PMAT,
                       Named("YVEK") = YVEK,
                       Named("ITER") = iter,
                       Named("ERRO") = erro);
  }
  
  NumericVector PSUM(ndim);
  
  while(true) {
    // Compute PSUM
    for(int j = 0; j < ndim; j++) {
      double sum1 = 0.0;
      for(int i = 0; i < mp; i++) {
        sum1 += PMAT(i, j);
      }
      PSUM[j] = sum1;
    }
    
    // Find ILO, IHI, INHI
    int ilo = 0;
    int ihi, inhi;
    
    if(YVEK[0] > YVEK[1]) {
      ihi = 0;
      inhi = 1;
    } else {
      ihi = 1;
      inhi = 0;
    }
    
    for(int i = 0; i < mp; i++) {
      if(YVEK[i] <= YVEK[ilo]) ilo = i;
      if(YVEK[i] > YVEK[ihi]) {
        inhi = ihi;
        ihi = i;
      } else if(YVEK[i] > YVEK[inhi]) {
        if(i != ihi) inhi = i;
      }
    }
    
    // Check convergence
    double rtol = 2.0 * fabs(YVEK[ihi] - YVEK[ilo]) / 
                  (fabs(YVEK[ihi]) + fabs(YVEK[ilo]) + TINY);
    
    if(rtol < FTOL) {
      // Put best point in slot 0
      double swap = YVEK[0];
      YVEK[0] = YVEK[ilo];
      YVEK[ilo] = swap;
      
      for(int j = 0; j < ndim; j++) {
        swap = PMAT(0, j);
        PMAT(0, j) = PMAT(ilo, j);
        PMAT(ilo, j) = swap;
      }
      
      break;
    }
    
    // Check iteration limit
    if(iter >= ITMAX) {
      erro = 1;
      break;
    }
    
    iter += 2;
    
    // Try reflection
    double ytry = amotry(PMAT, YVEK, PSUM, ihi, -1.0,
                              X, Y, M, MU, LAMBDA, CONST1, CONST2, U, V);
    
    if(ytry <= YVEK[ilo]) {
      // Try expansion
      ytry = amotry(PMAT, YVEK, PSUM, ihi, 2.0,
                        X, Y, M, MU, LAMBDA, CONST1, CONST2, U, V);
    } else if(ytry >= YVEK[inhi]) {
      // Try contraction
      double ysave = YVEK[ihi];
      ytry = amotry(PMAT, YVEK, PSUM, ihi, 0.5,
                        X, Y, M, MU, LAMBDA, CONST1, CONST2, U, V);
      
      if(ytry >= ysave) {
        // Shrink simplex
        for(int i = 0; i < mp; i++) {
          if(i != ilo) {
            for(int j = 0; j < ndim; j++) {
              PSUM[j] = 0.5 * (PMAT(i, j) + PMAT(ilo, j));
              PMAT(i, j) = PSUM[j];
            }
            YVEK[i] = alfunc(PSUM, X, Y, M, MU, LAMBDA, 
                                 CONST1, CONST2, U, V);
          }
        }
        iter += ndim;
      }
    } else {
      iter -= 1;
    }
  }
  
  return List::create(Named("PMAT") = PMAT,
                     Named("YVEK") = YVEK,
                     Named("ITER") = iter,
                     Named("ERRO") = erro);
}

// ========== MAIN WRAPPER ==========

// [[Rcpp::export]]
List optimize_spline(NumericVector BVEC,
                          NumericMatrix X,
                          NumericVector Y,
                          NumericMatrix M,
                          NumericVector MU,
                          double LAMBDA,
                          double CONST1,
                          double CONST2,
                          int U, int V,
                          double TAU,
                          double FTOL,
                          double TINY,
                          int ITMAX) {
  
  int p = X.ncol();
  int andim = (U + V) * p;
  int erro = 0;
  
  // ========== STAGE 1 ==========
  
  // Create simplex
  NumericMatrix AP1 = varset(BVEC, U, V, p, TAU);
  
  // Evaluate at each vertex
  NumericVector AY1(andim + 1);
  for(int i = 0; i <= andim; i++) {
    NumericVector tempvk(andim);
    for(int j = 0; j < andim; j++) {
      tempvk[j] = AP1(i, j);
    }
    AY1[i] = alfunc(tempvk, X, Y, M, MU, LAMBDA, CONST1, CONST2, U, V);
  }
  
  // Run amoeba
  List result1 = amoeba(AP1, AY1, FTOL, ITMAX, 
                            X, Y, M, MU, LAMBDA, CONST1, CONST2, U, V, TINY);
  
  NumericMatrix PMAT1 = result1["PMAT"];
  NumericVector YVEK1 = result1["YVEK"];
  int iter1 = result1["ITER"];
  erro = result1["ERRO"];
  
  if(erro != 0) {
    NumericVector bvec_out(andim);
    for(int j = 0; j < andim; j++) {
      bvec_out[j] = PMAT1(0, j);
    }
    
    return List::create(Named("LOSS") = YVEK1[0],
                       Named("BVEC") = bvec_out,
                       Named("LOSS1") = YVEK1[0],
                       Named("ITER1") = iter1,
                       Named("ITER") = 0,
                       Named("ERRO") = erro);
  }
  
  double loss1 = YVEK1[0];
  
  // ========== STAGE 2 ==========
  
  // Extract best from stage 1
  NumericVector BVEC2(andim);
  for(int j = 0; j < andim; j++) {
    BVEC2[j] = PMAT1(0, j);
  }
  
  // Create new simplex
  NumericMatrix AP2 = varset(BVEC2, U, V, p, TAU);
  
  // Evaluate at each vertex
  NumericVector AY2(andim + 1);
  for(int i = 0; i <= andim; i++) {
    NumericVector tempvk(andim);
    for(int j = 0; j < andim; j++) {
      tempvk[j] = AP2(i, j);
    }
    AY2[i] = alfunc(tempvk, X, Y, M, MU, LAMBDA, CONST1, CONST2, U, V);
  }
  
  // Run amoeba again
  List result2 = amoeba(AP2, AY2, FTOL, ITMAX,
                            X, Y, M, MU, LAMBDA, CONST1, CONST2, U, V, TINY);
  
  NumericMatrix PMAT2 = result2["PMAT"];
  NumericVector YVEK2 = result2["YVEK"];
  int iter2 = result2["ITER"];
  erro = result2["ERRO"];
  
  double loss = YVEK2[0];
  
  // Extract final BVEC
  NumericVector bvec_out(andim);
  for(int j = 0; j < andim; j++) {
    bvec_out[j] = PMAT2(0, j);
  }
  
  return List::create(Named("LOSS") = loss,
                     Named("BVEC") = bvec_out,
                     Named("LOSS1") = loss1,
                     Named("ITER1") = iter1,
                     Named("ITER") = iter2,
                     Named("ERRO") = erro);
}





// ========== ADDITIONAL FUNCTIONS (NOT USED IN OPTIMIZER) ==========

//These two functions (mipd and inddup) are not used in the Nelder–Mead algorithm or in biplot.spline.axis. They are rewritten to align with the original Fortran code (although they may be used elsewhere in the library).

double mipd(NumericVector X, NumericVector Y) {
  int n = X.length();
  double d = 0.0;
  int count = 0;
  
  for(int i = 1; i < n; i++) {
    for(int j = 0; j < i; j++) {
      double dx = X[i] - X[j];
      double dy = Y[i] - Y[j];
      d += sqrt(dx * dx + dy * dy);
      count++;
    }
  }
  
  return d / count;
}


LogicalVector inddup(NumericVector X, NumericVector Y, 
                          NumericVector RW, double FRAC) {
  int n = X.length();
  double xtol = FRAC * (RW[1] - RW[0]);
  double ytol = FRAC * (RW[3] - RW[2]);
  
  LogicalVector DUP(n);
  DUP[0] = false;
  
  for(int i = 1; i < n; i++) {
    DUP[i] = false;
    for(int j = 0; j < i; j++) {
      double dx = fabs(X[i] - X[j]);
      double dy = fabs(Y[i] - Y[j]);
      
      if(dx < xtol && dy < ytol) {
        DUP[i] = true;
        break;
      }
    }
  }
  
  return DUP;
}