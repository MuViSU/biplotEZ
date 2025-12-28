library(Rcpp)
library(splines)
#chang this according to your path
setwd("C:\\Users\\29827094\\Documents\\GitHub\\biplotEZ")
# Compile Rcpp code (has alfunc_rcpp and optimize_spline_rcpp)
sourceCpp("src\\LnjTinyNew.cpp")

# Load FORTRAN code
dyn.load("tests\\temp\\fortran.dll")

# ========================================
# I fixed all the randomness in this code (using seed), so you should see a similar result for Rcpp and Fortran.
# ========================================

biplot.spline.axis <- function(j, X, Y, means, sd, n.int, 
                                spline.control, dmeth=0, 
                                optim_method = "rcpp",
                                fix_seed = TRUE,  # NEW PARAMETER
                                base_seed = NULL, # NEW PARAMETER
                                sampling_seed = NULL, # NEW: separate seed for sampling
                                ...) {
  

  n <- nrow(X)
  p <- ncol(X)
  
  # Set seed BEFORE sampling if provided
  #new
  if (!is.null(sampling_seed)) {
    set.seed(sampling_seed)
  }
  #I dont know why we have this:
  if (n > 103) {  
    my.sample <- sample(1:n, size=103, replace=F)
    X <- X[my.sample,]
    Y <- Y[my.sample,]
    n <- nrow(X)
  }
  
  # Extract control parameters
  tau <- spline.control$tau
  nmu <- spline.control$nmu
  u <- spline.control$u
  v <- spline.control$v
  lambda <- spline.control$lambda
  smallsigma <- spline.control$smallsigma
  bigsigma <- spline.control$bigsigma
  gamma <- spline.control$gamma
  bigsigmaactivate <- spline.control$bigsigmaactivate
  eps <- spline.control$eps
  tiny <- spline.control$tiny
  itmax <- spline.control$itmax
  ftol <- spline.control$ftol
  
  cat("Calculating spline axis for variable", j, "using", optim_method, "\n")
  
  if(dmeth==1) stop("dmeth should be equal to zero or integer greater than 1 \n")  
  
  Ytilde <- scale(scale(Y, center=FALSE, scale=1/sd), 
                  center=-1*means, scale=FALSE)
  
  ytilde <- Ytilde[,j]
  mutilde <- seq(from=min(ytilde), to=max(ytilde), length.out=nmu)
  y <- Y[,j]
  rangey <- max(y) - min(y)
  mu <- seq(from=min(y)-.3*rangey, to=max(y)+.3*rangey, length.out=nmu)
  markers <- (pretty(ytilde) - means[j]) / sd[j]
  mu <- sort(c(mu, markers))
  mu <- unique(mu)
  nmu <- length(mu)
  
  if (v > 0) {
    knots <- seq.int(from=0, to=1, length.out=v+2)[-c(1, v+2)]
    knots <- stats::quantile(y, knots)
    M <- splines::bs(mu, knots=knots, degree=u, intercept=FALSE)
  } else {
    M <- splines::bs(mu, df=u+v, degree=u, intercept=FALSE)
  }
  
  M <- scale(M, scale=FALSE, center=M[which.min(abs(mu)),])
  Breg <- t(solve(t(X)%*%X)%*%t(X)%*%y)
  Zreg <- mu%*%Breg / sum(Breg^2)
  Bvec <- as.vector(solve(t(M)%*%M)%*%t(M)%*%Zreg)
  
  const1 <- sum(y^2)
  const2 <- sum(X^2) / (n*p)
  
  # ========================================
  # OPTIMIZATION WRAPPER
  # ========================================
  
  optimtouse <- function(Bvec) {
    timetemp <- proc.time()[3]
    
    if (optim_method == "rcpp") {
      # Use Rcpp two-stage
      returned_data <- optimize_spline(
        BVEC = Bvec, X = X, Y = y, M = M, MU = mu,
        LAMBDA = lambda, CONST1 = const1, CONST2 = const2,
        U = u, V = v, TAU = tau, FTOL = ftol, TINY = tiny, ITMAX = itmax
      )
      
      if(returned_data$ERRO > 0) {
        cat("  Error code:", returned_data$ERRO, "\n")
      }
      
      aa <- list(
        BestValue = returned_data$LOSS,
        BestSolution = returned_data$BVEC,
        ConvergenceCode = returned_data$ERRO,
        iter1 = returned_data$ITER1,
        iter = returned_data$ITER,
        TimeTaken = proc.time()[3] - timetemp
      )
      
    } else if (optim_method == "fortran") {
      # Use FORTRAN two-stage
      returned_data <- .Fortran("l",
        LOSS = as.double(0),
        X = as.double(X),
        N = as.integer(n),
        P = as.integer(p),
        NMU = as.integer(nmu),
        IND = as.integer(rep(0, n)),
        MU = as.double(mu),
        PRED = as.double(rep(0, n)),
        LAMBDA = as.double(lambda),
        Y = as.double(y),
        CONST1 = as.double(const1),
        CONST2 = as.double(const2),
        U = as.integer(u),
        V = as.integer(v),
        M = as.double(M),
        BVEC = as.double(Bvec),
        TAU = as.double(tau),
        AY = as.double(rep(0, (u+v)*p+1)),
        TEMPVK = as.double(rep(0, (u+v)*p)),
        ITER = as.integer(0),
        FTOL = as.double(ftol),
        LOSS1 = as.double(0),
        ITER1 = as.integer(0),
        ERRO = as.integer(0),
        CONST3 = as.double(tiny),
        ITMAX = as.integer(itmax)
      )
      
      if(returned_data$ERRO > 0) {
        cat("  Error code:", returned_data$ERRO, "\n")
      }
      
      aa <- list(
        BestValue = returned_data$LOSS,
        BestSolution = returned_data$BVEC,
        ConvergenceCode = returned_data$ERRO,
        iter1 = returned_data$ITER1,
        iter = returned_data$ITER,
        TimeTaken = proc.time()[3] - timetemp
      )
    }
    
    aa
  }
  
  EuclidDist2 <- function(X, Y) {
    n <- nrow(X)
    m <- nrow(Y)
    bx <- rowSums(X^2)
    by <- rowSums(Y^2)
    outer(bx, by, FUN = "+") - 2 * X %*% t(Y)
  }
  
  # Variable initialization
  outBestValues <- rep(NA, gamma+1)
  outBestSolutions <- matrix(nrow=2*(u+v), ncol=gamma+1)
  outTimeTaken <- rep(NA, gamma+1)
  BestSolutionsFrequency <- rep(NA, gamma+1)
  BestSolutionsIndices <- rep(NA, gamma+1)
  
  # ========================================
  # PRE-GENERATE ALL RANDOM PERTURBATIONS
  #new
  # ========================================
  if (fix_seed && !is.null(base_seed)) {
    set.seed(base_seed)
  }
  
  # Pre-generate random perturbations for all gamma iterations
  random_perturbations <- list()
  for (gammacounter in 2:(gamma+1)) {
    # Generate perturbation for this iteration
    # We don't know yet if it will use smallsigma or bigsigma,
    # so we pre-generate both and decide later
    random_perturbations[[gammacounter]] <- list(
      small = stats::rnorm((u+v)*2, mean=0, sd=smallsigma),
      big = stats::rnorm((u+v)*2, mean=0, sd=bigsigma)
    )
  }
  
  # Initial coefficients
  temp <- optimtouse(Bvec)
  outBestValues[1] <- temp$BestValue
  outBestSolutions[,1] <- temp$BestSolution
  outTimeTaken[1] <- temp$TimeTaken
  BestSolutionsFrequency[1] <- 1
  BestSolutionsIndices[1] <- 1
  DistinctSolutions <- 1
  PreviousBestSolution <- NA
  nSameSolutionConsecutively <- 0
  
  # Multi-start optimization
  for (gammacounter in 2:(gamma+1)) {
    # Use pre-generated perturbations
    if (nSameSolutionConsecutively >= bigsigmaactivate) {
      perturbation <- random_perturbations[[gammacounter]]$big
    } else {
      perturbation <- random_perturbations[[gammacounter]]$small
    }
    
    temp <- optimtouse(outBestSolutions[,which.min(outBestValues)] + perturbation)
    
    outTimeTaken[gammacounter] <- temp$TimeTaken
    tempSquaredDistances <- EuclidDist2(matrix(temp$BestSolution, nrow=1),
                                         t(outBestSolutions[,1:DistinctSolutions]))
    
    if (any(tempSquaredDistances < eps)) {
      BestSolutionsIndices[gammacounter] <- tempAA <- which.min(tempSquaredDistances)
      BestSolutionsFrequency[tempAA] <- BestSolutionsFrequency[tempAA] + 1
      if (!is.na(PreviousBestSolution) && tempAA == PreviousBestSolution) {
        nSameSolutionConsecutively <- nSameSolutionConsecutively + 1
      } else {
        PreviousBestSolution <- tempAA
        nSameSolutionConsecutively <- 0
      }
    } else {
      DistinctSolutions <- DistinctSolutions + 1
      outBestValues[DistinctSolutions] <- temp$BestValue
      outBestSolutions[,DistinctSolutions] <- temp$BestSolution
      BestSolutionsFrequency[DistinctSolutions] <- 1
      BestSolutionsIndices[gammacounter] <- DistinctSolutions
      nSameSolutionConsecutively <- 0
    }
  }
  
  axis.points <- cbind(M %*% matrix(outBestSolutions[,which.min(outBestValues)], ncol=2), 
                       mu, 0)
  
  for (i in 1:nrow(axis.points)) {
    if (any(zapsmall(axis.points[i,3] - markers) == 0)) {
      axis.points[i, 4] <- 1
    }
  }
  
  axis.points[,3] <- axis.points[,3] * sd[j] + means[j]
  
  # Return with attributes
  attr(axis.points, "loss") <- min(outBestValues, na.rm=TRUE)
  attr(axis.points, "bvec") <- outBestSolutions[,which.min(outBestValues)]
  attr(axis.points, "total_time") <- sum(outTimeTaken, na.rm=TRUE)
  attr(axis.points, "distinct_solutions") <- DistinctSolutions
  
  axis.points
}

# ========================================
# EXAMPLE 1: CALCULATE LOSS FUNCTION
# ========================================

set.seed(123)

n <- 150
p <- 2
Y <- matrix(rnorm(n * 2), ncol = 2)
Y[, 2] <- Y[, 1] + rnorm(n, 0, 0.5)

means <- colMeans(Y)
sd <- apply(Y, 2, sd)
X <- Y[, 1:2] + matrix(rnorm(n * p, 0, 0.3), ncol = p)

spline.control <- list(
  tau = 0.01, nmu = 10, u = 3, v = 0, lambda = 0.1,
  smallsigma = 0.1, bigsigma = 1.0, gamma = 3, bigsigmaactivate = 2,
  eps = 1e-6, tiny = 1e-4, itmax = 500, ftol = 1e-4
)

j <- 1
y <- Y[, j]
rangey <- max(y) - min(y)
mu <- seq(from=min(y)-.3*rangey, to=max(y)+.3*rangey, length.out=10)
M <- splines::bs(mu, df=3, degree=3, intercept=FALSE)
M <- scale(M, scale=FALSE, center=M[which.min(abs(mu)),])

const1 <- sum(y^2)
const2 <- sum(X^2) / (n*p)
lambda <- 0.1
u <- 3
v <- 0

# Test Bvec
Bvec_test <- rnorm((u+v)*p)

cat("========================================\n")
cat("EXAMPLE 1: LOSS FUNCTION CALCULATION\n")
cat("========================================\n\n")

loss_value <- alfunc(Bvec_test, X, y, M, mu, lambda, const1, const2, u, v)
cat("Test Bvec:", round(Bvec_test, 4), "\n")
cat("Loss value:", loss_value, "\n\n")

# ========================================
# EXAMPLE 2: COMPARE RCPP VS FORTRAN
# ========================================

cat("========================================\n")
cat("EXAMPLE 2: RCPP VS FORTRAN COMPARISON\n")
cat("WITH FIXED SEED FOR REPRODUCIBILITY\n")
cat("========================================\n\n")

COMPARISON_SEED <- 999  # Use same seed for both methods
SAMPLING_SEED <- 777    # NEW: Separate seed for data sampling

# Method 1: Rcpp
cat("1. Running Rcpp...\n")
time_rcpp <- system.time({
  result_rcpp <- biplot.spline.axis(
    j = 1, X = X, Y = Y, means = means, sd = sd,
    n.int = NULL, spline.control = spline.control,
    dmeth = 0, optim_method = "rcpp",
    fix_seed = TRUE, base_seed = COMPARISON_SEED,
    sampling_seed = SAMPLING_SEED  # NEW: Control sampling
  )
})

# Method 2: FORTRAN
cat("\n2. Running FORTRAN...\n")
time_fortran <- system.time({
  result_fortran <- biplot.spline.axis(
    j = 1, X = X, Y = Y, means = means, sd = sd,
    n.int = NULL, spline.control = spline.control,
    dmeth = 0, optim_method = "fortran",
    fix_seed = TRUE, base_seed = COMPARISON_SEED,
    sampling_seed = SAMPLING_SEED  # NEW: Control sampling
  )
})

# ========================================
# COMPARE RESULTS
# ========================================

cat("\n========================================\n")
cat("COMPARISON RESULTS\n")
cat("========================================\n\n")

cat("TIMING (seconds):\n")
cat(sprintf("  %-15s: %7.3f\n", "Rcpp", time_rcpp[3]))
cat(sprintf("  %-15s: %7.3f\n", "FORTRAN", time_fortran[3]))
cat(sprintf("  %-15s: %7.3f\n", "Speedup", time_fortran[3] / time_rcpp[3]))
cat("\n")

cat("LOSS VALUES:\n")
loss_rcpp <- attr(result_rcpp, "loss")
loss_fortran <- attr(result_fortran, "loss")

cat(sprintf("  %-15s: %.12f\n", "Rcpp", loss_rcpp))
cat(sprintf("  %-15s: %.12f\n", "FORTRAN", loss_fortran))
cat("\n")

cat("COEFFICIENT VECTORS (bvec):\n")
bvec_rcpp <- attr(result_rcpp, "bvec")
bvec_fortran <- attr(result_fortran, "bvec")

cat("  Rcpp:    ", round(bvec_rcpp, 6), "\n")
cat("  FORTRAN: ", round(bvec_fortran, 6), "\n")
cat("\n")

cat("DIFFERENCES:\n")
loss_diff <- abs(loss_rcpp - loss_fortran)
bvec_diff <- sqrt(sum((bvec_rcpp - bvec_fortran)^2))

cat(sprintf("  %-25s: %.15f\n", "Loss difference", loss_diff))
cat(sprintf("  %-25s: %.15f\n", "Bvec L2 distance", bvec_diff))
cat("\n")

# Check if results are practically identical
if (loss_diff < 1e-8 && bvec_diff < 1e-8) {
  cat("✓ Results are essentially IDENTICAL!\n\n")
} else if (loss_diff < 1e-6 && bvec_diff < 1e-6) {
  cat("✓ Results are very close (within numerical precision)\n\n")
} else {
  cat("✗ Results differ significantly - investigation needed\n\n")
}

# ========================================
# EXAMPLE 3: TEST REPEATABILITY
# ========================================

cat("========================================\n")
cat("EXAMPLE 3: REPEATABILITY TEST\n")
cat("========================================\n\n")

cat("Running Rcpp twice with same seed:\n")
result_rcpp_1 <- biplot.spline.axis(
  j = 1, X = X, Y = Y, means = means, sd = sd,
  n.int = NULL, spline.control = spline.control,
  dmeth = 0, optim_method = "rcpp",
  fix_seed = TRUE, base_seed = 777,
  sampling_seed = 888  # Add sampling seed
)

result_rcpp_2 <- biplot.spline.axis(
  j = 1, X = X, Y = Y, means = means, sd = sd,
  n.int = NULL, spline.control = spline.control,
  dmeth = 0, optim_method = "rcpp",
  fix_seed = TRUE, base_seed = 777,
  sampling_seed = 888  # Same sampling seed
)

loss_diff <- abs(attr(result_rcpp_1, "loss") - attr(result_rcpp_2, "loss"))
bvec_diff <- sqrt(sum((attr(result_rcpp_1, "bvec") - attr(result_rcpp_2, "bvec"))^2))

cat(sprintf("  Loss difference: %.15f\n", loss_diff))
cat(sprintf("  Bvec difference: %.15f\n", bvec_diff))

if (loss_diff < 1e-10 && bvec_diff < 1e-10) {
  cat("  ✓ Results are identical!\n")
} else {
  cat("  ✗ Results differ (possible numerical precision issues)\n")
}