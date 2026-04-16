

library(Rcpp)
library(splines)
#  chenge this path according to your system
sourceCpp("C:\\Users\\29827094\\Documents\\GitHub\\biplotEZ\\src\\LnjTinyNew.cpp")

# #The following function exactly follows the main function in the library (R/plot2D.R).
# biplot.spline.axis <- function(j, X, Y, means, sd, 
#                                n.int, spline.control, dmeth=0, ...) {
  
#   n <- nrow(X)
#   p <- ncol(X)
#   if (n > 103) {  
#     my.sample <- sample(1:n, size=103, replace=F)
#     X <- X[my.sample,]
#     Y <- Y[my.sample,]
#     n <- nrow(X)
#   }
  
#   tau <- spline.control$tau
#   nmu <- spline.control$nmu
#   u <- spline.control$u
#   v <- spline.control$v
#   lambda <- spline.control$lambda
#   smallsigma <- spline.control$smallsigma
#   bigsigma <- spline.control$bigsigma
#   gamma <- spline.control$gamma
#   bigsigmaactivate <- spline.control$bigsigmaactivate
#   eps <- spline.control$eps
#   tiny <- spline.control$tiny
#   itmax <- spline.control$itmax
#   ftol <- spline.control$ftol
  
#   cat("Calculating spline axis for variable", j)
  
#   if(dmeth==1) stop("dmeth should be equal to zero or integer greater than 1 \n")  
  
#   Ytilde <- scale(scale(Y, center=FALSE, scale=1/sd), 
#                   center=-1*means, scale=FALSE)
  
#   ytilde <- Ytilde[,j]
#   mutilde <- seq(from=min(ytilde), to=max(ytilde), length.out=nmu)
#   y <- Y[,j]
#   rangey <- max(y) - min(y)
#   mu <- seq(from=min(y)-.3*rangey, to=max(y)+.3*rangey, length.out=nmu)
#   markers <- (pretty(ytilde) - means[j]) / sd[j]
#   mu <- sort(c(mu, markers))
#   mu <- unique(mu)
#   nmu <- length(mu)
  
#   if (v > 0) {
#     knots <- seq.int(from=0, to=1, length.out=v+2)[-c(1, v+2)]
#     knots <- stats::quantile(y, knots)
#     M <- splines::bs(mu, knots=knots, degree=u, intercept=FALSE)
#   } else {
#     M <- splines::bs(mu, df=u+v, degree=u, intercept=FALSE)
#   }
  
#   M <- scale(M, scale=FALSE, center=M[which.min(abs(mu)),])
#   Breg <- t(solve(t(X)%*%X)%*%t(X)%*%y)
#   Zreg <- mu%*%Breg / sum(Breg^2)
#   Bvec <- as.vector(solve(t(M)%*%M)%*%t(M)%*%Zreg)
  
#   const1 <- sum(y^2)
#   const2 <- sum(X^2) / (n*p)
  
#   # ========================================
#   #The Fortran call has been replaced by the following Rcpp function
#   #=========================================
#   optimtouse <- function(Bvec) {
#     timetemp <- proc.time()[3]
    
#     # Use Rcpp optimize_spline function
#     returned_data <- optimize_spline(
#       BVEC = Bvec, 
#       X = X, 
#       Y = y, 
#       M = M, 
#       MU = mu,
#       LAMBDA = lambda, 
#       CONST1 = const1, 
#       CONST2 = const2,
#       U = u, 
#       V = v, 
#       TAU = tau, 
#       FTOL = ftol, 
#       TINY = tiny, 
#       ITMAX = itmax
#     )
    
#     if(returned_data$ERRO > 0) {
#       cat("  Warning: Error code =", returned_data$ERRO, "\n")
#     }
    
#     aa <- list(
#       BestValue = returned_data$LOSS,
#       BestSolution = returned_data$BVEC,
#       ConvergenceCode = returned_data$ERRO,
#       iter1 = returned_data$ITER1,
#       iter = returned_data$ITER,
#       TimeTaken = proc.time()[3] - timetemp
#     )
    
#     aa
#   }
  
#   EuclidDist2 <- function(X, Y) {
#     n <- nrow(X)
#     m <- nrow(Y)
#     bx <- rowSums(X^2)
#     by <- rowSums(Y^2)
#     outer(bx, by, FUN = "+") - 2 * X %*% t(Y)
#   }
  
#   # Variable initialization
#   outBestValues <- rep(NA, gamma+1)
#   outBestSolutions <- matrix(nrow=2*(u+v), ncol=gamma+1)
#   outTimeTaken <- rep(NA, gamma+1)
#   BestSolutionsFrequency <- rep(NA, gamma+1)
#   BestSolutionsIndices <- rep(NA, gamma+1)
  
#   # Initial coefficients
#   temp <- optimtouse(Bvec)
#   outBestValues[1] <- temp$BestValue
#   outBestSolutions[,1] <- temp$BestSolution
#   outTimeTaken[1] <- temp$TimeTaken
#   BestSolutionsFrequency[1] <- 1
#   BestSolutionsIndices[1] <- 1
#   DistinctSolutions <- 1
#   PreviousBestSolution <- NA
#   nSameSolutionConsecutively <- 0
  
#   # Multi-start optimization
#   for (gammacounter in 2:(gamma+1)) {
#     if (nSameSolutionConsecutively >= bigsigmaactivate) {
#       temp <- optimtouse(outBestSolutions[,which.min(outBestValues)] + 
#                          stats::rnorm((u+v)*2, mean=0, sd=bigsigma))
#     } else {
#       temp <- optimtouse(outBestSolutions[,which.min(outBestValues)] + 
#                          stats::rnorm((u+v)*2, mean=0, sd=smallsigma))
#     }
    
#     outTimeTaken[gammacounter] <- temp$TimeTaken
#     tempSquaredDistances <- EuclidDist2(matrix(temp$BestSolution, nrow=1),
#                                          t(outBestSolutions[,1:DistinctSolutions]))
    
#     if (any(tempSquaredDistances < eps)) {
#       BestSolutionsIndices[gammacounter] <- tempAA <- which.min(tempSquaredDistances)
#       BestSolutionsFrequency[tempAA] <- BestSolutionsFrequency[tempAA] + 1
#       if (!is.na(PreviousBestSolution) && tempAA == PreviousBestSolution) {
#         nSameSolutionConsecutively <- nSameSolutionConsecutively + 1
#       } else {
#         PreviousBestSolution <- tempAA
#         nSameSolutionConsecutively <- 0
#       }
#     } else {
#       DistinctSolutions <- DistinctSolutions + 1
#       outBestValues[DistinctSolutions] <- temp$BestValue
#       outBestSolutions[,DistinctSolutions] <- temp$BestSolution
#       BestSolutionsFrequency[DistinctSolutions] <- 1
#       BestSolutionsIndices[gammacounter] <- DistinctSolutions
#       nSameSolutionConsecutively <- 0
#     }
#   }
  
#   axis.points <- cbind(M %*% matrix(outBestSolutions[,which.min(outBestValues)], ncol=2), 
#                        mu, 0)
  
#   for (i in 1:nrow(axis.points)) {
#     if (any(zapsmall(axis.points[i,3] - markers) == 0)) {
#       axis.points[i, 4] <- 1
#     }
#   }
  
#   axis.points[,3] <- axis.points[,3] * sd[j] + means[j]
  
#   # Return with attributes
#   attr(axis.points, "loss") <- min(outBestValues, na.rm=TRUE)
#   attr(axis.points, "bvec") <- outBestSolutions[,which.min(outBestValues)]
#   attr(axis.points, "total_time") <- sum(outTimeTaken, na.rm=TRUE)
#   attr(axis.points, "distinct_solutions") <- DistinctSolutions
  
#   axis.points
# }



biplot.spline.axis <- function(j, X, Y, means, sd, 
                               n.int, spline.control, dmeth=0, ... ){
  n <- nrow(X)
  p <- ncol(X)
  if (n > 103){  
      my.sample <- sample (1:n, size=103, replace=F)
      X <- X[my.sample,]
      Y <- Y[my.sample,]
      n <- nrow(X)
    }
  
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
  
  cat ("Calculating spline axis for variable", j, "\n")
  if(dmeth==1) stop("dmeth should be equal to zero or integer greater than 1 \n")  
  Ytilde <- scale(scale(Y, center=FALSE, scale=1/sd), center=-1*means, scale=FALSE)
  
  ytilde <- Ytilde[,j]
  mutilde <- seq(from=min(ytilde),to=max(ytilde),length.out=nmu)
  y <- Y[,j]
  rangey <- max(y)-min(y)
  mu <- seq(from=min(y)-.3*rangey,to=max(y)+.3*rangey,length.out=nmu)
  markers <- (pretty(ytilde)-means[j])/sd[j]
  mu <- sort(c(mu,markers))
  mu <- unique(mu)
  nmu <- length(mu)
  
  if (v>0){
    knots <- seq.int(from=0,to=1,length.out=v+2)[-c(1,v+2)]
    knots <- stats::quantile(y,knots)
    M <- splines::bs(mu,knots=knots,degree=u,intercept=FALSE)
  } else M <- splines::bs(mu,df=u+v,degree=u,intercept=FALSE)
  M <- scale(M,scale=FALSE,center=M[which.min(abs(mu)),]) # To ensure that the spline passes through the origin at the calibration which represents the mean of the variable
  Breg <- t(solve(t(X)%*%X)%*%t(X)%*%y)
  Zreg <- mu%*%Breg/sum(Breg^2)
  Bvec <- as.vector(solve(t(M)%*%M)%*%t(M)%*%Zreg)  # Closest to regression biplot
  
  const1 <- sum(y^2)
  const2 <- sum(X^2)/(n*p)
  TotalNumberOfLossFunctionCalls <- 0
  optimtouse <- function(Bvec) {
    timetemp <- proc.time()[3]
  
      # Use Rcpp optimize_spline function
    returned_data <- optimize_spline(
      BVEC = Bvec, 
      X = X, 
      Y = y, 
      M = M, 
      MU = mu,
      LAMBDA = lambda, 
      CONST1 = const1, 
      CONST2 = const2,
      U = u, 
      V = v, 
      TAU = tau, 
      FTOL = ftol, 
      TINY = tiny, 
      ITMAX = itmax
    )
    
    if(returned_data$ERRO > 0) {
      cat("  Warning: Error code =", returned_data$ERRO, "\n")
    }
    
    aa <- list(
      BestValue = returned_data$LOSS,
      BestSolution = returned_data$BVEC,
      ConvergenceCode = returned_data$ERRO,
      iter1 = returned_data$ITER1,
      iter = returned_data$ITER,
      TimeTaken = proc.time()[3] - timetemp
    )
    
    aa
  }

  EuclidDist2 <- function (X, Y) {
    n <- nrow(X)
    m <- nrow(Y)
    bx <- rowSums(X^2)
    by <- rowSums(Y^2)
    outer(bx, by, FUN = "+") - 2 * X %*% t(Y)
  }
  
  ### Variable initialisation
  outBestValues <- rep(NA,gamma+1)
  outBestSolutions <- matrix(nrow=2*(u+v),ncol=gamma+1)
  outTimeTaken <- rep(NA,gamma+1) # Is made one element longer at each iteration.
  BestSolutionsFrequency <- rep(NA,gamma+1)
  BestSolutionsIndices <- rep(NA,gamma+1) # Is made one element longer at each iteration.
  SquaredDistancesBetweenBestSolutions <- matrix(nrow=gamma+1,ncol=gamma+1)
  
  ### Initial coefficients closest to regression biplot
  temp <- optimtouse(Bvec)
  outBestValues[1] <- temp$BestValue
  outBestSolutions[,1] <- temp$BestSolution
  outTimeTaken[1] <- temp$TimeTaken
  BestSolutionsFrequency[1] <- 1
  BestSolutionsIndices[1] <- 1
  DistinctSolutions <- 1
  PreviousBestSolution <- NA
  nSameSolutionConsecutively <- 0
  BigSigmaActivations <- NULL
  
  test.iter <- temp$iter
  test.iter1 <- temp$iter1
  
  ### Last best coefficients perturbed
  for (gammacounter in 2:(gamma+1)){
    if (nSameSolutionConsecutively>=bigsigmaactivate){
      temp <- optimtouse(outBestSolutions[,which.min(outBestValues)]+stats::rnorm((u+v)*2,mean=0,sd=bigsigma))
      BigSigmaActivations <- c(BigSigmaActivations,gammacounter)
    }
    else temp <- optimtouse(outBestSolutions[,which.min(outBestValues)]+stats::rnorm((u+v)*2,mean=0,sd=smallsigma))
    outTimeTaken[gammacounter] <- temp$TimeTaken
    tempSquaredDistances <- EuclidDist2(matrix(temp$BestSolution,nrow=1),t(outBestSolutions[,1:DistinctSolutions]))
    if (any(tempSquaredDistances<eps)){
      BestSolutionsIndices[gammacounter] <- tempAA<-which.min(tempSquaredDistances)
      BestSolutionsFrequency[tempAA] <- BestSolutionsFrequency[tempAA]+1
      if (!is.na(PreviousBestSolution) && tempAA==PreviousBestSolution) nSameSolutionConsecutively<-nSameSolutionConsecutively+1
      else{
        PreviousBestSolution <- tempAA
        nSameSolutionConsecutively <- 0
      }
    }
    else{
      DistinctSolutions <- DistinctSolutions+1
      outBestValues[DistinctSolutions] <- temp$BestValue
      outBestSolutions[,DistinctSolutions] <- temp$BestSolution
      BestSolutionsFrequency[DistinctSolutions] <- 1
      BestSolutionsIndices[gammacounter] <- DistinctSolutions
      SquaredDistancesBetweenBestSolutions[1:(DistinctSolutions-1),DistinctSolutions]<-tempSquaredDistances
      nSameSolutionConsecutively <- 0
    }
  }
  axis.points <- cbind(M%*%matrix(outBestSolutions[,which.min(outBestValues)],ncol=2), mu, 0)
  
  for (i in 1:nrow(axis.points)) if (any(zapsmall(axis.points[i,3] - markers) == 0)) axis.points[i, 4] <- 1
  axis.points[,3] <- axis.points[,3]*sd[j] + means[j]
  axis.points
}









set.seed(123)

n <- 180
p <- 2 


n_vars <- 4
Y <- matrix(rnorm(n * n_vars), ncol = n_vars)

Y[, 2] <- Y[, 1] + rnorm(n, 0, 0.5)
Y[, 3] <- -Y[, 1] + rnorm(n, 0, 0.5)
Y[, 4] <- Y[, 2] + Y[, 3] + rnorm(n, 0, 0.5)

means <- colMeans(Y)
sd <- apply(Y, 2, sd)

X <- Y[, 1:2] + matrix(rnorm(n * p, 0, 0.3), ncol = p)

u <- 3
v <- 2
spline.control <- list(
  tau = 0.01, nmu = 10, u = u, v = v, lambda = 0.1,
  smallsigma = 0.1, bigsigma = 1.0, gamma = 3, bigsigmaactivate = 2,
  eps = 1e-6, tiny = 1e-4, itmax = 1000, ftol = 1e-4
)

j <- 1
y <- Y[, j]
rangey <- max(y) - min(y)
mu <- seq(from=min(y)-.3*rangey, to=max(y)+.3*rangey, length.out=50)
M <- splines::bs(mu, df=3, degree=3, intercept=FALSE)
M <- scale(M, scale=FALSE, center=M[which.min(abs(mu)),])

const1 <- sum(y^2)
const2 <- sum(X^2) / (n*p)
lambda <- 0.1


# Test Bvec
Bvec_test <- rnorm((u+v)*p)

result_optim <- biplot.spline.axis(
  j = 1, X = X, Y = Y, means = means, sd = sd,
  n.int = NULL, spline.control = spline.control,
  dmeth = 0
)

loss <- attr(result_optim, "loss")


par(mfrow = c(1, 1), mar = c(4, 4, 3, 1))

plot(X[, 1], X[, 2], pch = 19, col = "gray70",
     xlab = "Dim 1", ylab = "Dim 2",
     main = sprintf("R optim (Loss=%.4f)", loss), asp = 1)
grid()
lines(result_optim[, 1], result_optim[, 2], col = "darkgreen", lwd = 2)


#All variables

all_results <- list()
all_losses <- numeric(n_vars)

for(j in 1:n_vars) {
  cat("\nProcessing variable", j, "...\n")
  all_results[[j]] <- biplot.spline.axis(
    j = j, X = X, Y = Y, means = means, sd = sd,
    n.int = NULL, spline.control = spline.control, dmeth = 0
  )
  all_losses[j] <- attr(all_results[[j]], "loss")
  cat("  Loss:", all_losses[j], "\n")
  cat("  Distinct solutions:", attr(all_results[[j]], "distinct_solutions"), "\n")
}


par(mfrow = c(1, 1), mar = c(4, 4, 3, 1))
plot(X[, 1], X[, 2], 
     pch = 19, col = "gray70",
     xlab = "Dimension 1", ylab = "Dimension 2",
     main = "All Spline Axes (optim method)",
     asp = 1)
grid()

colors <- c("blue", "red", "green", "purple")
for(j in 1:n_vars) {
  lines(all_results[[j]][, 1], all_results[[j]][, 2], 
        col = colors[j], lwd = 2)
  # Label the axis
  mid <- ceiling(nrow(all_results[[j]]) / 2)
  text(all_results[[j]][mid, 1], all_results[[j]][mid, 2],
       paste("Var", j), pos = 3, col = colors[j], font = 2)
}

legend("topright", legend = paste("Variable", 1:n_vars),
       col = colors, lwd = 2, cex = 0.8)









