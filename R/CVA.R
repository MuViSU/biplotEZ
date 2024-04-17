# -------------------------------------------------------------------------------------------
#' Canonical Variate Analysis (CVA) method
#'
#' @description
#' This function produces a list of elements to be used for CVA biplot construction.
#'
#' @param bp object of class \code{biplot} obtained from preceding function \code{biplot()}.
#' @param dim.biplot dimension of the biplot. Only values 1, 2 and 3 are accepted, with default \code{2}.
#' @param e.vects which eigenvectors (canonical variates) to extract, with default \code{1:dim.biplot}.
#' @param classes vector of the same length as the number of rows in the data matrix
#'                  with the class indicator for the samples.
#' @param weightedCVA the default is "weighted", specifying a weighted CVA to be performed. Other possible values are "unweightedI" and "unweightedCent".
#' @param show.class.means logical, indicating whether to plot the class means on the biplot.
#'
#' @param low.dim if the dimension of the canonical space is smaller than `dim.biplot`, the method to use to construct
#'                   additional dimension(s). Currently two options are implemented: `sample.opt` maximises the sample predictivity
#'                   of the individual samples in the biplot and `Bhattacharyya.dist` is based on the decomposition of the
#'                   Bhattacharyya distance into a component for the sample means and a component for the dissimilarity
#'                   between the sample covariance matrices.
#'
#' @return  Object of class CVA with the following elements:
#' \item{X}{matrix of the centered and scaled numeric variables.}
#' \item{Xcat}{matrix of the categorical variables.}
#' \item{raw.X}{original data.}
#' \item{na.action}{vector of observations that have been removed.}
#' \item{center}{TRUE or FALSE, whether X is centred.}
#' \item{scaled}{TRUE or FALSE, whether X is scaled.}
#' \item{means}{mean of each numerical variable.}
#' \item{sd}{standard deviation of each numerical variable.}
#' \item{n}{number of observations.}
#' \item{p}{number of variables.}
#' \item{group.aes}{vector of the same length as the number of rows in the data matrix for differentiated aesthetics for samples. }
#' \item{g.names}{descriptive name to be used for group labels.}
#' \item{g}{number of groups.}
#' \item{Title}{title of the biplot to be rendered.}
#' \item{Gmat}{indicator matrix for class membership.}
#' \item{Xmeans}{matrix of class means.}
#' \item{Z}{matrix with each row containing the details of the point to be plotted (i.e. coordinates).}
#' \item{Zmeans}{matrix of canonical means.}
#' \item{Lmat}{matrix for transformation to the canonical space.}
#' \item{eigenvalues}{vector of eigenvalues of the two-sided eigenvalue problem.}
#' \item{Cmat}{Centring matrix based on different choices of weighting.
#'             For \code{"weighted"}, \code{Cmat} is a diagonal matrix with
#'             the class sizes, for \code{"unweightedI"}, \code{Cmat} is an
#'             indicator matrix and for
#'             \code{"unweightedCent"}, \code{Cmat} is the usual centring matrix. }
#' \item{Bmat}{Between class sums of squares and cross products matrix.}
#' \item{Wmat}{Within class sums of squares and corss products matrix.}
#' \item{e.vects}{vector indicating which canonical variates are plotted in the biplot.}
#' \item{ax.one.unit}{one unit in the positive direction of each biplot axis.}
#' \item{class.means}{logical value, indicating whether the class means should be plotted in the biplot.}
#' \item{Zmeans}{matrix of the class mean coordinates to be plotted in the biplot.}
#'
#' @usage CVA(bp, dim.biplot = c(2, 1, 3), e.vects = 1:ncol(bp$X),
#'            classes=bp$classes, weightedCVA = "weighted", show.class.means = TRUE,
#'            low.dim = "sample.opt")
#' @aliases CVA
#'
#' @export
#'
#' @examples
#' biplot(iris[,1:4]) |> CVA(classes=iris[,5])
#' # create a CVA biplot
#' biplot(iris[,1:4]) |> CVA(classes=iris[,5]) |> plot()

CVA <- function(bp, dim.biplot = c(2,1,3), e.vects = 1:ncol(bp$X), classes=bp$classes,
                weightedCVA = "weighted", show.class.means = TRUE, low.dim = "sample.opt")
{
  UseMethod("CVA")
}

#' CVA biplot
#'
#' @description Performs calculations for a CVA biplot.
#'
#' @inheritParams CVA
#'
#' @return an object of class CVA, inherits from class biplot.
#' @export
#'
#' @examples
#' biplot(iris[,1:4]) |> CVA(classes=iris[,5])
#'
CVA.biplot <- function(bp, dim.biplot = c(2,1,3), e.vects = 1:ncol(bp$X), classes=bp$classes,
                       weightedCVA = "weighted", show.class.means = TRUE, low.dim="sample.opt")
{
  dim.biplot <- dim.biplot[1]
  if (dim.biplot != 1 & dim.biplot != 2 & dim.biplot != 3) stop("Only 1D, 2D and 3D biplots")
  e.vects <- e.vects[1:dim.biplot]

  if (is.null(classes)) stop ("You have to specify the class variable in argument classes.")
  else
  {
    classes <- factor(classes)
    bp$classes <- classes
  }

  if (bp$g == 1)
  {
    bp$group.aes <- factor(classes)
    bp$g.names <- levels(factor(classes))
    bp$g <- length(bp$g.names)
  }

  X <- bp$X
  n <- bp$n
  p <- bp$p
  G <- indmat(classes)
  J <- ncol(G)
  K <- min(p, J-1)

  N <- t(G) %*% G
  X_bar <- solve(N) %*% t(G) %*% X
  W <- t(X) %*% X - t(X_bar) %*% N %*% X_bar
  B <- t(X_bar) %*% N %*% X_bar
  
  svd.W <- svd(W)
  W_minhalf <- svd.W$u %*% diag(1/sqrt(svd.W$d)) %*% t(svd.W$v)
  if (weightedCVA == "weighted")
    Cmat <- N
  if (weightedCVA == "unweightedI")
    Cmat <- diag(J)
  if (weightedCVA == "unweightedCent")
    Cmat <- diag(J) - matrix(1/J, nrow = J, ncol = J)
  if (is.na(match(weightedCVA, c("weighted", "unweightedI", "unweightedCent"))))
    stop(" Argument 'weightedCVA' must be one of 'weighted','unweightedI','unweightedCent' ")
  svd.out <- svd(W_minhalf %*% t(X_bar) %*% Cmat %*% X_bar %*% W_minhalf)
  V <- svd.out$v
  lambdamat <- diag(svd.out$d)
  M <- W_minhalf %*% V 
  Minv <- solve(M)[e.vects, , drop = F]
  
  if (K == 1 & dim.biplot > 1) 
    {  warning (paste("The dimension of the canonical space < dim.biplot", low.dim, "method used for additional dimension(s)"))
       out <- CVA.2class (bp, G, W, M, low.dim)
       Mr <- out$Mr
       bp$Lmat <- NULL
       bp$eigenvalues <- NULL
       Minv <- out$Mrr
    }
    else
    {
       Mr <- M[,e.vects]
       bp$Lmat <- M
       bp$eigenvalues <- svd.out$d
    }

  bp$Z <- X %*% Mr
  bp$ax.one.unit <- 1/(diag(t(Minv) %*% Minv)) * t(Minv)

  bp$Xbar <- X_bar
  bp$Gmat <- G
  bp$Xmeans <- X_bar
  bp$Zmeans <- bp$Xmeans %*% Mr
  bp$e.vects <- e.vects
  bp$Cmat <- Cmat
  bp$Bmat <- B
  bp$Wmat <- W
  bp$Mrr <- Minv
  bp$Mr <- Mr
  bp$Nmat <- N
  bp$lambda.mat <- diag(bp$eigenvalues[e.vects])
  bp$class.means <- show.class.means
  bp$dim.biplot <- dim.biplot

  class(bp) <- append(class(bp),"CVA")
  bp
}

#' CVA extra dimensions if dim(canonical space) < dimension of the biplot
#'
#' @param bp object of class `biplot`
#' @param G indicator matrix of class membership
#' @param W within class covariance matrix
#' @param Mmat eigenvector matrix from CVA
#' @param low.dim if the dimension of the canonical space is smaller than `dim.biplot`, the method to use to construct
#'                   additional dimension(s). Currently two options are implemented: `sample.opt` maximises the sample predictivity
#'                   of the individual samples in the biplot and `Bhattacharyya.dist` is based on the decomposition of the
#'                   Bhattacharyya distance into a component for the sample means and a component for the dissimilarity
#'                   between the sample covariance matrices.
#'
#' @return A list with three components:
#' \item{Mr}{pxr matrix for the transformation to the canonical space.}
#' \item{Mrr}{rxp matrix for prediction from the canonical space.}
#' \item{Lmat}{pxp matrix for transformation to the canonical space.}
CVA.2class <- function (bp, G, W, Mmat, low.dim)
{
  svd.Wmat <- svd(W)
  lambdamatI <- diag(svd.Wmat$d)
  Lmat <- svd.Wmat$u %*% solve(sqrt(lambdamatI))
  
  if (!is.na(pmatch(low.dim,"sample.opt")))
  {
    # Calculate fvek.opt
    Minv <- solve(Mmat)
    M.sup2 <- Minv[-1,]
    fvek.opt <- svd(M.sup2 %*% t(M.sup2))$v[,1]
    # Note that fvek.opt is a linear combination of the columns of M.sup2
    fvek.opt <- matrix(fvek.opt, ncol=1)
    f.mat <- cbind(c(1, rep(0, bp$p-1)), rbind(0,fvek.opt))
    D2 <- Mmat %*% f.mat
    D22 <- t(f.mat) %*% solve(Mmat)
    M2.opt <- Mmat[,2:bp$p] %*% fvek.opt
    #Linear combination of last p-1 columns of B
    M.opt <- cbind(Mmat[,1], M2.opt)
    
    uit.temp4 <- NULL
    uit.temp5 <- NULL
    
    X.hat <- bp$X %*% D2 %*% D22
    X.hats <- vector('list', bp$p+1)
    X.hats[[1]] <- X.hat
    X.hats[[2]] <- bp$X %*% Mmat %*% diag(c(1,rep(0,bp$p-1))) %*% Minv
    tempJ <- cbind (1, diag(bp$p-1))
    
    for(i in 1:(bp$p-1))
      X.hats[[i+2]] <- bp$X %*% Mmat %*% diag(tempJ[i,]) %*% Minv
    names(X.hats) <- c('X.hat.opt', 'X.hat.1', paste('X.hat.1',2:bp$p,sep='_'))
    
    IminH.mat <- diag(bp$n) - G %*% solve(t(G) %*% G) %*% t(G)
    left.part <- IminH.mat %*% bp$X
    RL.part <- lapply(X.hats,function(x) IminH.mat %*% x)
    right.part <- lapply(X.hats,function(x) IminH.mat %*% (bp$X-x))
    first.eq <- list(Left = left.part, RightL = RL.part, RightR = right.part)
    
    uit.temp4 <- lapply(X.hats, function(x) sum (diag ((bp$X-x) %*% solve(W) %*%t(bp$X-x))) / sum((bp$X%*%Lmat)^2) )
    uit.temp5 <- lapply(X.hats,function(x) sum (diag ((bp$X-x) %*% t(bp$X-x))) / sum(bp$X^2) )
    Mr <- M.opt
    Mrr <- D22
    Lmat <- NULL
  }
  
  if (!is.na(pmatch(low.dim,"Bhattacharyya.dist")))
  {
    #X1 First group and X2 the second group
    X1.cent <- bp$X[as.logical(G[,1]),]
    X2.cent <- bp$X[as.logical(G[,2]),]
    y1vec <- bp$X %*% Mmat[,1]  
    n1 <- nrow(X1.cent)
    n2 <- nrow(X2.cent)
    M.star <- Mmat[,-1]
    
    S1.star <- t(M.star) %*% stats::cov(X1.cent) %*% M.star
    S2.star <- t(M.star) %*% stats::cov(X2.cent) %*% M.star
    svd2min1.1 <- svd(solve(S2.star) %*% S1.star)
    svd1min1.2 <- svd(solve(S1.star) %*% S2.star)
    TMP <-solve(S2.star) %*% S1.star
    TMPeigen <- eigen(TMP)
    A.unordered <- TMPeigen$vectors
    rev <- TMPeigen$values + 1/TMPeigen$values + 2
    A.ordered <- A.unordered[,order(rev, decreasing=TRUE)]
    K.mat <- cbind(Mmat[,1], M.star %*%A.ordered)
    vec.temp <- rep(0, bp$p)
    scaffoldaxs <- bp$e.vects
    vec.temp[scaffoldaxs] <- 1
    Jmat <- diag(vec.temp)
    X.cent.hat.bhat <- bp$X %*% K.mat %*% Jmat %*% solve(K.mat)
    standTSREM.bhat  <- sum((bp$X - X.cent.hat.bhat)^2)	 / sum(bp$X^2)
    Mr <- K.mat[ ,1:2]
    Mrr <- solve(K.mat)[1:2, ]
    Lmat <- K.mat
    
  }
  
  list (Mr = Mr, Mrr = Mrr, Lmat = Lmat)
}

