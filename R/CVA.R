# -------------------------------------------------------------------------------------------
#' Canonical Variate Analysis (CVA) method
#'
#' @description
#' This function produces a list of elements to be used for CVA biplot construction.
#'
#' @param bp object of class \code{biplot} obtained from preceding function \code{biplot()}.
#' @param classes vector of the same length as the number of rows in the data matrix
#'                  with the class indicator for the samples.
#' @param dim.biplot dimension of the biplot. Only values 1, 2 and 3 are accepted, with default \code{2}.
#' @param e.vects which eigenvectors (canonical variates) to extract, with default \code{1:dim.biplot}.
#' @param weightedCVA the default is "weighted", specifying a weighted CVA to be performed. Other possible values are "unweightedI" and "unweightedCent".
#' @param show.class.means logical, indicating whether to plot the class means on the biplot.
#'
#' @param low.dim if the dimension of the canonical space is smaller than \code{dim.biplot}, the method to use to construct
#'                   additional dimension(s). Currently two options are implemented: \code{sample.opt} maximises the sample predictivity
#'                   of the individual samples in the biplot and \code{Bhattacharyya.dist} is based on the decomposition of the
#'                   Bhattacharyya distance into a component for the sample means and a component for the dissimilarity
#'                   between the sample covariance matrices.
#'
#' @return  Object of class CVA with the following elements:
#' \item{X}{matrix of the centered and scaled numeric variables.}
#' \item{Xcat}{matrix of the categorical variables.}
#' \item{raw.X}{original data.}
#' \item{classes}{vector of category levels for the class variable. This is to be used for colour, pch and cex specifications.}
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
#' \item{Lmat}{matrix for transformation to the canonical space.}
#' \item{Linv}{inverse of the \code{Lmat} matrix.}
#' \item{eigenvalues}{vector of eigenvalues of the two-sided eigenvalue problem.}
#' \item{Z}{matrix with each row containing the details of the point to be plotted (i.e. coordinates).}
#' \item{ax.one.unit}{one unit in the positive direction of each biplot axis.}
#' \item{Xbar}{the matrix of group means.}
#' \item{Gmat}{indicator matrix defining membership of the classes.}
#' \item{Xmeans}{matrix of class means.}
#' \item{Zmeans}{matrix of the class mean coordinates to be plotted in the biplot.}
#' \item{e.vects}{vector indicating which canonical variates are plotted in the biplot.}
#' \item{Cmat}{centring matrix based on different choices of weighting.
#'             For \code{"weighted"}, \code{Cmat} is a diagonal matrix with
#'             the class sizes, for \code{"unweightedI"}, \code{Cmat} is an
#'             indicator matrix and for
#'             \code{"unweightedCent"}, \code{Cmat} is the usual centring matrix. }
#' \item{Bmat}{between class sums of squares and cross products matrix.}
#' \item{Wmat}{within class sums of squares and cross products matrix.}
#' \item{Mrr}{the inverse of the matrix \eqn{M = LV}.}
#' \item{Mr}{the first r dimensions of the solution to be plotted.}
#' \item{Nmat}{a matrix with the class sizes on the diagonal.}
#' \item{lambda.mat}{the matrix with the eigenvalues of \eqn{W^{-1/2}BW^{-1/2}} on the diagonal.}
#' \item{class.means}{logical value, indicating whether the class means should be plotted in the biplot.}
#' \item{dim.biplot}{dimension of the biplot.}
#' \item{low.dim}{if the dimension of the canonical space is smaller than \code{dim.biplot}, the method used to construct
#'                   additional dimension(s).}
#'
#' @usage CVA(bp, classes=bp$classes, dim.biplot = c(2, 1, 3), e.vects = 1:ncol(bp$X),
#'            weightedCVA = "weighted", show.class.means = TRUE,
#'            low.dim = "sample.opt")
#' @aliases CVA
#'
#' @export
#'
#' @examples
#' biplot(iris[,1:4]) |> CVA(classes=iris[,5])
#' # create a CVA biplot
#' biplot(iris[,1:4]) |> CVA(classes=iris[,5]) |> plot()

CVA <- function(bp, classes=bp$classes, dim.biplot = c(2,1,3), e.vects = 1:ncol(bp$X), 
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
CVA.biplot <- function(bp, classes=bp$classes, dim.biplot = c(2,1,3), e.vects = 1:ncol(bp$X), 
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
  
  crit.opt <- NULL
  if (K < dim.biplot) 
    {  out <- CVA.2class (bp, G, W, M, low.dim, K, e.vects)
       Mr <- out$Lmat[,e.vects, drop = F]
       bp$Lmat <- out$Lmat
       bp$Linv <- solve(out$Lmat)
       bp$eigenvalues <- NULL
       Minv <- solve(out$Lmat)[e.vects, , drop = F]
       crit.opt <- out$TSRES
       low.dim <- out$low.dim
       warning (paste("The dimension of the canonical space < dim.biplot", low.dim, "method used for additional dimension(s)."))
    }
    else
    {
       Mr <- M[, e.vects, drop = F]
       bp$Lmat <- M
       bp$Linv <- solve(M)
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
  bp$crit.opt <- crit.opt
  bp$low.dim <- low.dim

  class(bp) <- append(class(bp),"CVA")
  bp
}

#' CVA extra dimensions if dim(canonical space) < dimension of the biplot
#'
#' @param bp object of class \code{biplot}
#' @param G indicator matrix of class membership
#' @param W within class covariance matrix
#' @param Mmat eigenvector matrix from CVA
#' @param low.dim if the dimension of the canonical space is smaller than \code{dim.biplot}, the method to use to construct
#'                   additional dimension(s). Currently two options are implemented: \code{sample.opt} maximises the sample predictivity
#'                   of the individual samples in the biplot and \code{Bhattacharyya.dist} is based on the decomposition of the
#'                   Bhattacharyya distance into a component for the sample means and a component for the dissimilarity
#'                   between the sample covariance matrices.
#' @param K dimension of the canonical space
#' @param e.vects which eigenvectors (canonical variates) to extract, with default \code{1:dim.biplot}
#'
#' @return A list with three components:
#' \item{Mr}{pxr matrix for the transformation to the canonical space.}
#' \item{Mrr}{rxp matrix for prediction from the canonical space.}
#' \item{Lmat}{pxp matrix for transformation to the canonical space.}
CVA.2class <- function (bp, G, W, Mmat, low.dim, K, e.vects)
{
  first.dims <- e.vects[1:K]
  
  if (!is.na(pmatch(low.dim,"sample.opt")))
  {
    # Calculate fvek.opt
    Minv <- solve(Mmat)
    M.sup2 <- Minv[-first.dims,,drop=FALSE]
    fvek.opt <- svd(M.sup2 %*% t(M.sup2))$v
    # Note that fvek.opt is a linear combination of the columns of M.sup2
    M.opt <- cbind(Mmat[,first.dims],Mmat[,-first.dims]%*%fvek.opt)

    Mr <- M.opt[,e.vects]
    Mrr <- solve(M.opt)[e.vects,]
    Lmat <- M.opt
    low.dim <- "sample.opt"
  }
  
  if (!is.na(pmatch(low.dim,"Bhattacharyya.dist")))
  {
    if (ncol(G)==3) stop ("Bhattacharyya distance method only defined for two groups. Use the sample.opt method to add another dimension.")
    X1.cent <- bp$X[as.logical(G[,1]),]
    X2.cent <- bp$X[as.logical(G[,2]),]
    y1vec <- bp$X %*% Mmat[,first.dims]  
    n1 <- nrow(X1.cent)
    n2 <- nrow(X2.cent)
    M.star <- Mmat[,-first.dims]
    
    S1.star <- t(M.star) %*% stats::cov(X1.cent) %*% M.star
    S2.star <- t(M.star) %*% stats::cov(X2.cent) %*% M.star
    svd2min1.1 <- svd(solve(S2.star) %*% S1.star)
    svd1min1.2 <- svd(solve(S1.star) %*% S2.star)
    TMP <-solve(S2.star) %*% S1.star
    TMPeigen <- eigen(TMP)
    A.unordered <- TMPeigen$vectors
    rev <- TMPeigen$values + 1/TMPeigen$values + 2
    A.ordered <- A.unordered[,order(rev, decreasing=TRUE)]
    K.mat <- cbind(Mmat[,first.dims], M.star %*%A.ordered)
    Mr <- K.mat[ ,e.vects]
    Mrr <- solve(K.mat)[e.vects, ]
    Lmat <- K.mat
    low.dim <- "Bhattacharyya.dist"
  }
  X.hat <- bp$X %*% Mr %*% Mrr
  TSRES  <- sum((bp$X-X.hat)^2) / sum(bp$X^2)
  
  list (Lmat = Lmat, TSRES = TSRES, low.dim = low.dim)
}

# -------------------------------------------------------------------------------------------
#' Analysis of Distance biplot method
#'
#' @param bp an object of class \code{biplot} obtained from preceding function \code{biplot()}. 
#' @param Dmat nxn matrix of Euclidean embeddable distances between samples
#' @param dist.func function to compute Euclidean embeddable distances between samples. The
#'                  default NULL computes Euclidean distance.
#' @param dim.biplot dimension of the biplot. Only values 1, 2 and 3 are accepted, with default \code{2}.
#' @param e.vects e.vects which eigenvectors (canonical variates) to extract, with default \code{1:dim.biplot}.
#' @param classes classes vector of the same length as the number of rows in the data matrix
#'                  with the class indicator for the samples.
#' @param weighted the default is "unweighted" where each class receives equal weight, alternatively "weighted" use class sizes as weights.
#' @param show.class.means logical, indicating whether to plot the class means on the biplot.
#' @param axes type of biplot axes, currently only regression axes are implemented
#' @param ... more arguments to \code{dist.func}
#'
#' @return  Object of class biplot
#'
#' @usage AoD(bp, Dmat=NULL, dist.func="Euclidean", dim.biplot = c(2,1,3), 
#' e.vects = 1:ncol(bp$X), classes=bp$classes, weighted = c("unweighted","weighted"), 
#' show.class.means = TRUE, axes = "regression", ...)

#' @aliases AoD
#'
#' @export
#'
#' @examples
#' biplot(iris[,1:4]) |> AoD(classes=iris[,5])
#' # create a CVA biplot
#' biplot(iris[,1:4]) |> AoD(classes=iris[,5]) |> plot()


AoD <- function (bp, Dmat=NULL, dist.func="Euclidean",
                 dim.biplot = c(2,1,3), e.vects = 1:ncol(bp$X), classes=bp$classes,
                 weighted = c("unweighted","weighted"), show.class.means = TRUE, 
                 axes = "regression", ...)
{
  UseMethod("AoD")
}

#' AoD biplot
#'
#' @description Computes Analysis of Distance biplot
#'
#' @inheritParams AoD
#'
#' @return an object of class biplot.
#' @export
#'
#' @examples
#' biplot(iris) |> AoD(classes = iris[,5]) |> plot()
#'
AoD.biplot <- function (bp, Dmat=NULL, dist.func=NULL,
                        dim.biplot = c(2,1,3), e.vects = 1:ncol(bp$X), classes=bp$classes,
                        weighted = c("unweighted","weighted"), show.class.means = TRUE, 
                        axes = "regression", ...)
{
  dim.biplot <- dim.biplot[1]
  if (dim.biplot != 1 & dim.biplot != 2 & dim.biplot != 3) stop("Only 1D, 2D and 3D biplots")
  e.vects <- e.vects[1:dim.biplot]
  
  weight <- weighted[1]
  
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
  G <- indmat(classes)
  Xbar <- solve(t(G) %*% G) %*% t(G) %*% X
  
  if (is.null(dist.func)) dist.func <- stats::dist
  theta.bar <- NULL
  if (is.null(Dmat)) Dmat <- dist.func(Xbar) 
  else theta.bar <- solve(t(G) %*% G) %*% t(G) %*% (-0.5*as.matrix(Dmat)^2) %*% G %*% solve(t(G) %*% G)

  n <- bp$n
  p <- bp$p
  J <- ncol(G)
  K <- min(p, J-1)
  
  if (weight=="unweighted") s.vec <- rep(1,J)/J
  if (weight=="weighted") s.vec <- diag(t(G)%*%G)/n

  if (is.null(theta.bar)) DDmat <- -0.5 * as.matrix(Dmat)^2
  one <- matrix (1, nrow=1, ncol=J)
  
  I.min.N <- diag(J) - s.vec %*% one
  if (is.null(theta.bar)) B <- I.min.N %*% DDmat %*% I.min.N
  else B <- I.min.N %*% theta.bar %*% I.min.N
  if (any(zapsmall(eigen(B)$values)<0)) warning ("Your distances are not Euclidean embeddable.")
  
  svd.out <- svd(B)
  Ybar <- svd.out$v %*% diag(sqrt(svd.out$d))
  Zmeans <- Ybar[,e.vects]

  Mr <- solve(t(Zmeans) %*% Zmeans) %*% t(Zmeans) %*% Xbar
  bp$Lmat <- NULL
  bp$eigenvalues <- svd.out$d
  
  bp$ax.one.unit <- 1/(diag(t(Mr) %*% Mr)) * t(Mr)

  ddist0 <- matrix(-0.5*apply(Ybar, 1, function(y) sum((y-rep(0,ncol(Ybar)))^2)), ncol=1)
  Z <- matrix (nrow=n, ncol=dim.biplot)
  if (is.null(theta.bar))
  {
    for (i in 1:n)
    {
      mat <- rbind (X[i,], Xbar)
      Dmat <- as.matrix(dist.func(mat, ...))
      ddist.i <- matrix(-0.5*Dmat[1,-1]^2, ncol=1)
      Z[i,] <- solve(t(Zmeans)%*%Zmeans) %*% t(Zmeans) %*% (I.min.N) %*% (ddist.i - ddist0)
    }
  }
  else
  {
    # --- if the dist.func is not given,
    #     first embed X:nxp in Euclidean space -> Ymat
    #     Y2.bar is class means of Ymat
    #     compute Euclidean distance between rows of Ymat and Y2.bar
    DDmat.2 <- -0.5 * as.matrix(Dmat)^2
    s2.vec <- matrix(rep(1,n)/n, ncol=1)
    one.n <- matrix (1, nrow=1, ncol=n)
    I.min.N.2 <- diag(n) - s2.vec %*% one.n
    B2 <- I.min.N.2 %*% DDmat.2 %*% I.min.N.2
    svd.out.2 <- svd(B2)
    Ymat <- svd.out.2$v %*% diag(sqrt(svd.out.2$d))
    Y2.bar <- solve(t(G) %*% G) %*% t(G) %*% Ymat
    for (i in 1:n)
    {
      mat <- rbind (Ymat[i,], Y2.bar)
      Dmat <- as.matrix(stats::dist(mat))
      ddist.i <- matrix(-0.5*Dmat[1,-1]^2, ncol=1)
      Z[i,] <- solve(t(Zmeans)%*%Zmeans) %*% t(Zmeans) %*% (I.min.N) %*% (ddist.i - ddist0)
    }
  }
  
  rownames(Z) <- rownames(X)
  bp$Z <- Z
  bp$Zmeans <- Zmeans
  bp$ax.one.unit <- 1/(diag(t(Mr) %*% Mr)) * t(Mr)
  
  bp$Xbar <- Xbar
  bp$Gmat <- G
  bp$Xmeans <- Xbar
  bp$e.vects <- e.vects
  bp$Mrr <- NULL
  bp$Mr <- Mr
  bp$class.means <- show.class.means
  bp$dim.biplot <- dim.biplot
  
  bp
  
}
