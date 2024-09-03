# -------------------------------------------------------------------------------------------
#' Perform Canonical Variate Analysis (CVA)
#'
#' @description
#' This function appends the \code{biplot} object with elements resulting from performing CVA.
#'
#' @param bp an object of class \code{biplot} obtained from preceding function \code{biplot()}.
#' @param classes a vector of the same length as the number of rows in the data matrix with the class indicator for the samples.
#' @param dim.biplot the dimension of the biplot. Only values \code{1}, \code{2} and \code{3} are accepted, with default \code{2}.
#' @param e.vects the vector indicating which eigenvectors (canonical variates) should be plotted in the biplot, with default \code{1:dim.biplot}.
#' @param weightedCVA a character string indicating which type of CVA to perform. One of "\code{weighted}" (default) for a weighted CVA to be performed (The centring matrix will be a diagonal matrix with the class sizes (\eqn{\mathbf{C} = \mathbf{N}}), "\code{unweightedCent}" for unweighted CVA to be performed (The centring matrix is the usual centring matrix (\eqn{\mathbf{C} = \mathbf{I}_{G} - G^{-1}\mathbf{1}_{G}\mathbf{1}_{G}'})) or "\code{unweightedI}" for unweighted CVA to be performed while retaining the weighted centroid (The centring matrix is an indicator matrix (\eqn{\mathbf{C} = \mathbf{I}_{G}})).
#' @param show.class.means a logical value indicating whether to plot the class means on the biplot.
#' @param low.dim a character string indicating which method to use to construct additional dimension(s) if the dimension of the canonical space is smaller than \code{dim.biplot}. One of "\code{sample.opt}" (default) for maximising the sample predictivity of the individual samples in the biplot or "\code{Bhattacharyya.dist}" which is based on the decomposition of the Bhattacharyya distance into a component for the sample means and a component for the dissimilarity between the sample covariance matrices.
#'
#' @return  Object of class CVA with the following elements:
#' \item{X}{the matrix of the centered and scaled numeric variables.}
#' \item{Xcat}{the data frame of the categorical variables.}
#' \item{raw.X}{the original data.}
#' \item{classes}{the vector of category levels for the class variable. This is to be used for \code{colour}, \code{pch} and \code{cex} specifications.}
#' \item{na.action}{the vector of observations that have been removed.}
#' \item{center}{a logical value indicating whether \eqn{\mathbf{X}} is centered.}
#' \item{scaled}{a logical value indicating whether \eqn{\mathbf{X}} is scaled.}
#' \item{means}{the vector of means for each numerical variable.}
#' \item{sd}{the vector of standard deviations for each numerical variable.}
#' \item{n}{the number of observations.}
#' \item{p}{the number of variables.}
#' \item{group.aes}{the vector of category levels for the grouping variable. This is to be used for \code{colour}, \code{pch} and \code{cex} specifications. }
#' \item{g.names}{the descriptive names to be used for group labels.}
#' \item{g}{the number of groups.}
#' \item{Title}{the title of the biplot rendered.}
#' \item{Lmat}{the matrix for transformation to the canonical space.}
#' \item{Linv}{the inverse of \eqn{\mathbf{L}}.}
#' \item{eigenvalues}{the vector of eigenvalues of the two-sided eigenvalue problem.}
#' \item{Z}{the matrix with each row containing the details of the points to be plotted (i.e. coordinates).}
#' \item{ax.one.unit}{one unit in the positive direction of each biplot axis.}
#' \item{Gmat}{the indicator matrix defining membership of the classes.}
#' \item{Xmeans}{the matrix of the class means.}
#' \item{Zmeans}{the matrix of the class mean coordinates that are plotted in the biplot.}
#' \item{e.vects}{the vector indicating which canonical variates are plotted in the biplot.}
#' \item{Cmat}{the centring matrix based on different choices of weighting described in arguments.}
#' \item{Bmat}{the between class sums of squares and cross products matrix.}
#' \item{Wmat}{the within class sums of squares and cross products matrix.}
#' \item{Mrr}{the matrix used for prediction from the canonical space (the inverse of \eqn{\mathbf{M}=\mathbf{LV})}.}
#' \item{Mr}{the first r dimensions of the solution to be plotted.}
#' \item{Nmat}{the matrix with the class sizes on the diagonal.}
#' \item{lambda.mat}{the matrix with the eigenvalues of \eqn{\mathbf{W}^{-1/2}\mathbf{BW}^{-1/2}} on the diagonal.}
#' \item{class.means}{a logical value indicating whether the class means should be plotted in the biplot.}
#' \item{dim.biplot}{the dimension of the biplot.}
#' \item{low.dim}{the method used to construct additional dimension(s).}
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
#' 
CVA <- function(bp, classes=bp$classes, dim.biplot = c(2,1,3), e.vects = 1:ncol(bp$X), 
                weightedCVA = "weighted", show.class.means = TRUE, low.dim = "sample.opt")
{
  UseMethod("CVA")
}

#' Calculate elements for the CVA biplot 
#'
#' @description This function performs calculations for the construction of a CVA biplot.
#'
#' @inheritParams CVA
#'
#' @return an object of class \code{CVA}, inherits from class \code{biplot}.
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
    {  out <- CVAlowdim (bp, G, W, M, low.dim, K, e.vects)
       Mr <- out$Lmat[, e.vects, drop = F]
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
       Mr <- M[,e.vects]
       bp$Lmat <- M
       bp$L.inv <- solve(M)
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

#' Construct additional dimensions when the dimension of the canonical space is smaller than the dimension of the biplot
#' 
#' @description
#' This function is used to add dimensions to the CVA biplot when the dimension of the canonical space \eqn{K} is smaller than the dimension of the biplot (\code{dim.biplot}). This function is already used in the CVA calculations, and will therefore not have to be used in isolation. 
#' 
#'
#' @param bp an object of class \code{biplot}.
#' @param G the indicator matrix defining membership of the classes.
#' @param W the within class sums of squares and cross products matrix.
#' @param Mmat the eigenvector matrix from CVA.
#' @param low.dim a character string indicating which method to use to construct additional dimension(s) if the dimension of the canonical space is smaller than \code{dim.biplot}. One of "\code{sample.opt}" (default) for maximising the sample predictivity of the individual samples in the biplot or \code{Bhattacharyya.dist} which is based on the decomposition of the Bhattacharyya distance into a component for the sample means and a component for the dissimilarity between the sample covariance matrices.
#' @param K the dimension of the canonical space.
#' @param e.vects the vector indicating which canonical variates are plotted in the biplot, with default \code{1:dim.biplot}
#'
#' @return A list with three components:
#' \item{Mr}{the first r dimensions of the solution to be plotted.}
#' \item{Mrr}{the matrix used for prediction from the canonical space.}
#' \item{Lmat}{the matrix for transformation to the canonical space.}
#' 
CVAlowdim <- function (bp, G, W, Mmat, low.dim, K, e.vects)
{
  first.dims <- e.vects[1:K]
  
  if (!is.na(pmatch(low.dim,"sample.opt")))
  {
    Minv <- solve(Mmat)
    M.sup2 <- Minv[-first.dims,,drop=FALSE]
    fvek.opt <- svd(M.sup2 %*% t(M.sup2))$v
    # Note that fvek.opt is a linear combination of the columns of M.sup2
    M.opt <- cbind(Mmat[,first.dims], Mmat[,-first.dims]%*%fvek.opt)
    
    Mr <- M.opt[,e.vects]
    Mrr <- solve(M.opt)[e.vects, ]
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
#' Use the Analysis of Distance (AoD) method to construct the biplot
#' 
#' @description
#' This function appends the \code{biplot} object with elements resulting from using the AoD method.
#'
#' @param bp an object of class \code{biplot} obtained from preceding function \code{biplot()}. 
#' @param classes a vector of the same length as the number of rows in the data matrix with the class indicator for the samples.
#' @param dist.func a character string indicating which distance function is used to compute the Euclidean embeddable distances between samples. One of \code{NULL} (default) which computes the Euclidean distance or other functions that can be used for the \code{dist()} function.
#' @param dist.func.cat a character string indicating which distance function is used to compute the Euclidean embeddable distances between samples. One of \code{NULL} (default) which computes the extended matching coefficient or other functions.
#' @param dim.biplot the dimension of the biplot. Only values \code{1}, \code{2} and \code{3} are accepted, with default \code{2}.
#' @param e.vects the vector indicating which eigenvectors (canonical variates) should be plotted in the biplot, with default \code{1:dim.biplot}.
#' @param weighted a character string indicating the weighting of the classes. One of "\code{unweighted}" for each class to receive equal weighting or "\code{weighted}" for each class to receive their class sizes as weights.
#' @param show.class.means a logical value indicating whether to plot the class means on the biplot.
#' @param axes a character string indicating the type of biplot axes to be used in the biplot. One of \code{"regression"} or \code{"splines"}.
#' @param ... more arguments to \code{dist.func}.
#'
#' @return  Object of class \code{biplot}
#'
#' @usage AoD(bp, classes=bp$classes, dist.func=NULL, dist.func.cat=NULL,
#' dim.biplot = c(2,1,3), e.vects = 1:ncol(bp$X), 
#' weighted = c("unweighted","weighted"), show.class.means = TRUE, 
#' axes = c("regression","splines"), ...)

#' @aliases AoD
#'
#' @export
#'
#' @examples
#' biplot(iris[,1:4]) |> AoD(classes=iris[,5])
#' # create a CVA biplot
#' biplot(iris[,1:4]) |> AoD(classes=iris[,5]) |> plot()
#' 
AoD <- function (bp, classes=bp$classes, dist.func=NULL, dist.func.cat=NULL,
                 dim.biplot = c(2,1,3), e.vects = 1:ncol(bp$X), 
                 weighted = c("unweighted","weighted"), show.class.means = TRUE, 
                 axes = c("regression","splines"), ...)
{
  UseMethod("AoD")
}

#' Calculate elements for the Analysis of Distance (AoD) biplot
#'
#' @description This function is used to construct the AoD biplot
#'
#' @inheritParams AoD
#'
#' @return an object of class \code{biplot}.
#' @export
#'
#' @examples
#' biplot(iris) |> AoD(classes = iris[,5]) |> plot()
#'
AoD.biplot <- function (bp, classes=bp$classes, dist.func=NULL, dist.func.cat=NULL,
                        dim.biplot = c(2,1,3), e.vects = 1:ncol(bp$X), 
                        weighted = c("unweighted","weighted"), show.class.means = TRUE, 
                        axes = c("regression","splines"), ...)
{
  dim.biplot <- dim.biplot[1]
  if (dim.biplot != 1 & dim.biplot != 2 & dim.biplot != 3) stop("Only 1D, 2D and 3D biplots")
  e.vects <- e.vects[1:dim.biplot]
  
  weight <- weighted[1]
  axes <- axes[1]
  
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
  Xcat <- bp$Xcat
  G <- indmat(classes)
  Nmat <- t(G) %*% G
  
  for (j in 1:ncol(Xcat))
   if (all(levels(Xcat[,j]) %in% levels(classes)) & all(levels(classes) %in% levels(Xcat[,j])))
      if (all(classes == Xcat[,j]))
       {  Xcat <- Xcat[,-j]
          bp$Xcat <- Xcat
          bp$p2 <- ncol(Xcat)
          break
       }
  
  if (is.null(dist.func) & !is.null(X)) dist.func <- stats::dist
  if (is.null(dist.func.cat) & !is.null(Xcat)) dist.func.cat <- extended.matching.coefficient
  D1 <- D2 <- NULL
  if (!is.null(dist.func)) D1 <- dist.func(X, ...)
  if (!is.null(dist.func.cat)) D2 <- dist.func.cat(Xcat, ...)
  if (!is.null(D1)) D.sq <- D1^2 else D.sq <- NULL
  if (!is.null(D2)) if (is.null(D.sq)) D.sq <- D2^2 else D.sq <- D.sq + D2^2
  Dmat <- sqrt(D.sq)
  D.bar <- solve(Nmat) %*% t(G) %*% (-0.5*as.matrix(Dmat)^2) %*% G %*% solve(Nmat)

  n <- bp$n
  p <- bp$p
  p2 <- ncol(Xcat)
  J <- ncol(G)
  K <- min(p, J-1)
  
  if (weight=="unweighted") s.vec <- rep(1,J)/J
  if (weight=="weighted") s.vec <- diag(Nmat)/n

  one <- matrix (1, nrow=1, ncol=J)
  I.min.N <- diag(J) - s.vec %*% one
  B <- I.min.N %*% D.bar %*% I.min.N
  if (any(zapsmall(eigen(B)$values)<0)) warning ("Your distances are not Euclidean embeddable.")
  
  svd.out <- svd(B)
  Ybar <- svd.out$v %*% diag(sqrt(svd.out$d))
  Zmeans <- Ybar[,e.vects]
  Ybar <- Ybar[,svd.out$d>sqrt(.Machine$double.eps)]
  
  if (axes == "regression")
  {
    Xbar <- solve(Nmat) %*% t(G) %*% X
    Mr <- solve(t(Zmeans) %*% Zmeans) %*% t(Zmeans) %*% Xbar
    bp$Lmat <- NULL
    bp$eigenvalues <- svd.out$d
    bp$ax.one.unit <- 1/(diag(t(Mr) %*% Mr)) * t(Mr)
    bp$Mr <- Mr
  }
  else Xbar <- NULL
  if(axes == "splines") bp$spline.control <- biplot.spline.axis.control()
  bp$PCOaxes <- axes

  Delta <- matrix (0, nrow=J, ncol=J)
  for (i in 1:(J-1))
    for (j in (i+1):J)
      Delta[i,j] <- -0.5*(D.bar[i,i]+D.bar[j,j]-2*D.bar[i,j])
  Delta <- Delta + t(Delta)

#  print(apply(Delta, 1, sum)/J)
#  stop ("tot hier")
 
  ddist0 <- matrix (-0.5*apply(Ybar, 1, function(y) sum((y-rep(0,ncol(Ybar)))^2)), ncol=1) 
  new.point <- function (coords, coords.cat)
  {
    m <- nrow(coords)
    d1vec <- d2vec <- NULL
    if (!is.null(dist.func)) d1vec <- as.matrix(dist.func(rbind(coords,X), ...))[-(1:m),1:m]
    if (!is.null(dist.func.cat)) d2vec <- as.matrix(dist.func.cat(rbind(coords.cat,Xcat), ...))[-(1:m),1:m]
    if (!is.null(d1vec)) dvec <- d1vec^2 else dvec <- NULL
    if (!is.null(d2vec)) 
      { if (is.null(dvec)) dvec <- d2vec^2 else dvec <- dvec + d2vec^2 }
    dvec <- -0.5*dvec
    delta.vec <- diag(D.bar)-2*solve(Nmat) %*% apply(dvec, 2, function(dd) tapply (dd, classes, sum))
    solve(t(Ybar)%*%Ybar) %*% t(Ybar) %*% (delta.vec - matrix(apply(Delta, 1, sum)/J,ncol=1) %*% matrix(1, nrow=1, ncol=ncol(delta.vec)))
  }
  
  Z <- matrix (nrow=n, ncol=dim.biplot)
  Z <- t(new.point(X, Xcat))[,e.vects]

  if (!is.null(Xcat))
  {
    CLPs <- vector("list", p2)
    for (j in 1:p2)
    {
      CLPj <- NULL
      for (tau in levels(factor(Xcat[,j])))
      {
        X.tau <- Xcat
        X.tau[,j] <- tau
        Y.tau <- t(new.point(X, X.tau))
        CLPj <- rbind (CLPj, apply(Y.tau,2,mean))
      }
      rownames(CLPj) <- levels(factor(Xcat[,j]))
      CLPs[[j]] <- CLPj
    }
    names(CLPs) <- colnames(Xcat)
  }
  else CLPs <- NULL
  
  if (!is.null(CLPs))
  {
    CLP.coords <- vector("list", length(CLPs))
    my.list <- vector("list", ncol(Z))
    for (h in 1:ncol(Z))
      my.list[[h]] <- seq(from=min(Z[,h]), to=max(Z[,h]), len=20)
    grid <- as.matrix(expand.grid(my.list))
    for (h in 1:length(CLPs))
    {
      grid.mat <- cbind (grid, matrix (0, nrow=nrow(grid), ncol=ncol(CLPs[[h]])-2))
      D.CLP <- as.matrix(stats::dist(rbind (CLPs[[h]], grid.mat)))[1:nrow(CLPs[[h]]),-(1:nrow(CLPs[[h]]))]
      closest.level <- apply (D.CLP, 2, which.min)
      CLP.coords[[h]] <- apply (grid, 2, function (x) tapply(x, closest.level, mean))
      if (!is.matrix(CLP.coords[[h]])) CLP.coords[[h]] <- matrix (CLP.coords[[h]], nrow=1)
      rownames(CLP.coords[[h]]) <- rownames(CLPs[[h]])[sort(unique(closest.level))]
    }
  }
  else CLP.coords <- NULL
  
  rownames(Z) <- rownames(X)
  bp$Z <- Z
  bp$Zmeans <- Zmeans
  bp$Xbar <- Xbar
  bp$Gmat <- G
  bp$Xmeans <- Xbar
  bp$CLPs <- CLPs
  bp$CLP.coords <- CLP.coords
  bp$e.vects <- e.vects
  bp$Mrr <- NULL
  bp$class.means <- show.class.means
  bp$dim.biplot <- dim.biplot
  
  bp
  
}
