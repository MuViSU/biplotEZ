
# -------------------------------------------------------------------------------------------
#' Regression biplot method
#'
#' @param bp an object of class \code{biplot} obtained from preceding function \code{biplot()}. 
#' @param Z the matrix of coordinates of the samples
#' @param group.aes vector of the same length as the number of rows in the data matrix
#'                  for differentiated aesthetics for samples.
#' @param show.group.means logical, indicating whether group means should be plotted in the biplot.
#'
#' @return  Object of class biplot
#'
#' @usage regress(bp, Z, group.aes=NULL, show.group.means = TRUE)
#' @aliases regress
#'
#' @export
#'
#' @examples
#' biplot(iris[,1:4]) |> regress(Z=cmdscale(dist(iris[,1:4]))) |> plot()

regress <- function (bp, Z, group.aes=NULL, show.group.means = TRUE)
{
  UseMethod("regress")
}

#' Regression biplot
#'
#' @description Computes regression biplot axes
#'
#' @inheritParams regress
#'
#' @return an object of class \code{biplot}.
#' @export
#'
#' @examples
#' biplot(iris) |> regress(Z = cmdscale(dist(iris[,1:4]))) |> plot()
#'
regress.biplot <- function (bp, Z, group.aes=NULL, show.group.means = TRUE)
{
  
  dim.biplot <- ncol(Z)
  if (dim.biplot != 1 & dim.biplot != 2 & dim.biplot != 3) stop("Only 1D, 2D and 3D biplots")
  e.vects <- 1:dim.biplot
  if (!is.null(group.aes)) { bp$group.aes <- factor(group.aes)
  bp$g.names <-levels(factor(group.aes))
  bp$g <- length(bp$g.names)
  }
  
  if (any(apply(Z, 2, mean) != 0)) Z <- scale(Z, scale=F)
  
  X <- bp$X
  n <- bp$n
  p <- bp$p
  
  Lmat <- solve(t(Z)%*%Z) %*% t(Z) %*% X
  Vr <- t(Lmat)
  ax.one.unit <- 1/(diag(Vr %*% t(Vr))) * Vr
  
  if (is.null(rownames(Z))) rownames(Z) <- rownames(X)
  
  bp$Z <- Z
  bp$Lmat <- NULL
  bp$eigenvalues <- NULL
  bp$ax.one.unit <- ax.one.unit
  bp$e.vects <- e.vects
  bp$Vr <- Vr
  bp$dim.biplot <- dim.biplot
  if (bp$g == 1) bp$class.means <- FALSE else bp$class.means <- show.group.means
  if (bp$class.means)
  {
    G <- indmat(bp$group.aes)
    Xmeans <- solve(t(G)%*%G) %*% t(G) %*% X
    Zmeans <- solve(t(G)%*%G) %*% t(G) %*% Z
    bp$Zmeans <- Zmeans
  }
  
  bp
}

# -------------------------------------------------------------------------------------------
#' Principal Coordinate Analysis (PCO) biplot method
#'
#' @param bp an object of class \code{biplot} obtained from preceding function \code{biplot()}. 
#' @param Dmat nxn matrix of Euclidean embeddable distances between samples
#' @param dist.func function to compute Euclidean embeddable distances between samples. The
#'                  default NULL computes Euclidean distance.
#' @param dim.biplot dimension of the biplot. Only values 1, 2 and 3 are accepted, with default \code{2}.
#' @param e.vects e.vects which eigenvectors (canonical variates) to extract, with default \code{1:dim.biplot}.
#' @param group.aes vector of the same length as the number of rows in the data matrix
#'                  for differentiated aesthetics for samples.
#' @param show.class.means logical, indicating whether to plot the class means on the biplot.
#' @param axes type of biplot axes, currently only regression axes are implemented
#' @param ... more arguments to \code{dist.func}
#'
#' @return  Object of class biplot
#'
#' @usage PCO(bp, Dmat=NULL, dist.func=NULL, dim.biplot = c(2,1,3), 
#'            e.vects = 1:ncol(bp$X), group.aes=NULL, show.class.means = TRUE, 
#'            axes = c("regression","splines"), ...)
#' @aliases PCO
#'
#' @export
#'
#' @examples
#' biplot(iris[,1:4]) |> PCO(dist.func = sqrtManhattan)
#' # create a CVA biplot
#' biplot(iris[,1:4]) |> PCO(dist.func = sqrtManhattan) |> plot()

PCO <- function (bp, Dmat=NULL, dist.func=NULL,
                 dim.biplot = c(2,1,3), e.vects = 1:ncol(bp$X), group.aes=NULL,
                 show.class.means = TRUE, axes = c("regression","splines"), ...)
{
  UseMethod("PCO")
}

#' PCO biplot
#'
#' @description Computes Principal Coordinate Analysis biplot
#'
#' @inheritParams PCO
#'
#' @return an object of class biplot.
#' @export
#'
#' @examples
#' biplot(iris) |> PCO(dist.func=sqrtManhattan) |> plot()
#'
PCO.biplot <- function (bp, Dmat=NULL, dist.func=NULL, 
                        dim.biplot = c(2,1,3), e.vects = 1:ncol(bp$X), group.aes=NULL,
                        show.class.means = FALSE, axes = c("regression","splines"), ...)
{
  X <- bp$X
  if (is.null(dist.func)) dist.func <- stats::dist
  if (is.null(Dmat)) Dmat <- dist.func(X, ...)
  Dmat <- as.matrix(Dmat)

  n <- bp$n
  p <- bp$p
  dim.biplot <- dim.biplot[1]
  if (dim.biplot != 1 & dim.biplot != 2 & dim.biplot != 3) stop("Only 1D, 2D and 3D biplots")
  e.vects <- e.vects[1:dim.biplot]
  if (!is.null(group.aes)) { bp$group.aes <- factor(group.aes)
  bp$g.names <-levels(factor(group.aes))
  bp$g <- length(bp$g.names)
  }

  DDmat <- -0.5 * Dmat^2
  s.vec <- matrix(rep(1,n)/n, ncol=1)
  one <- matrix (1, nrow=1, ncol=n)
  
  I.min.N <- diag(n) - s.vec %*% one
  B <- I.min.N %*% DDmat %*% I.min.N
  if (any(zapsmall(eigen(B)$values)<0)) warning ("Your distances are not Euclidean embeddable.")
  
  svd.out <- svd(B)
  Ymat <- svd.out$v %*% diag(sqrt(svd.out$d))
  Z <- Ymat[,e.vects]

  Mr <- solve(t(Z) %*% Z) %*% t(Z) %*% X
  bp$Lmat <- NULL
  bp$eigenvalues <- svd.out$d

  bp$ax.one.unit <- 1/(diag(t(Mr) %*% Mr)) * t(Mr)

  rownames(Z) <- rownames(X)
  bp$Z <- Z
  bp$Lmat <- NULL
  bp$eigenvalues <- svd.out$d
  bp$e.vects <- e.vects
  bp$dim.biplot <- dim.biplot
  bp$PCOaxes <- axes[1]
  
  if (bp$g == 1) bp$class.means <- FALSE else bp$class.means <- show.class.means
  if (bp$class.means)
  {
    G <- indmat(bp$group.aes)
    Xmeans <- solve(t(G)%*%G) %*% t(G) %*% X
    Zmeans <- solve(t(G)%*%G) %*% t(G) %*% Z
    bp$Zmeans <- Zmeans
  }
  
  bp
}
  