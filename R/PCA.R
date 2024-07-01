# -------------------------------------------------------------------------------------------
#' Principal Component Analysis (PCA) method
#'
#' @description
#' This function produces a list of elements to be used for PCA biplot construction.
#'
#' @param bp an object of class \code{biplot} obtained from preceding function \code{biplot()}.
#' @param dim.biplot dimension of the biplot. Only values 1, 2 and 3 are accepted, with default \code{2}.
#' @param e.vects which eigenvectors (principal components) to extract, with default \code{1:dim.biplot}.
#' @param group.aes vector of the same length as the number of rows in the data matrix
#'                  for differentiated aesthetics for samples.
#' @param show.class.means logical, indicating whether group means should be plotted in the biplot.
#' @param correlation.biplot logical, if \code{FALSE}, the distances between sample points are
#'                           optimally approximated in the biplot. If \code{TRUE}, the correlations between
#'                           variables are optimally approximated by the cosine of the angles between
#'                           axes. Default is \code{FALSE}.
#'
#' @return  Object of class PCA with the following elements:
#' \item{X}{matrix of the centered and scaled numeric variables.}
#' \item{Xcat}{data frame of the categorical variables.}
#' \item{raw.X}{original data.}
#' \item{classes}{vector of category levels for the class variable. This is to be used for \code{colour}, \code{pch} and \code{cex} specifications.}
#' \item{na.action}{vector of observations that have been removed.}
#' \item{center}{logical. indicating whether X is centered.}
#' \item{scaled}{logical. indicating whether X is scaled.}
#' \item{means}{vector of means for each numerical variable.}
#' \item{sd}{vector of standard deviations for each numerical variable.}
#' \item{n}{number of observations.}
#' \item{p}{number of variables.}
#' \item{group.aes}{vector of the same length as the number of rows in the data matrix for differentiated aesthetics for samples.}
#' \item{g.names}{descriptive names to be used for group labels.}
#' \item{g}{number of groups.}
#' \item{Title}{title of the biplot to be rendered.}
#' \item{Z}{matrix with each row containing the details of the point to be plotted (i.e. coordinates).}
#' \item{Lmat}{matrix for transformation to the principal components.}
#' \item{Linv}{inverse of the Lmat matrix.}
#' \item{eigenvalues}{vector of eigenvalues of the covariance matrix of \code{X}.}
#' \item{ax.one.unit}{one unit in the positive direction of each biplot axis.}
#' \item{e.vects}{vector indicating which principal components are plotted in the biplot.}
#' \item{Vr}{the \code{1:dim.biplot} columns of the \code{V.mat}.}
#' \item{dim.biplot}{dimension of the biplot.}
#' \item{V.mat}{the matrix containing the right singular vectors of \code{X}.}
#' \item{Sigma.mat}{the matrix with the singular values of \code{X} on the diagonal.}
#' \item{U.mat}{the matrix containing the left singular vectors of \code{X}.}
#' \item{class.means}{logical. indicating whether group means should be plotted in the biplot.}
#' \item{Zmeans}{matrix of class mean coordinates to be plotted in the biplot.}
#'
#' @usage PCA(bp, dim.biplot = c(2, 1, 3), e.vects = 1:ncol(bp$X),
#' group.aes = NULL, show.class.means = FALSE, correlation.biplot = FALSE)
#' @aliases PCA
#'
#' @export
#'
#'@references
#' Gabriel, K.R. (1971) The biplot graphic display of matrices with application to principal component analysis. \emph{Biometrika.} 58(3):453–467.
#'
#' @examples
#' biplot(iris[,1:4]) |> PCA()
#' # create a PCA biplot
#' biplot(data = iris) |> PCA() |> plot()
PCA <- function (bp, dim.biplot = c(2, 1, 3), e.vects = 1:ncol(bp$X), group.aes=NULL, show.class.means = FALSE,
                 correlation.biplot=FALSE)
{
  UseMethod("PCA")
}

#' PCA biplot
#'
#' @description Performs calculations for a PCA biplot.
#'
#' @inheritParams PCA
#'
#' @return an object of class PCA, inherits from class biplot.
#' @export
#'
#' @examples
#' biplot(iris[,1:4]) |> PCA()
#' # create a PCA biplot
#' biplot(data = iris) |> PCA() |> plot()
#'
PCA.biplot <- function (bp, dim.biplot = c(2, 1, 3), e.vects = 1:ncol(bp$X), group.aes=NULL,
                        show.class.means = FALSE, correlation.biplot=FALSE)
{
  
  dim.biplot <- dim.biplot[1]
  if (dim.biplot != 1 & dim.biplot != 2 & dim.biplot != 3) stop("Only 1D, 2D and 3D biplots")
  e.vects <- e.vects[1:dim.biplot]
  if (!is.null(group.aes)) { bp$group.aes <- factor(group.aes)
  bp$g.names <-levels(factor(group.aes))
  bp$g <- length(bp$g.names)
  }
  
  if (!bp$center)
  {  warning("PCA requires a centred datamatrix. Your data was centred before computation. Use center = TRUE in the call to biplot()")
    bp <- biplot (bp$X, center = TRUE, scaled=bp$scaled, bp$classes, bp$group.aes)
  }
  X <- bp$X
  n <- bp$n
  p <- bp$p

  svd.out <- svd(X)
  V.mat <- svd.out$v
  U.mat <- svd.out$u
  Sigma.mat <- diag(svd.out$d)
  Lmat <- svd.out$v
  Vr <- svd.out$v[, e.vects, drop = FALSE]

  if (correlation.biplot)
  {

    if (dim.biplot>1) lambda.r <- diag(svd(t(X) %*% X)$d[1:dim.biplot])
    else lambda.r <- matrix(svd(t(X) %*% X)$d, nrow=1, ncol=1)
    Z <- sqrt(n - 1) * X %*% Vr %*% sqrt(solve(lambda.r))
    Lmat <- sqrt(n-1) * Lmat %*% sqrt(solve(diag(svd(t(X) %*% X)$d)))
  }
  else { Z <- X %*% Vr }
  rownames(Z) <- rownames(X)

  if (correlation.biplot)
    ax.one.unit <- (sqrt(n - 1)/(diag(Vr %*% lambda.r %*% t(Vr)))) * Vr %*% sqrt(lambda.r)
  else
    ax.one.unit <- 1/(diag(Vr %*% t(Vr))) * Vr

  bp$Z <- Z
  bp$Lmat <- Lmat
  bp$Linv <- solve(Lmat)
  bp$eigenvalues <- svd.out$d^2
  bp$ax.one.unit <- ax.one.unit
  bp$e.vects <- e.vects
  bp$Vr <- Vr
  bp$dim.biplot <- dim.biplot
  bp$V.mat <- V.mat
  bp$Sigma.mat <- Sigma.mat
  bp$U.mat <- U.mat
  if (bp$g == 1) bp$class.means <- FALSE else bp$class.means <- show.class.means
  if (bp$class.means)
  {
    G <- indmat(bp$group.aes)
    Xmeans <- solve(t(G)%*%G) %*% t(G) %*% X
    Zmeans <- Xmeans %*% Lmat[,e.vects]
    bp$Zmeans <- Zmeans
  }
  
  class(bp)<-append(class(bp),"PCA")
  bp
}

