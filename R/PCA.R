# -------------------------------------------------------------------------------------------
#' Perform Principal Components Analysis (PCA)
#'
#' @description
#' This function appends the \code{biplot} object with elements resulting from performing PCA.
#'
#' @param bp an object of class \code{biplot} obtained from preceding function \code{biplot()}.
#' @param dim.biplot the dimension of the biplot. Only values \code{1}, \code{2} and \code{3} are accepted, with default \code{2}.
#' @param e.vects the vector indicating which eigenvectors (principal components) should be plotted in the biplot, with default \code{1:dim.biplot}.
#' @param group.aes a vector of the same length as the number of rows in the data matrix
#'                  for differentiated aesthetics for samples.
#' @param show.class.means a logical value indicating whether group means should be plotted in the biplot.
#' @param correlation.biplot a logical value. If \code{FALSE}, the distances between sample points are
#'                           optimally approximated in the biplot. If \code{TRUE}, the correlations between
#'                           variables are optimally approximated by the cosine of the angles between
#'                           axes. Default is \code{FALSE}.
#'
#' @return An object of class PCA with the following elements:
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
#' \item{group.aes}{the vector of category levels for the grouping variable. This is to be used for \code{colour}, \code{pch} and \code{cex} specification.}
#' \item{g.names}{the descriptive names to be used for group labels.}
#' \item{g}{the number of groups.}
#' \item{Title}{the title of the biplot rendered.}
#' \item{Z}{the matrix with each row containing the details of the points that are plotted (i.e. coordinates).}
#' \item{Lmat}{the matrix for transformation to the principal components.}
#' \item{Linv}{the inverse of \eqn{\mathbf{L}}.}
#' \item{eigenvalues}{the vector of eigenvalues of the covariance matrix of \eqn{\mathbf{X}}.}
#' \item{ax.one.unit}{one unit in the positive direction of each biplot axis.}
#' \item{e.vects}{the vector indicating which principal components are plotted in the biplot.}
#' \item{Vr}{the \code{1:dim.biplot} columns of \eqn{\mathbf{V}}.}
#' \item{dim.biplot}{the dimension of the biplot.}
#' \item{V.mat}{the matrix containing the right singular vectors of \eqn{\mathbf{X}}.}
#' \item{Sigma.mat}{the matrix with the singular values of \eqn{\mathbf{X}} on the diagonal.}
#' \item{U.mat}{the matrix containing the left singular vectors of \eqn{\mathbf{X}}.}
#' \item{class.means}{a logical value indicating whether group means are plotted in the biplot.}
#' \item{Zmeans}{the matrix of class mean coordinates that are plotted in the biplot.}
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
#' 
PCA <- function (bp, dim.biplot = c(2, 1, 3), e.vects = 1:ncol(bp$X), group.aes=NULL, show.class.means = FALSE,
                 correlation.biplot=FALSE)
{
  UseMethod("PCA")
}

#' Calculate elements for the PCA biplot
#'
#' @description This function performs calculations for the construction of a PCA biplot.
#'
#' @inheritParams PCA
#'
#' @return an object of class \code{PCA}, inherits from class \code{biplot}.
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

