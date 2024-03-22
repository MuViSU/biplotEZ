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
#'            classes=bp$classes, weightedCVA = "weighted", show.class.means = TRUE)
#' @aliases CVA
#'
#' @export
#'
#' @examples
#' biplot(iris[,1:4]) |> CVA(classes=iris[,5])
#' # create a CVA biplot
#' biplot(iris[,1:4]) |> CVA(classes=iris[,5]) |> plot()

CVA <- function(bp, dim.biplot = c(2,1,3), e.vects = 1:ncol(bp$X), classes=bp$classes,
                weightedCVA = "weighted", show.class.means = TRUE)
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
                       weightedCVA = "weighted", show.class.means = TRUE)
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
  if (K == 1) stop ("Only 2D biplots currently implemented. Maximum dimension of the canonical space is min(number of variables, number of groups-1)")

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
  M <- W_minhalf %*% V

  Z <- X %*% M[,e.vects]
  Minv <- solve(M)[e.vects, , drop = F]
  ax.one.unit <- 1/(diag(t(Minv[e.vects,]) %*% Minv[e.vects,])) * t(Minv[e.vects,])

  bp$Gmat <- G
  bp$Xmeans <- X_bar
  bp$Z <- Z
  bp$Zmeans <- bp$Xmeans %*% M[,e.vects]
  bp$ax.one.unit <- ax.one.unit
  bp$Lmat <- M
  bp$eigenvalues <- svd.out$d
  bp$e.vects <- e.vects
  bp$Cmat <- Cmat
  bp$Bmat <- B
  bp$Wmat <- W
  bp$Mrr <- Minv
  bp$class.means <- show.class.means

  class(bp) <- append(class(bp),"CVA")
  bp
}


