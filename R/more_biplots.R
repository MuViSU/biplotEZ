# -------------------------------------------------------------------------------------------
#' Classification biplot method
#'
#' @description
#' This function produces a list of elements to be used for constructing a classification biplot.
#'
#' @param bp an object of class \code{biplot} obtained from preceding function \code{biplot()}.
#' @param Pmat a matrix containing the posterior probability for the classes
#' @param dim.biplot dimension of the biplot. Only values 1, 2 and 3 are accepted, with default \code{2}.
#' @param e.vects which eigenvectors (principal components) to extract, with default \code{1:dim.biplot}.
#' @param group.aes vector of the same length as the number of rows in the data matrix
#'                  for differentiated aesthetics for samples.
#' @param axes type of axes, defaults to "regression"
#' @param col colour of the classification regions
#' @param opacity opacity of classification regions
#' @param borders logical, indicating whether borders should be added to classification regions
#'
#' @return  Object of class biplot with the following elements:
#'
#' @usage classification(bp, Pmat, dim.biplot = c(2, 1, 3), e.vects = 1:ncol(bp$X),
#' group.aes=NULL, axes = "regression", col=ez.col, opacity=0.4, borders = FALSE)
#' @aliases classification
#'
#' @export
#'
#'@references
#' Gardner-Lubbe, S., 2016. A triplot for multiclass classification visualisation. \emph{Computational Statistics & Data Analysis}, 94, pp.20-32.
#'
#' @examples
#' biplot(iris[,1:4]) |> 
#' classification(predict(MASS::lda(Species ~ ., data = iris))$posterior)
#' # create a classification biplot
#' biplot(iris[,1:4]) |> 
#' classification(predict(MASS::lda(Species ~ ., data = iris))$posterior) |> 
#' plot()
classification <- function (bp, Pmat, dim.biplot = c(2, 1, 3), e.vects = 1:ncol(bp$X), 
                            group.aes=NULL, axes = "regression",
                            col=ez.col, opacity=0.4, borders = FALSE)
{
  UseMethod("classification")
}

#' classification biplot
#'
#' @description Performs calculations for a classification biplot.
#'
#' @inheritParams classification
#'
#' @return an object of class biplot.
#' @export
#'
classification.biplot <- function (bp, Pmat, dim.biplot = c(2, 1, 3), e.vects = 1:ncol(bp$X), 
                                   group.aes=NULL, axes = "regression",
                                   col=ez.col, opacity=0.4, borders = FALSE)
{
  while (length(col) < ncol(Pmat)) col <- c(col, col)
  col <- as.vector(col[1:ncol(Pmat)])
  col <- grDevices::adjustcolor(col,opacity[1])
  
  dim.biplot <- dim.biplot[1]
  if (dim.biplot != 1 & dim.biplot != 2 & dim.biplot != 3) stop("Only 1D, 2D and 3D biplots")
  e.vects <- e.vects[1:dim.biplot]
  if (!is.null(group.aes)) { bp$group.aes <- factor(group.aes)
                             bp$g.names <-levels(factor(group.aes))
                             bp$g <- length(bp$g.names)
  }
  
  X <- bp$X
  n <- bp$n
  p <- bp$p

  if (nrow(Pmat) != n) stop ("Number of rows in Pmat must correspond to your data set.")
  k <- ncol(Pmat)
  
  I.min.N <- diag(n) - matrix(1/n, nrow=n, ncol=n)
  I.min.K <- diag(k) - matrix(1/k, nrow=k, ncol=k)
  Pmat[Pmat < sqrt(.Machine$double.eps)] <- sqrt(.Machine$double.eps)
  A <- I.min.N %*% log(Pmat) %*% I.min.K
  
  svd.out <- svd(A)
  Z <- (svd.out$u %*% diag(svd.out$d))[, e.vects, drop = F]
  rownames(Z) <- rownames(X)
  
  if (axes == "regression")
    Vr <- t(solve(t(Z)%*%Z) %*% t(Z) %*% X)
    
  ax.one.unit <- 1/(diag(Vr %*% t(Vr))) * Vr

  bp$classify <- list(classify.regions = TRUE,
                      region.midpoints = svd.out$v[, e.vects, drop=F],
                      aes=list(col=col,opacity=opacity,borders=borders)  ) 
  bp$Z <- Z
  bp$ax.one.unit <- ax.one.unit
  bp$e.vects <- e.vects
  bp$dim.biplot <- dim.biplot

  bp
}

