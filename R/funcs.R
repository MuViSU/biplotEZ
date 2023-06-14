### MuViSU (Centre for Multi-Dimensional Data Visualisation
### biplotEZ functions
### ========================================================

#' Create a new biplot
#'
#' @description
#'  This function produces a list of elements to be used when producing a biplot
#'
#' @param datmat A dataframe or matrix containing all variables the user wants to analyse
#' @param center TRUE or FALSE
#' @param scaled TRUE or FALSE
#' @param group.aes Variable from the data to be used as a grouping variable
#' @param Title Title
#'
#' @return a list with the following components
#' \itemize{
#' \item{X}{Matrix of the centered and scaled numeric variables}
#' \item{raw.X}{Original data}
#' \item{center}{TRUE or FALSE, should the numeric data be centred?}
#' \item{scaled}{TRUE or FALSE, should the numeric data be scaled?}
#' \item{means}{Vector of means for each numeric variable}
#' \item{sd}{Vector of standard deviations for each numeric variable}
#' \item{group.aes}{Vector of category levels for the grouping variable. This is to be used for color, pch and cex specifications}
#' }
#' @export
#'
#' @examples
#' biplot(datmat = iris)
biplot <- function(datmat, group.aes = NULL, center = TRUE, scaled = FALSE, Title = NULL)
{
  dim.mat<-dim(datmat)
  if(is.null(dim.mat)) stop("Not enough variables to construct a biplot \n Consider using data with more columns")
  if(ncol(datmat)<2) stop("Not enough variables to construct a biplot \n Consider using data with more columns")

  # check for missing values
  na.vec.df<-na.action(na.omit(datmat))
  if (length(na.vec.df) == nrow(datmat))
  {
    stop("No observations left after deleting missing observations")
  }
  else if (!is.null(na.vec.df)) {
    print(paste(
      "Warning:",
      length(na.vec.df),
      "rows deleted due to missing values"
    ))
  }
  datmat<-datmat[complete.cases(datmat),]

  # Separating numeric and categorical data
    # X is the matrix of numeric variables, if there are no numeric columns, then X = NULL
    # Xcat is the dataframe of non-numeric variables, if there are no categorical columns, then Xcat = NULL

    type.vec <- NULL
    for (j in 1:ncol(datmat)) {
      if (is.numeric(datmat[, j])) {
        type.vec[j] <- "num"
      }
      else if (!is.numeric(datmat[, j])) {
        type.vec[j] <- "cat"
      }
      else
        NA
    }
    # type.vec contains either "num" or "cat"

    if ("num" %in% type.vec) {
      X <- as.matrix(datmat[, type.vec == "num", drop = FALSE])
    } else X <- NULL

    if ("cat" %in% type.vec) {
      Xcat <- as.data.frame(datmat[, type.vec == "cat", drop = FALSE])
    } else Xcat <- NULL

  # scaling of numeric data
  if(is.null(X))
    {  means <- NULL
       sd <- NULL
    }
  else
    {
      means <- apply(X, 2, mean)
      sd <- apply(X, 2, stats::sd)
      if (!center) {  X <- X
                      means <- rep(0, ncol(X))
                      sd <- rep(1, ncol(X))
      }
      else if (scaled) {X <- scale(X) }
           else { X <- scale(X, scale = FALSE)
                  sd <- rep(1, ncol(X))
                }
      if (is.null(rownames(X))) rownames(X) <- paste(1:nrow(X))
      if (is.null(colnames(X))) colnames(X) <- paste("V", 1:ncol(X), sep = "")
  }

  if(!is.null(Xcat))
    {
      if (is.null(rownames(Xcat))) rownames(Xcat) <- paste(1:nrow(Xcat))
      if (is.null(colnames(Xcat))) colnames(Xcat) <- paste("F", 1:ncol(Xcat), sep = "")
    }

  if(is.null(group.aes)) { group.aes <- factor(rep(1,nrow(datmat))) }
  g.names <-levels(group.aes)
  g <- length(g.names)

  object <- list(X = X, Xcat = Xcat, raw.X = datmat, na.action=na.vec.df, center=center, scaled=scaled,
                 means = means, sd = sd, n=nrow(X), p=ncol(X), group.aes = group.aes,g.names = g.names,g = g, Title = Title)
  class(object) <- "biplot"
  object
}

# ************************
# bp object of class PCA
# we are not (yet) creating an object of class PCA
# PCA should inherit from class biplot
# **************************

# -------------------------------------------------------------------------------------------
#' PCA method
#'
#' @param bp object of class PCA
#' @param dim.biplot dimension of the biplot. Only values 1, 2 and 3 are accepted, with default 2
#' @param e.vects which eigevectors (principal components) to extract, defaults to `1:dim.biplot`
#' @param group.aes optional vector of the same length as the number of rows in the data matrix
#'                  for differentiated aesthetics for samples
#' @param correlation.biplot defaults to FALSE. If FALSE, the distances between sample points are
#'                           optimally approximated in the biplot. If TRUE, the correlations between
#'                           variables are optimally approximated by the cosine of the angles between
#'                           axes. See Gabriel (1971) The biplot graphic display of matrices with application
#'                           to principal component analysis. Biometrika, 58(3), pp.453-467.
#'
#'
#' @return  object of class PCA
#' @export
#'
#' @examples
PCA <- function (bp, dim.biplot = c(2, 1, 3), e.vects = 1:ncol(bp$X), group.aes=NULL,
                 correlation.biplot=FALSE, ...)
{
  UseMethod("PCA")
}

#' PCA biplot
#'
#' @description Performs calculations for a PCA biplot
#'
#' @inheritParams PCA
#'
#' @return an object of class ??
#' @export
#'
#' @examples
#' biplot(iris[,1:4]) |> PCA()
#'
PCA.biplot <- function (bp, dim.biplot = c(2, 1, 3), e.vects = 1:ncol(bp$X), group.aes=NULL,
                        correlation.biplot=FALSE, ...)
{
  dim.biplot <- dim.biplot[1]
  if (dim.biplot != 1 & dim.biplot != 2 & dim.biplot != 3) stop("Only 1D, 2D and 3D biplots")
  e.vects <- e.vects[1:dim.biplot]
  if (!is.null(group.aes)) bp$group.aes <- factor(group.aes)
  group.aes <- bp$group.aes

  if (!bp$center)
  {  warning("PCA requires a centred datamatrix. Your data was centred before computation. Use center = TRUE in the call to biplot()")
    bp <- biplot (bp$X, center = TRUE, scaled=bp$scaled)
  }
  X <- bp$X
  n <- bp$n
  p <- bp$p
  J <- nlevels(group.aes)

  svd.out <- svd(X)
  V.mat <- svd.out$v
  U.mat <- svd.out$u
  Sigma.mat <- diag(svd.out$d)
  Vr <- svd.out$v[, e.vects, drop = F]

  if (correlation.biplot)
  {

    if (dim.biplot>1) lambda.r <- diag(svd(t(X) %*% X)$d[1:dim.biplot])
    else lambda.r <- matrix(svd(t(X) %*% X)$d, nrow=1, ncol=1)
    Z <- sqrt(n - 1) * X %*% Vr %*% sqrt(solve(lambda.r))
  }
  else { Z <- X %*% Vr }
  rownames(Z) <- rownames(X)

  if (correlation.biplot)
    axes.direction <- (sqrt(n - 1)/(diag(Vr %*% lambda.r %*% t(Vr)))) * Vr %*% sqrt(lambda.r)
  else
    axes.direction <- 1/(diag(Vr %*% t(Vr))) * Vr

  bp$Z <- Z
  bp$Vr <- Vr
  bp$Xhat <- Z %*% t(bp$Vr)
  bp$axes.direction <- axes.direction
  if (bp$scaled) bp$Xhat <- scale(bp$Xhat, center=F, scale=1/bp$sd)
  if (bp$center) bp$Xhat <- scale(bp$Xhat, center=-1*bp$means, scale=F)
  bp
}

# ----------------------------------------------------------------------------------------------
#' Formatting of samples in biplot
#'
#' @param bp biplot objects
#' @param col sample colour
#' @param pch sample plotting character
#' @param cex sample character expansion
#' @param label logical, whether samples should be labelled or not
#' @param label.cex label character expansion
#' @param label.side side of the plotting character where label appears
#' @param connected logical, whether samples are connected in order of rows of data matrix
#' @param alpha opacity
#'
#' @return
#' @export
#'
#' @examples
samples <- function (bp, col = c("blue","green","gold","cyan","magenta","black","red","grey","purple","salmon"),
                         pch = 3, cex = 1, label = F, label.cex = 0.75, label.side = "bottom",
                         connected=F, alpha = 1)
{
   g <- nlevels(factor(bp$group.aes))
   while (length(col) < g) col <- c(col, col)
   col <- as.vector(col[1:g])
   while (length(pch) < g) pch <- c(pch, pch)
   pch <- as.vector(pch[1:g])
   while (length(cex) < g) cex <- c(cex, cex)
   cex <- as.vector(cex[1:g])
   while (length(label) < g) label <- c(label, label)
   label <- as.vector(label[1:g])
   while (length(label.cex) < g) label.cex <- c(label.cex, label.cex)
   label.cex <- as.vector(label.cex[1:g])
   while (length(label.side) < g) label.side <- c(label.side, label.side)
   label.side <- as.vector(label.side[1:g])
   while (length(alpha) < g) alpha <- c(alpha, alpha)
   if (length(connected)>1) connected <- connected[1]
   alpha <- as.vector(alpha[1:g])

   bp$samples = list(col = col, pch = pch, cex = cex, label = label, label.cex = label.cex,
                     label.side = label.side, connected=connected, alpha = alpha)
   bp
}

# ----------------------------------------------------------------------------------------------
#' Plotting of the biplot
#'
#' @param x object of class biplot
#' @param y NULL
#' @param ... more arguments
#'
#' @return biplot is created on graphics device
#' @export
#'
#' @examples
plot.biplot <- function(x, y = NULL, ...)
{
  sample.components <- names(x$samples)
  if (!("col") %in% sample.components)
    x$samples$col <- c("blue","green","gold","cyan","magenta","black","red","grey","purple","salmon")

  plot(x$Z, col=x$samples$col)
  invisible(NULL)
}

