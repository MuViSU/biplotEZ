### MuViSU (Centre for Multi-Dimensional Data Visualisation
### biplotEZ functions
### ========================================================

#' First step to create a new biplot with \pkg{biplotEZ}
#'
#' @description
#' This function produces a list of elements to be used when producing a biplot,
#' which provides a useful data analysis tool and allows the visual appraisal
#' of the structure of large data matrices. Biplots are the
#' multivariate analogue of scatter plots. They approximate the multivariate
#' distribution of a sample in a few dimensions and they superimpose on this
#' display representations of the variables on which the samples are measured.
#'
#' @param data Required argument. A dataframe or matrix containing all variables the user wants to analyse.
#' @param group.aes Optional argument. Variable from the data to be used as a grouping variable.
#' @param center Logical argument indicating whether {data} should be column centered, with default \code{TRUE}.
#' @param scaled Logical argument indicating whether {data} should be standardized to unit column variances, with default \code{FALSE}.
#' @param Title Optional argument. Title of the biplot to be rendered, enter text in "  ".
#'
#' @return A list with the following components is available:
#' \item{X}{Matrix of the centered and scaled numeric variables.}
#' \item{raw.X}{Original data.}
#' \item{center}{TRUE or FALSE, as specified.}
#' \item{scaled}{TRUE or FALSE, as specified.}
#' \item{means}{Vector of means for each numeric variable.}
#' \item{sd}{Vector of standard deviations for each numeric variable.}
#' \item{group.aes}{Vector of category levels for the grouping variable. This is to be used for colour, pch and cex specifications.}
#'
#' @references
#' Gabriel, K.R. 1971. The biplot graphic display of matrices with application to principal component analysis. <em>Biometrika.<em> 58(3):453–467.<br><br>
#' Gower, J., Gardner-Lubbe, S. & Le Roux, N. 2011. <em>Understanding Biplots.<em> Chichester, England: John Wiley & Sons Ltd.<br><br>
#' Gower, J.C. & Hand, D.J. 1996. <em>Biplots.<em> London: Chapman & Hall.
#'
#' @usage biplot(data, group.aes = NULL, center = TRUE, scaled = FALSE,
#' Title = NULL)
#'
#' @export
#'
#' @examples
#' biplot(data = iris)
biplot <- function(data, group.aes = NULL, center = TRUE, scaled = FALSE, Title = NULL)
{
  dim.mat<-dim(data)
  if(is.null(dim.mat)) stop("Not enough variables to construct a biplot \n Consider using data with more columns")
  if(ncol(data)<2) stop("Not enough variables to construct a biplot \n Consider using data with more columns")

  # check for missing values
  na.vec.df <- stats::na.action(stats::na.omit(data))
  if (length(na.vec.df) == nrow(data)) stop("No observations left after deleting missing observations")
  else if (!is.null(na.vec.df))  warning(paste(length(na.vec.df), "rows deleted due to missing values"))
  data<-data[stats::complete.cases(data),]
  if (!is.null(group.aes) & length(na.vec.df) > 0) group.aes <- group.aes[na.vec.df]

  # Separating numeric and categorical data
  type.vec <- unlist(lapply(data, is.numeric), use.names = FALSE)
  if (sum(type.vec)>0) X <- as.matrix(data[, type.vec, drop=FALSE])
  else X <- NULL
  if (sum(type.vec)<length(type.vec)) Xcat <- as.data.frame(data[, !type.vec, drop=FALSE])
  else Xcat <- NULL

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
    else if (scaled) { X <- scale(X) }
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

  if(is.null(group.aes)) group.aes <- factor(rep(1,nrow(data)))
  else group.aes <- factor(group.aes)
  g.names <-levels(group.aes)
  g <- length(g.names)

  object <- list(X = X, Xcat = Xcat, raw.X = data, na.action=na.vec.df, center=center, scaled=scaled,
                 means = means, sd = sd, n=nrow(X), p=ncol(X), group.aes = group.aes,g.names = g.names,g = g,
                 Title = Title)
  class(object) <- "biplot"
  object
}

# -------------------------------------------------------------------------------------------
#' Principal Component Analysis (PCA) method
#'
#' @description
#' This function produces a list of elements to be used for PCA biplot construction.
#'
#' @param bp An object of class \code{biplot} obtained from preceding function \code{biplot()}.
#' @param dim.biplot Dimension of the biplot. Only values 1, 2 and 3 are accepted, with default \code{2}.
#' @param e.vects Which eigenvectors (principal components) to extract, with default \code{1:dim.biplot}.
#' @param group.aes Optional argument. Vector of the same length as the number of rows in the data matrix
#'                  for differentiated aesthetics for samples.
#' @param correlation.biplot If \code{FALSE}, the distances between sample points are
#'                           optimally approximated in the biplot. If \code{TRUE}, the correlations between
#'                           variables are optimally approximated by the cosine of the angles between
#'                           axes. Default is \code{FALSE}.
#'
#'
#' @return  Object of class PCA with the following elements:
#' \item{X}{Matrix of the centered and scaled numeric variables.}
#' \item{Xcat}{Matrix of the categorical variables.}
#' \item{raw.X}{Original data.}
#' \item{na.action}{Vector of observations that have been removed.}
#' \item{center}{TRUE or FALSE, as specified.}
#' \item{scaled}{TRUE or FALSE, as specified.}
#' \item{means}{Mean of each numerical variable.}
#' \item{sd}{Standard deviation of each numerical variable.}
#' \item{n}{Number of observations.}
#' \item{p}{Number of variables.}
#' \item{group.aes}{Vector of the same length as the number of rows in the data matrix for differentiated aesthetics for samples.}
#' \item{g.names}{Descriptive name to be used for group labels.}
#' \item{g}{Number of groups.}
#' \item{Title}{Title of the biplot to be rendered, as specified.}
#' \item{Z}{Matrix with each row containing the details of the point to be plotted (i.e. coordinates).}
#' \item{Vr}{Matrix consisting of the eigenvectors as columns.}
#' \item{Xhat}{Predictions of the samples.}
#' \item{ax.one.unit}{One unit in the positive direction of each biplot axis.}
#'
#' @usage PCA(bp, dim.biplot = c(2, 1, 3), e.vects = 1:ncol(bp$X),
#' group.aes = NULL, correlation.biplot = FALSE)
#'
#' @export
#'
#'@references
#' Gabriel, K.R. 1971. The biplot graphic display of matrices with application to principal component analysis. <em>Biometrika.<em> 58(3):453–467.
#'
#' @examples
#' biplot(iris[,1:4]) |> PCA()
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
#' @return an object of class PCA
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
  if (!is.null(group.aes)) { bp$group.aes <- factor(group.aes)
  bp$g.names <-levels(factor(group.aes))
  bp$g <- length(bp$g.names)
  }

  if (!bp$center)
  {  warning("PCA requires a centred datamatrix. Your data was centred before computation. Use center = TRUE in the call to biplot()")
    bp <- biplot (bp$X, center = TRUE, scaled=bp$scaled)
  }
  X <- bp$X
  n <- bp$n
  p <- bp$p

  svd.out <- svd(X)
  V.mat <- svd.out$v
  U.mat <- svd.out$u
  Sigma.mat <- diag(svd.out$d)
  Vr <- svd.out$v[, e.vects, drop = FALSE]

  if (correlation.biplot)
  {

    if (dim.biplot>1) lambda.r <- diag(svd(t(X) %*% X)$d[1:dim.biplot])
    else lambda.r <- matrix(svd(t(X) %*% X)$d, nrow=1, ncol=1)
    Z <- sqrt(n - 1) * X %*% Vr %*% sqrt(solve(lambda.r))
  }
  else { Z <- X %*% Vr }
  rownames(Z) <- rownames(X)

  if (correlation.biplot)
    ax.one.unit <- (sqrt(n - 1)/(diag(Vr %*% lambda.r %*% t(Vr)))) * Vr %*% sqrt(lambda.r)
  else
    ax.one.unit <- 1/(diag(Vr %*% t(Vr))) * Vr

  bp$Z <- Z
  bp$Vr <- Vr
  bp$Xhat <- Z %*% t(bp$Vr)
  bp$ax.one.unit <- ax.one.unit
  if (bp$scaled) bp$Xhat <- scale(bp$Xhat, center=FALSE, scale=1/bp$sd)
  if (bp$center) bp$Xhat <- scale(bp$Xhat, center=-1*bp$means, scale=FALSE)
  class(bp)<-append(class(bp),"PCA")
  bp
}
# ---------------------------------------------------------------------------------------------

# -------------------------------------------------------------------------------------------
#' Canonical Variate Analysis (CVA) method
#'
#' @description
#' This function produces a list of elements to be used for CVA biplot construction.
#'
#' @param bp Object of class biplotAn object of class \code{biplot} obtained from preceding function \code{biplot()}.
#' @param dim.biplot Dimension of the biplot. Only values 1, 2 and 3 are accepted, with default \code{2}.
#' @param e.vects Which eigenvectors (canonical variates) to extract, with default \code{1:dim.biplot}.
#' @param group.aes Vector of the same length as the number of rows in the data matrix
#'                  for differentiated aesthetics for samples.
#' @param weightedCVA The default is "weighted", specifying a weighted CVA to be performed. Other possible values are "unweightedI" and "unweightedCent".
#'
#'
#' @return  Object of class CVA with the following elements:
#' \item{X}{Matrix of the centered and scaled numeric variables.}
#' \item{Xcat}{Matrix of the categorical variables.}
#' \item{raw.X}{Original data.}
#' \item{na.action}{Vector of observations that have been removed.}
#' \item{center}{TRUE or FALSE, as specified.}
#' \item{scaled}{TRUE or FALSE, as specified.}
#' \item{means}{Mean of each numerical variable.}
#' \item{sd}{Standard deviation of each numerical variable.}
#' \item{n}{Number of observations.}
#' \item{p}{Number of variables.}
#' \item{group.aes}{Vector of the same length as the number of rows in the data matrix for differentiated aesthetics for samples. }
#' \item{g.names}{Descriptive name to be used for group labels.}
#' \item{g}{Number of groups.}
#' \item{Title}{Title of the biplot to be rendered, as specified.}
#' \item{Z}{Matrix with each row containing the details of the point to be plotted (i.e. coordinates).}
#' \item{Xhat}{Predictions of the samples.}
#' \item{ax.one.unit}{One unit in the positive direction of each biplot axis.}
#'
#' @usage CVA(bp, dim.biplot = c(2, 1, 3), e.vects = 1:ncol(bp$X),
#' group.aes = bp$group.aes,weightedCVA = "weighted")
#'
#' @export
#'
#' @examples
#' biplot(iris[,1:4],iris[,5]) |> CVA()
CVA <- function(bp, dim.biplot = c(2,1,3), e.vects = 1:ncol(bp$X), group.aes = bp$group.aes,
                weightedCVA = "weighted",...)
{
  UseMethod("CVA")
}

#' CVA biplot
#'
#' @description Performs calculations for a CVA biplot
#'
#' @inheritParams CVA
#'
#' @return an object of class CVA
#' @export
#'
#' @examples
#' biplot(iris[,1:4],iris[,5]) |> CVA()
#'
CVA.biplot <- function(bp, dim.biplot = c(2,1,3), e.vects = 1:ncol(bp$X), group.aes = bp$group.aes,
                       weightedCVA = "weighted", ...)
{
  dim.biplot <- dim.biplot[1]
  if (dim.biplot != 1 & dim.biplot != 2 & dim.biplot != 3) stop("Only 1D, 2D and 3D biplots")
  e.vects <- e.vects[1:dim.biplot]
  if (!is.null(group.aes)) { bp$group.aes <- factor(group.aes)
  bp$g.names <- levels(factor(group.aes))
  bp$g <- length(bp$g.names)
  }

  X <- bp$X
  n <- bp$n
  p <- bp$p
  G <- indmat(group.aes)
  J <- ncol(G)
  K <- min(p, J-1)
  if (K == 1) stop ("Only 2D biplots currently implemented. Maximum dimension of the canonical space is min(number of variables, number of groups-1)")

  N <- t(G) %*% G
  X_bar <- solve(N) %*% t(G) %*% X
  W <- t(X) %*% X - t(X_bar) %*% N %*% X_bar
  B <- t(X_bar) %*% N %*% X_bar

  W_minhalf <- eigen(W)$vectors %*% diag(1/sqrt(eigen(W)$values)) %*% t(eigen(W)$vectors)
  L <- W_minhalf
  if (weightedCVA == "weighted")
    Cmat <- N
  if (weightedCVA == "unweightedI")
    Cmat <- diag(J)
  if (weightedCVA == "unweightedCent")
    Cmat <- diag(J) - matrix(1/J, nrow = J, ncol = J)
  if (is.na(match(weightedCVA, c("weighted", "unweightedI", "unweightedCent"))))
    stop(" Argument 'weightedCVA' must be one of 'weighted','unweightedI','unweightedCent' ")
  eigenresult <- eigen(W_minhalf %*% t(X_bar) %*% Cmat %*% X_bar %*% W_minhalf)
  V <- eigenresult$vectors
  M <- L %*% V

  Z <- X %*% M[,1:dim.biplot]
  ax.one.unit <- solve(diag(diag(t(solve(M)[1:dim.biplot,]) %*% solve(M)[1:dim.biplot,]))) %*% t(solve(M)[1:dim.biplot,])

  bp$Z <- Z
  bp$ax.one.unit <- ax.one.unit
  bp$Xhat <- X %*% M %*% solve(M)
  if (bp$scaled) Xhat <- scale(bp$Xhat, center=FALSE, scale=1/bp$sd)
  if (bp$center) Xhat <- scale(bp$Xhat, center=-1*bp$means, scale=FALSE)

  class(bp) <- append(class(bp),"CVA")
  bp
}

# ---------------------------------------------------------------------------------------------

ez.col <- c("blue","green","gold","cyan","magenta","black","red","grey","purple","salmon")

# ----------------------------------------------------------------------------------------------

#' indmat
#'
#' @param groep.vec Grouping
#'
#' @return Returns an indicator matrix.
#' \code{Y}, indicator matrix per categorical variable
#'
#' @export
#'
#' @noRd
indmat  <- function (groep.vec)
{
  elements <- levels(factor(groep.vec))
  Y <- matrix(0, nrow = length(groep.vec), ncol = length(elements))
  dimnames(Y) <- list(NULL, paste(elements))
  for (i in 1:length(elements)) Y[groep.vec == elements[i], i] <- 1
  return(Y)
}

# ----------------------------------------------------------------------------------------------
#' Aesthetics for biplot samples
#'
#' @description
#' This function allows formatting changes to samples.
#'
#' @param bp An object of class \code{biplot}.
#' @param col Sample colour, with default \code{blue}.
#' @param pch Sample plotting character, with default \code{+}.
#' @param cex Sample character expansion, with default \code{1}.
#' @param label Logical argument, whether samples should be labelled or not, with default \code{FALSE}.
#' @param label.cex Label text expansion, with default \code{0.75}.
#' @param label.side Side of the plotting character where label appears, with default \code{bottom}.
#' @param connected Logical argument, whether samples are connected in order of rows of data matrix, with default \code{FALSE}.
#' @param alpha Opacity of sample plotting character, default is \code{1}.
#'
#' @return A list with the following components is available:
#' \item{col}{Colour of the samples.}
#' \item{pch}{Plotting character of the samples.}
#' \item{cex}{Expansion of the plotting character of the samples.}
#' \item{label}{TRUE or FALSE, as specified.}
#' \item{label.cex}{Expansion of the label.}
#' \item{label.side}{Side at which to plot the label of samples.}
#' \item{connected}{TRUE or FALSE, as specified.}
#' \item{alpha}{Opacity of the samples.}
#' \item{g}{Number of groups.}
#'
#' @usage
#' samples (bp,  col = ez.col, pch = 3, cex = 1, label = FALSE,
#' label.cex = 0.75, label.side = "bottom", connected = FALSE, alpha = 1)
#'
#' @export
#'
#' @examples biplot(iris[,1:4]) |> PCA() |> samples(col="purple",pch=15) |> plot()
samples <- function (bp,  col = ez.col,
                     pch = 3, cex = 1, label = FALSE, label.cex = 0.75, label.side = "bottom",
                     connected=FALSE, alpha = 1)
{
  ##g available in bp
  #to test now without bp$g.names
  g <- bp$g
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
                    label.side = label.side, connected=connected, alpha = alpha, g = g)
  bp
}

#' Aesthetics for biplot axes
#'
#' @description
#' This function allows formatting changes to axes.
#'
#'
#' @param bp An object of class \code{biplot}.
#' @param X.names Column names of \code{bp}.
#' @param which  Vector of columns to be displayed in the biplot, with default \code{1:bp$p}.
#' @param col Axis colour, with default \code{grey(0.7)}.
#' @param lwd Axis line width, with default \code{1}.
#' @param lty Axis line type, with default \code{1}.
#' @param label.dir Direction of axis label, with default \code{Orthog}.
#' @param label.col Axis label colour, with default, \code{col}.
#' @param label.cex Aixs label expansion, with default \code{0.75}.
#' @param label.dist Axis label distance from axis, with default \code{0}.
#' @param ticks Number of tick marks per axis, with default \code{5}.
#' @param tick.col Tick mark colour, with default \code{col}.
#' @param tick.size Tick mark size, with default \code{1}.
#' @param tick.label Logical argument, whether axes should be labelled or not, with default \code{TRUE}.
#' @param tick.label.col Tick mark label colour, with default \code{tick.col}.
#' @param tick.label.cex Tick mark label expansion, with default \code{0.6}.
#' @param tick.label.side Side of the tick mark label, with default \code{left}.
#' @param tick.label.offset Tick mark label offset, with default \code{0.5}.
#' @param tick.label.pos Side of the tick mark label, with default \code{below}.
#' @param predict.col Predicted samples colour, with default \code{col}.
#' @param predict.lwd Predicted samples line width, with default \code{lwd}.
#' @param predict.lty Predicted samples line type, with default \code{lty}.
#' @param ax.names Vector of size \code{p} containing user defined names for the variables.
#' @param orthogx ,the horizontal translation, with default \code{0}.
#' @param orthogy ,the vertical translation with default \code{0}.
#'
#' @return A list with the following components is available:
#' \item{which}{Vector of the columns displayed as axes.}
#' \item{col}{Vector of axis colours.}
#' \item{lwd}{Vector of axis line widths.}
#' \item{lty}{Vector of axis line types.}
#' \item{label.dir}{Direction of the axis labels.}
#' \item{label.col}{Vector of axis label colours.}
#' \item{label.cex}{Vector of axis labels expansions.}
#' \item{label.dist}{Vector of axis label distances from axes.}
#' \item{ticks}{Vector representing the number of tick marks per axis.}
#' \item{tick.col}{Vector of tick mark colours.}
#' \item{tick.size}{Vector of tick mark sizes.}
#' \item{tick.label}{Vector of logical values indicating whether axes are labelled.}
#' \item{tick.label.col}{Vector of tick mark label colours.}
#' \item{tick.label.cex}{Vector of tick mark label expansions.}
#' \item{tick.label.side}{Vector of the side of tick mark labels.}
#' \item{tick.label.offset}{Vector of tick mark label offsets.}
#' \item{tick.label.pos}{Vector of the side of tick mark labels.}
#' \item{predict.col}{Vector of colours for the predicted samples.}
#' \item{predict.lty}{Vector of line types for the predicted samples.}
#' \item{predict.lwd}{Vector of line widths for the predicted samples.}
#' \item{names}{Vector of variable names defined by the user.}
#' \item{orthogx}{Vector of the horisontal translations for each axis.}
#' \item{orthogy}{Vector of the vertical translations for each axis.}
#'
#' @usage
#' axes(bp, X.names=colnames(bp$X), which = 1:bp$p, col = grey(0.7),
#' lwd = 1, lty = 1, label.dir = "Orthog", label.col = col, label.cex = 0.75,
#'  label.dist = 0, ticks = 5, tick.col = col, tick.size = 1, tick.label = TRUE,
#'  tick.label.col = tick.col, tick.label.cex = 0.6, tick.label.side = "left",
#'  tick.label.offset = 0.5, tick.label.pos = 1, predict.col = col, predict.lwd = lwd,
#'   predict.lty = lty, ax.names = X.names, orthogx = 0, orthogy = 0)
#'
#' @export
#'
#' @importFrom grDevices grey
#'
#' @examples
#' biplot(iris[,1:4]) |> PCA() |> axes(col="purple") |> plot()
#' biplot(iris[,1:4]) |> PCA() |> samples(col="purple",pch=15) |> axes() |> plot()
#'
axes <- function (bp, X.names=colnames(bp$X), which = 1:bp$p, col = grey(0.7), lwd = 1, lty = 1,
                  label.dir = "Orthog", label.col = col, label.cex = 0.75, label.dist = 0, ticks = 5,
                  tick.col = col, tick.size = 1, tick.label = TRUE, tick.label.col = tick.col, tick.label.cex = 0.6,
                  tick.label.side = "left", tick.label.offset = 0.5, tick.label.pos = 1,
                  predict.col = col, predict.lwd = lwd, predict.lty = lty, ax.names = X.names,
                  orthogx = 0, orthogy = 0)
{
  if (!all(is.numeric(which))) which <- match(which, X.names, nomatch = 0)
  p <- bp$p
  which <- which[which <= p]
  which <- which[which > 0]
  ax.num <- length(which)
  while (length(col) < ax.num) col <- c(col, col)
  col <- as.vector(col[1:ax.num])
  while (length(lwd) < ax.num) lwd <- c(lwd, lwd)
  lwd <- as.vector(lwd[1:ax.num])
  while (length(lty) < ax.num) lty <- c(lty, lty)
  lty <- as.vector(lty[1:ax.num])
  if (label.dir != "Orthog" & label.dir != "Hor" & label.dir != "Paral")
    stop("Incorrect specification of axis label direction")
  while (length(label.col) < ax.num) label.col <- c(label.col, label.col)
  label.col <- as.vector(label.col[1:ax.num])
  while (length(label.cex) < ax.num) label.cex <- c(label.cex, label.cex)
  label.cex <- as.vector(label.cex[1:ax.num])
  while (length(label.dist) < ax.num) label.dist <- c(label.dist, label.dist)
  label.dist <- as.vector(label.dist[1:ax.num])
  while (length(ticks) < ax.num) ticks <- c(ticks, ticks)
  ticks <- as.vector(ticks[1:ax.num])
  while (length(tick.col) < ax.num) tick.col <- c(tick.col, tick.col)
  tick.col <- as.vector(tick.col[1:ax.num])
  while (length(tick.size) < ax.num) tick.size <- c(tick.size, tick.size)
  tick.size <- as.vector(tick.size[1:ax.num])
  while (length(tick.label) < ax.num) tick.label <- c(tick.label, tick.label)
  tick.label <- as.vector(tick.label[1:ax.num])
  while (length(tick.label.col) < ax.num) tick.label.col <- c(tick.label.col, tick.label.col)
  tick.label.col <- as.vector(tick.label.col[1:ax.num])
  while (length(tick.label.cex) < ax.num) tick.label.cex <- c(tick.label.cex, tick.label.cex)
  tick.label.cex <- as.vector(tick.label.cex[1:ax.num])
  while (length(tick.label.side) < ax.num) tick.label.side <- c(tick.label.side, tick.label.side)
  tick.label.side <- as.vector(tick.label.side[1:ax.num])
  while (length(tick.label.offset) < ax.num) tick.label.offset <- c(tick.label.offset, tick.label.offset)
  tick.label.offset <- as.vector(tick.label.offset[1:ax.num])
  while (length(tick.label.pos) < ax.num) tick.label.pos <- c(tick.label.pos, tick.label.pos)
  tick.label.pos <- as.vector(tick.label.pos[1:ax.num])
  while (length(predict.col) < ax.num) predict.col <- c(predict.col, predict.col)
  predict.col <- as.vector(predict.col[1:ax.num])
  while (length(predict.lwd) < ax.num) predict.lwd <- c(predict.lwd, predict.lwd)
  predict.lwd <- as.vector(predict.lwd[1:ax.num])
  while (length(predict.lty) < ax.num) predict.lty <- c(predict.lty, predict.lty)
  predict.lty <- as.vector(predict.lty[1:ax.num])
  ax.names <- ax.names[which]
  while (length(ax.names) < p) ax.names <- c(ax.names, "")
  ax.names <- as.vector(ax.names[1:ax.num])
  while (length(orthogx) < p) orthogx <- c(orthogx, orthogx)
  orthogx <- as.vector(orthogx[1:p])
  while (length(orthogy) < p) orthogy <- c(orthogy, orthogy)
  orthogy <- as.vector(orthogy[1:p])
  bp$axes = list(which = which, col = col, lwd = lwd, lty = lty, label.dir = label.dir, label.col = label.col, label.cex = label.cex,
                 label.dist = label.dist, ticks = ticks, tick.col = tick.col, tick.size = tick.size, tick.label = tick.label,
                 tick.label.col = tick.label.col, tick.label.cex = tick.label.cex, tick.label.side = tick.label.side,
                 tick.label.offset = tick.label.offset, tick.label.pos = tick.label.pos,
                 predict.col = predict.col, predict.lty = predict.lty, predict.lwd = predict.lwd,
                 names = ax.names, orthogx = orthogx, orthogy = orthogy)
  bp
}

#' alpha-bag formatting
#'
#' @param g number of groups
#' @param g.names names of groups
#' @param alpha size of alpha bag
#' @param which which group to select
#' @param col colour of bags
#' @param lty line type of bags
#' @param lwd line width of bags
#' @param max maximum number of samples to be included in a bag
#'
#' @noRd
#'
control.alpha.bags <- function (g, g.names, alpha, which, col, lty, lwd, max)
{
  if (!all(is.numeric(which)))
    which <- match(which, g.names, nomatch = 0)
  which <- which[which <= g]
  which <- which[which > 0]
  rep.num <- length(which)
  rep.alp <- length(alpha)

  if (length(alpha)>1)
  {
    temp.mat <- expand.grid(which, alpha)
    which <- temp.mat[,1]
    alpha <- temp.mat[,2]
  }
  bag.num <- length(which)
  while (length(alpha) < bag.num)
    alpha <- c(alpha, alpha)
  alpha <- as.vector(alpha[1:bag.num])
  if (any(alpha < 0 |
          alpha > 0.99))
    stop(message = "alpha not to be negative or larger than 0.99")
  alpha.entered <- alpha
  while (length(col) < bag.num)
    col <- c(col, col)
  col <- as.vector(rep(col[1:rep.num],rep.alp))
  while (length(lty) < bag.num)
    lty <- c(lty, lty)
  lty <- as.vector(rep(lty[1:rep.alp],each=rep.num))
  while (length(lwd) < bag.num)
    lwd <- c(lwd, lwd)
  lwd <- as.vector(rep(lwd[1:rep.alp],each=rep.num))
  list(which = which, alpha = alpha, col = col, lty = lty, lwd = lwd, max = max)
}

#' Create alpha bags
#'
#' @description
#' This function produces \eqn{\alpha}-bags, which is a useful graphical summary of the
#' scatter plot. The alpha-bag refers to a contour which contains \eqn{\alpha}% of the observations.
#'
#' @param bp An object of class \code{biplot}.
#' @param alpha Value between 0 and 1 to determine coverage of bag (\eqn{\alpha}), with default \code{0.95}.
#' @param which The selection of groups or classes to be fitted with \eqn{\alpha}-bags.
#' @param col Vector of colours for the \eqn{\alpha}-bags. Multiple \eqn{\alpha} bags for one group will be displayed in the same colour.
#' @param lty Vector of line types for the \eqn{\alpha}-bags. The same line type will be used per value of \eqn{\alpha}.
#' @param lwd Vector of line widths for the \eqn{\alpha}-bags. The same line width will be used per value of \eqn{\alpha}.
#' @param max Maximum number of samples to include in \eqn{\alpha}-bag calculations, with default 2500. If
#'              more samples are in the group, a random sample of size max is taken for the computations.
#'
#' @return  A list with the following components is available:
#' \item{alpha.bags}{List of coordinates for the \eqn{\alpha}-bags for each group.}
#' \item{col}{Vector of colours for the \eqn{\alpha}-bags.}
#' \item{lty}{Vector of line types for the \eqn{\alpha}-bags.}
#' \item{lwd}{Vector of line widths for the \eqn{\alpha}-bags.}
#'
#' @references
#' Gower, J., Gardner-Lubbe, S. & Le Roux, N. 2011. <em>Understanding Biplots.<em> Chichester, England: John Wiley & Sons Ltd.<br><br>
#'
#' @export
#' @usage alpha.bags(bp, alpha=0.95, which = NULL, col = ez.col, lty = 1,
#' lwd = 1, max = 2500)
#'
#' @examples
#' biplot (iris[,1:4]) |> PCA(group.aes=iris[,5]) |> alpha.bags(alpha=0.95) |> plot()
#' biplot (iris[,1:4],group.aes=iris[,5]) |> PCA() |> alpha.bags(alpha=0.95) |> plot()
#'
alpha.bags <- function(bp, alpha=0.95, which = NULL, col = ez.col, lty = 1, lwd = 1, max = 2500)
{
  g <- bp$g
  g.names <- bp$g.names
  if (is.null(which)) which <- 1:g
  control.output <- control.alpha.bags (g=g, g.names=g.names, alpha=alpha, which=which,
                                        col=col, lty=lty, lwd=lwd, max=max)

  all.alpha.bags <- list()
  for(a in 1:length(control.output$which))
  {
    cat (paste("Computing", control.output$alpha[a], "-bag for",g.names[control.output$which[a]], "\n"))
    Zgroup <- bp$Z[bp$group.aes==g.names[control.output$which[a]],]
    calc <- calc.alpha.bags(Zgroup, aa=control.output$alpha[a], approx.limit=control.output$max)$xy[,1:2]
    all.alpha.bags[[a]] <- calc
  }
  names(all.alpha.bags) <- paste (g.names[control.output$which], control.output$alpha, sep="-")

  if (is.null(bp$alpha.bags))  bp$alpha.bags <- all.alpha.bags
  else bp$alpha.bags <- append(bp$alpha.bags, all.alpha.bags)
  if (is.null(bp$alpha.bag.aes))  bp$alpha.bag.aes <- list(col=control.output$col,
                                                           lty=control.output$lty,
                                                           lwd=control.output$lwd)
  else { bp$alpha.bag.aes$col <- c(bp$alpha.bag.aes$col, control.output$col)
  bp$alpha.bag.aes$lty <- c(bp$alpha.bag.aes$lty, control.output$lty)
  bp$alpha.bag.aes$lwd <- c(bp$alpha.bag.aes$lwd, control.output$lwd)
  }
  bp
}

#' Calculate alpha bags
#'
#' @param x,y,aa,na.rm,approx.limit,precision see arguments of function `compute.bagplot` in the R package `aplpack`
#' @return a list with an xy component
#' @importFrom grDevices chull
#' @noRd
#'
calc.alpha.bags <- function (x, y, aa=0.95, na.rm = TRUE, approx.limit = 2500, precision = 1)
{
  # based on the function compute.bagplot
  # from the R package aplpack
  # Wolf H (2019). _aplpack: Another Plot Package (version 190512)_. <URL: https://cran.r-project.org/package=aplpack>.

  win <- function(dx, dy) { atan2(y = dy, x = dx) }
  out.of.polygon <- function(xy, pg) {
    xy <- matrix(xy, ncol = 2)
    if (nrow(pg) == 1) return(xy[, 1] == pg[1] & xy[, 2] == pg[2])
    m <- nrow(xy)
    n <- nrow(pg)
    limit <- -abs(1e-10 * diff(range(pg)))
    pgn <- cbind(diff(c(pg[, 2], pg[1, 2])), -diff(c(pg[,1], pg[1, 1])))
    S <- colMeans(xy)
    dxy <- cbind(S[1] - pg[, 1], S[2] - pg[, 2])
    if (!all(limit < apply(dxy * pgn, 1, sum))) {
      pg <- pg[n:1, ]
      pgn <- -pgn[n:1, ]
    }
    in.pg <- rep(TRUE, m)
    for (j in 1:n) {
      dxy <- xy - matrix(pg[j, ], m, 2, byrow = TRUE)
      in.pg <- in.pg & limit < (dxy %*% pgn[j, ])
    }
    return(!in.pg)
  }
  cut.z.pg <- function(zx, zy, p1x, p1y, p2x, p2y) {
    a2 <- (p2y - p1y)/(p2x - p1x)
    a1 <- zy/zx
    sx <- (p1y - a2 * p1x)/(a1 - a2)
    sy <- a1 * sx
    sxy <- cbind(sx, sy)
    h <- any(is.nan(sxy)) || any(is.na(sxy)) || any(Inf == abs(sxy))
    if (h) {
      h <- 0 == zx
      sx <- ifelse(h, zx, sx)
      sy <- ifelse(h, p1y - a2 * p1x, sy)
      a1 <- ifelse(abs(a1) == Inf, sign(a1) * 123456789 * 1e+10, a1)
      a2 <- ifelse(abs(a2) == Inf, sign(a2) * 123456789 * 1e+10, a2)
      h <- 0 == (a1 - a2) & sign(zx) == sign(p1x)
      sx <- ifelse(h, p1x, sx)
      sy <- ifelse(h, p1y, sy)
      h <- 0 == (a1 - a2) & sign(zx) != sign(p1x)
      sx <- ifelse(h, p2x, sx)
      sy <- ifelse(h, p2y, sy)
      h <- p1x == p2x & zx != p1x & p1x != 0
      sx <- ifelse(h, p1x, sx)
      sy <- ifelse(h, zy * p1x/zx, sy)
      h <- p1x == p2x & zx != p1x & p1x == 0
      sx <- ifelse(h, p1x, sx)
      sy <- ifelse(h, 0, sy)
      h <- p1x == p2x & zx == p1x & p1x != 0
      sx <- ifelse(h, zx, sx)
      sy <- ifelse(h, zy, sy)
      h <- p1x == p2x & zx == p1x & p1x == 0 & sign(zy) == sign(p1y)
      sx <- ifelse(h, p1x, sx)
      sy <- ifelse(h, p1y, sy)
      h <- p1x == p2x & zx == p1x & p1x == 0 & sign(zy) != sign(p1y)
      sx <- ifelse(h, p1x, sx)
      sy <- ifelse(h, p2y, sy)
      h <- zx == p1x & zy == p1y
      sx <- ifelse(h, p1x, sx)
      sy <- ifelse(h, p1y, sy)
      h <- zx == p2x & zy == p2y
      sx <- ifelse(h, p2x, sx)
      sy <- ifelse(h, p2y, sy)
      h <- zx == 0 & zy == 0
      sx <- ifelse(h, 0, sx)
      sy <- ifelse(h, 0, sy)
      sxy <- cbind(sx, sy)
    }
    return(sxy)
  }
  find.cut.z.pg <- function(z, pg, center = c(0, 0)) {
    if (!is.matrix(z)) z <- rbind(z)
    if (1 == nrow(pg)) return(matrix(center, nrow(z), 2, TRUE))
    n.pg <- nrow(pg)
    n.z <- nrow(z)
    z <- cbind(z[, 1] - center[1], z[, 2] - center[2])
    pgo <- pg
    pg <- cbind(pg[, 1] - center[1], pg[, 2] - center[2])
    apg <- win(pg[, 1], pg[, 2])
    apg[is.nan(apg)] <- 0
    a <- order(apg)
    apg <- apg[a]
    pg <- pg[a, ]
    az <- win(z[, 1], z[, 2])
    segm.no <- apply((outer(apg, az, "<")), 2, sum)
    segm.no <- ifelse(segm.no == 0, n.pg, segm.no)
    next.no <- 1 + (segm.no%%length(apg))
    cuts <- cut.z.pg(z[, 1], z[, 2], pg[segm.no, 1], pg[segm.no,2], pg[next.no, 1], pg[next.no, 2])
    cuts <- cbind(cuts[, 1] + center[1], cuts[, 2] + center[2])
    return(cuts)
  }
  hdepth.of.points <- function(tp) {
    n.tp <- nrow(tp)
    tphdepth <- rep(0, n.tp)
    dpi <- 2 * pi - 1e-06
    for (j in 1:n.tp) {
      dx <- tp[j, 1] - xy[, 1]
      dy <- tp[j, 2] - xy[, 2]
      a <- win(dx, dy) + pi
      h <- a < 10
      a <- a[h]
      ident <- sum(!h)
      init <- sum(a < pi)
      a.shift <- (a + pi)%%dpi
      minusplus <- c(rep(-1, length(a)), rep(1, length(a)))
      h <- cumsum(minusplus[order(c(a, a.shift))])
      tphdepth[j] <- init + min(h) + 1
    }
    tphdepth
  }
  find.hdepths.tp <- function (tp, data, number.of.directions = 181)
  {
    xy <- as.matrix(data)
    tp <- as.matrix(rbind(tp))
    n.tp <- dim(tp)[1]
    for (j in 1:2) {
      xy[, j] <- xy[, j] - (h <- min(xy[, j], na.rm = TRUE))
      tp[, j] <- tp[, j] - h
      if (0 < (h <- max(xy[, j], na.rm = TRUE))) {
        xy[, j] <- xy[, j]/h
        tp[, j] <- tp[, j]/h
      }
    }
    phi <- c(seq(0, 180, length = number.of.directions)[-1] * (2 * pi/360))
    sinphi <- c(sin(phi), 1)
    cosphi <- c(cos(phi), 0)
    RM1 <- round(digits = 6, rbind(cosphi, sinphi))
    hdtp <- rep(length(xy[, 1]), length(tp[, 1]))
    for (j in seq(along = sinphi)) {
      xyt <- xy %*% RM1[, j]
      tpt <- (tp %*% RM1[, j])[]
      xyt <- xyt[!is.na(xyt)]
      hdtp <- pmin(hdtp, (rank(c(tpt, xyt), ties.method = "min"))[1:n.tp] - rank(tpt, ties.method = "min"),
                   rank(-c(tpt, xyt), ties.method = "min")[1:n.tp] - rank(-tpt, ties.method = "min"))
    }
    hdtp
  }
  expand.hull <- function(pg, k) {
    if (1 >= nrow(pg)) return(pg)
    resolution <- floor(20 * precision)
    pg0 <- xy[hdepth == 1, ]
    pg0 <- pg0[chull(pg0[, 1], pg0[, 2]), ]
    end.points <- find.cut.z.pg(pg, pg0, center = center)
    lam <- ((0:resolution)^1)/resolution^1
    pg.new <- pg
    for (i in 1:nrow(pg)) {
      tp <- cbind(pg[i, 1] + lam * (end.points[i, 1] - pg[i, 1]), pg[i, 2] + lam * (end.points[i, 2] - pg[i, 2]))
      hd.tp <- find.hdepths.tp(tp, xy)
      ind <- max(sum(hd.tp >= k), 1)
      if (ind < length(hd.tp)) {
        tp <- cbind(tp[ind, 1] + lam * (tp[ind + 1, 1] - tp[ind, 1]), tp[ind, 2] + lam * (tp[ind + 1, 2] - tp[ind, 2]))
        hp.tp <- find.hdepths.tp(tp, xy)
        ind <- max(sum(hd.tp >= k), 1)
      }
      pg.new[i, ] <- tp[ind, ]
    }
    pg.new <- pg.new[chull(pg.new[, 1], pg.new[, 2]), ]
    pg.add <- 0.5 * (pg.new + rbind(pg.new[-1, ], pg.new[1, ]))
    end.points <- find.cut.z.pg(pg.add, pg0, center = center)
    for (i in 1:nrow(pg.add)) {
      tp <- cbind(pg.add[i, 1] + lam * (end.points[i, 1] - pg.add[i, 1]), pg.add[i, 2] + lam * (end.points[i, 2] - pg.add[i, 2]))
      hd.tp <- find.hdepths.tp(tp, xy)
      ind <- max(sum(hd.tp >= k), 1)
      if (ind < length(hd.tp)) {
        tp <- cbind(tp[ind, 1] + lam * (tp[ind + 1, 1] - tp[ind, 1]), tp[ind, 2] + lam * (tp[ind + 1, 2] - tp[ind, 2]))
        hd.tp <- find.hdepths.tp(tp, xy)
        ind <- max(sum(hd.tp >= k), 1)
      }
      pg.add[i, ] <- tp[ind, ]
    }
    pg.new <- rbind(pg.new, pg.add)
    pg.new <- pg.new[chull(pg.new[, 1], pg.new[, 2]), ]
  }
  cut.p.sl.p.sl <- function(xy1, m1, xy2, m2) {
    sx <- (xy2[2] - m2 * xy2[1] - xy1[2] + m1 * xy1[1])/(m1 - m2)
    sy <- xy1[2] - m1 * xy1[1] + m1 * sx
    if (!is.nan(sy)) return(c(sx, sy))
    if (abs(m1) == Inf) return(c(xy1[1], xy2[2] + m2 * (xy1[1] - xy2[1])))
    if (abs(m2) == Inf) return(c(xy2[1], xy1[2] + m1 * (xy2[1] - xy1[1])))
  }
  pos.to.pg <- function(z, pg, reverse = FALSE) {
    if (reverse) {
      int.no <- apply(outer(pg[, 1], z[, 1], ">="), 2, sum)
      zy.on.pg <- pg[int.no, 2] + pg[int.no, 3] * (z[, 1] - pg[int.no, 1])
    }
    else {
      int.no <- apply(outer(pg[, 1], z[, 1], "<="), 2, sum)
      zy.on.pg <- pg[int.no, 2] + pg[int.no, 3] * (z[, 1] - pg[int.no, 1])
    }
    result <- ifelse(z[, 2] < zy.on.pg, "lower", "higher")
    return(result)
    if (all(result == "lower")) {
      result <- ifelse(((z[, 2] - zy.on.pg)/max(z[, 2] - zy.on.pg) + 1e-10) < 0, "lower", "higher")
    }
    if (all(result == "higher")) {
      result <- ifelse(((z[, 2] - zy.on.pg)/max(z[, 2] - zy.on.pg) - 1e-10) < 0, "lower", "higher")
    }
    print(result)
    return(result)
  }
  find.polygon.center <- function(xy) {
    if (length(xy) == 2) return(xy[1:2])
    if (nrow(xy) == 2) return(colMeans(xy))
    n <- length(xy[, 1])
    mxy <- colMeans(xy)
    xy2 <- rbind(xy[-1, ], xy[1, ])
    xy3 <- cbind(rep(mxy[1], n), mxy[2])
    S <- (xy + xy2 + xy3)/3
    F2 <- abs((xy[, 1] - xy3[, 1]) * (xy2[, 2] - xy3[, 2]) - (xy[, 2] - xy3[, 2]) * (xy2[, 1] - xy3[, 1]))
    lambda <- F2/sum(F2)
    SP <- colSums(cbind(S[, 1] * lambda, S[, 2] * lambda))
    return(SP)
  }
  xydata <- if (missing(y)) x
  else cbind(x, y)
  if (is.data.frame(xydata)) xydata <- as.matrix(xydata)
  if (any(is.na(xydata))) {
    if (na.rm) {
      xydata <- xydata[!apply(is.na(xydata), 1, any), , drop = FALSE]
      print("Warning: NA elements have been removed!!")
    }
    else {
      xy.medians <- apply(xydata, 2, function(x) stats::median(x, na.rm = TRUE))
      for (j in 1:ncol(xydata)) xydata[is.na(xydata[, j]), j] <- xy.medians[j]
      print("Warning: NA elements have been exchanged by median values!!")
    }
  }
  if (length(xydata) < 4) {
    print("not enough data points")
    return()
  }
  if ((length(xydata)%%2) == 1) {
    print("number of values isn't even")
    return()
  }
  if (!is.matrix(xydata))
    xydata <- matrix(xydata, ncol = 2, byrow = TRUE)
  very.large.data.set <- nrow(xydata) > approx.limit
  if (very.large.data.set) {
    step <- (n <- nrow(xydata))/approx.limit
    ind <- round(seq(1, n, by = step))
    xy <- xydata[ind, ]
  }
  else xy <- xydata
  n <- nrow(xy)
  points.in.bag <- floor(n*aa)
  prdata <- stats::prcomp(xydata)
  is.one.dim <- (0 == max(prdata[[1]])) || (min(prdata[[1]])/max(prdata[[1]])) < 1e-05
  if (is.one.dim) {
    center <- colMeans(xydata)
    res <- list(xy = xy, xydata = xydata, prdata = prdata, is.one.dim = is.one.dim, center = center)
    class(res) <- "bagplot"
    return(res)
  }
  if (nrow(xydata) <= 4) {
    center <- colMeans(xydata)
    res <- list(xy = xy, xydata = xydata, prdata = prdata, hdepths = rep(1, n), hdepth = rep(1, n), is.one.dim = is.one.dim,
                center = center, hull.center = NULL, hull.bag = NULL, hull.loop = NULL, pxy.bag = NULL, pxy.outer = xydata,
                pxy.outlier = NULL, exp.dk = xydata)
    class(res) <- "bagplot"
    return(res)
  }
  xym <- apply(xy, 2, mean)
  xysd <- apply(xy, 2, stats::sd)
  xyxy <- cbind((xy[, 1] - xym[1])/xysd[1], (xy[, 2] - xym[2])/xysd[2])
  dx <- (outer(xy[, 1], xy[, 1], "-"))
  dy <- (outer(xy[, 2], xy[, 2], "-"))
  alpha <- atan2(y = dy, x = dx)
  diag(alpha) <- 1000
  for (j in 1:n) alpha[, j] <- sort(alpha[, j])
  alpha <- alpha[-n, ]
  m <- n - 1
  hdepth <- rep(0, n)
  dpi <- 2 * pi - 1e-06
  mypi <- pi - 1e-06
  minusplus <- c(rep(-1, m), rep(1, m))
  if (FALSE) {
    for (j in 1:n) {
      a <- alpha[, j] + pi
      h <- a < 10
      a <- a[h]
      init <- sum(a < mypi)
      a.shift <- (a + pi)%%dpi
      minusplus <- c(rep(-1, length(a)), rep(1, length(a)))
      h <- cumsum(minusplus[order(c(a, a.shift))])
      hdepth[j] <- init + min(h) + 1
    }
  }
  find.hdepths <- function(xy, number.of.directions = 181) {
    xy <- as.matrix(xy)
    for (j in 1:2) {
      xy[, j] <- xy[, j] - min(xy[, j])
      if (0 < (h <- max(xy[, j]))) xy[, j] <- xy[, j]/max(xy[, j])
    }
    phi <- c(seq(0, 180, length = number.of.directions)[-1] * (2 * pi/360))
    sinphi <- c(sin(phi), 1)
    cosphi <- c(cos(phi), 0)
    RM1 <- round(digits = 6, rbind(cosphi, sinphi))
    hd <- rep(h <- length(xy[, 1]), h)
    for (j in seq(along = sinphi)) {
      xyt <- xy %*% RM1[, j]
      hd <- pmin(hd, rank(xyt, ties.method = "min"), rank(-xyt, ties.method = "min"))
    }
    hd
  }
  hdepth <- find.hdepths(xy, 181 * precision)
  hd.table <- table(sort(hdepth))
  d.k <- cbind(dk = rev(cumsum(rev(hd.table))), k = as.numeric(names(hd.table)))
  k.1 <- sum(points.in.bag < d.k[, 1])
  k <- d.k[k.1, 2] + 1
  center <- apply(xy[which(hdepth == max(hdepth)), , drop = FALSE], 2, mean)
  hull.center <- NULL
  if (3 < nrow(xy) && length(hd.table) > 0) {
    n.p <- floor(1.5 * c(32, 16, 8)[1 + (n > 50) + (n > 200)] * precision)
    h <- unique(sort(hdepth, decreasing = TRUE))
    limit.hdepth.to.check <- sort(h)[min(length(h), 3)]
    h <- cands <- xy[limit.hdepth.to.check <= hdepth, , drop = FALSE]
    cands <- cands[chull(cands[, 1], cands[, 2]), ]
    n.c <- nrow(cands)
    if (is.null(n.c)) cands <- h
    xyextr <- rbind(apply(cands, 2, min), apply(cands, 2, max))
    if ((xyextr[2, 1] - xyextr[1, 1]) < 0.2 * (h <- diff(range(xy[, 1])))) {
      xyextr[1:2, 1] <- mean(xyextr[, 1]) + c(-0.1, 0.1) * h
    }
    if ((xyextr[2, 2] - xyextr[1, 2]) < 0.2 * (h <- diff(range(xy[, 2])))) {
      xyextr[1:2, 2] <- mean(xyextr[, 2]) + c(-0.1, 0.1) * h
    }
    h1 <- seq(xyextr[1, 1], xyextr[2, 1], length = n.p)
    h2 <- seq(xyextr[1, 2], xyextr[2, 2], length = n.p)
    tp <- cbind(as.vector(matrix(h1, n.p, n.p)), as.vector(matrix(h2, n.p, n.p, TRUE)))
    tphdepth <- max(find.hdepths.tp(tp, xy))
    tphdepth <- max(tphdepth, d.k[, 2])
    num <- floor(2 * c(417, 351, 171, 85, 67, 43)[sum(n > c(1, 50, 100, 150, 200, 250))] * precision)
    num.h <- floor(num/2)
    angles <- seq(0, pi, length = num.h)
    ang <- tan(pi/2 - angles)
    kkk <- tphdepth
    ia <- 1
    a <- angles[ia]
    xyt <- xyxy %*% c(cos(a), -sin(a))
    xyto <- order(xyt)
    ind.k <- xyto[kkk]
    cutp <- c(xyxy[ind.k, 1], -10)
    dxy <- diff(range(xyxy))
    pg <- rbind(c(cutp[1], -dxy, Inf), c(cutp[1], dxy, NA))
    ind.kk <- xyto[n + 1 - kkk]
    cutpl <- c(xyxy[ind.kk, 1], 10)
    pgl <- rbind(c(cutpl[1], dxy, -Inf), c(cutpl[1], -dxy, NA))
    for (ia in seq(angles)[-1]) {
      a <- angles[ia]
      angtan <- ang[ia]
      xyt <- xyxy %*% c(cos(a), -sin(a))
      xyto <- order(xyt)
      ind.k <- xyto[kkk]
      ind.kk <- xyto[n + 1 - kkk]
      pnew <- xyxy[ind.k, ]
      pnewl <- xyxy[ind.kk, ]
      if (abs(angtan) > 1e+10) {
        pg.no <- sum(pg[, 1] < pnew[1])
        if (0 < pg.no) {
          cutp <- c(pnew[1], pg[pg.no, 2] + pg[pg.no, 3] * (pnew[1] - pg[pg.no, 1]))
          pg <- rbind(pg[1:pg.no, ], c(cutp, angtan), c(cutp[1] + dxy, cutp[2] + angtan * dxy, NA))
        }
        else {
          pg <- rbind(pg[1, ], c(pg[2, 1:2], NA))
        }
        pg.nol <- sum(pgl[, 1] >= pnewl[1])
        if (0 < pg.nol) {
          cutpl <- c(pnewl[1], pgl[pg.nol, 2] + pgl[pg.nol, 3] * (pnewl[1] - pgl[pg.nol, 1]))
          pgl <- rbind(pgl[1:pg.nol, ], c(cutpl, angtan), c(cutpl[1] - dxy, cutpl[2] - angtan * dxy, NA))
        }
        else {
          pgl <- rbind(pgl[1, ], c(pgl[2, 1:2], NA))
        }
      }
      else {
        pg.inter <- pg[, 2] - angtan * pg[, 1]
        pnew.inter <- pnew[2] - angtan * pnew[1]
        pg.no <- sum(pg.inter < pnew.inter)
        if (is.na(pg[pg.no, 3])) pg[pg.no, 3] <- -Inf
        cutp <- cut.p.sl.p.sl(pnew, ang[ia], pg[pg.no, 1:2], pg[pg.no, 3])
        pg <- rbind(pg[1:pg.no, ], c(cutp, angtan), c(cutp[1] + dxy, cutp[2] + angtan * dxy, NA))
        pg.interl <- pgl[, 2] - angtan * pgl[, 1]
        pnew.interl <- pnewl[2] - angtan * pnewl[1]
        pg.nol <- sum(pg.interl > pnew.interl)
        if (is.na(pgl[pg.nol, 3])) pgl[pg.nol, 3] <- Inf
        cutpl <- cut.p.sl.p.sl(pnewl, angtan, pgl[pg.nol, 1:2], pgl[pg.nol, 3])
        pgl <- rbind(pgl[1:pg.nol, ], c(cutpl, angtan), c(cutpl[1] - dxy, cutpl[2] - angtan * dxy, NA))
      }
    }
    if (2 < nrow(pg) && 2 < nrow(pgl)) {
      limit <- 1e-10
      idx <- c(TRUE, (abs(diff(pg[, 1])) > limit) | (abs(diff(pg[, 2])) > limit))
      if (any(idx == FALSE)) {
        pg <- pg[idx, ]
        pg[, 3] <- c(diff(pg[, 2])/diff(pg[, 1]), NA)
      }
      idx <- c((abs(diff(pgl[, 1])) > limit) | (abs(diff(pgl[, 2])) > limit), TRUE)
      if (any(idx == FALSE)) {
        pgl <- pgl[idx, ]
        pgl[, 3] <- c(diff(pgl[, 2])/diff(pgl[, 1]), NA)
      }
      pgl[, 2] <- pgl[, 2] - 1e-05
      pg <- pg[-nrow(pg), ][-1, , drop = FALSE]
      pgl <- pgl[-nrow(pgl), ][-1, , drop = FALSE]
      indl <- pos.to.pg(round(pgl, digits = 10), round(pg, digits = 10))
      indu <- pos.to.pg(round(pg, digits = 10), round(pgl, digits = 10), TRUE)
      sr <- sl <- NULL
      if (indu[(npg <- nrow(pg))] == "lower" & indl[1] == "higher") {
        rnuml <- which(indl == "lower")[1] - 1
        rnumu <- npg + 1 - which(rev(indu == "higher"))[1]
        if (is.na(rnuml)) rnuml <- sum(pg[rnumu, 1] < pgl[, 1])
        if (is.na(rnumu)) rnumu <- sum(pg[, 1] < pgl[rnuml, 1])
        xyl <- pgl[rnuml, ]
        xyu <- pg[rnumu, ]
        sr <- cut.p.sl.p.sl(xyl[1:2], xyl[3], xyu[1:2], xyu[3])
      }
      if (indl[(npgl <- nrow(pgl))] == "higher" & indu[1] == "lower") {
        lnuml <- npgl + 1 - which(rev(indl == "lower"))[1]
        lnumu <- which(indu == "higher")[1] - 1
        if (is.na(lnuml)) lnuml <- sum(pg[lnumu, 1] < pgl[, 1])
        if (is.na(lnumu)) lnumu <- sum(pg[, 1] < pgl[lnuml, 1])
        xyl <- pgl[lnuml, ]
        xyu <- pg[lnumu, ]
        sl <- cut.p.sl.p.sl(xyl[1:2], xyl[3], xyu[1:2], xyu[3])
      }
      pg <- rbind(pg[indu == "higher", 1:2, drop = FALSE], sr, pgl[indl == "lower", 1:2, drop = FALSE], sl)
      if (!any(is.na(pg))) pg <- pg[chull(pg[, 1], pg[, 2]), ]
    }
    else {
      if (2 < nrow(pgl)) { pg <- rbind(pg[2, 1:2], pgl[-c(1, length(pgl[, 1])), 1:2]) }
      else { pg <- rbind(pg[-c(1, length(pg[, 1])), 1:2], pgl[2, 1:2])  }
    }
    hull.center <- cbind(pg[, 1] * xysd[1] + xym[1], pg[, 2] * xysd[2] + xym[2])
    if (!any(is.na(hull.center))) center <- find.polygon.center(hull.center)
    else hull.center <- rbind(center)
  }

  num <- floor(2 * c(417, 351, 171, 85, 67, 43)[sum(n > c(1, 50, 100, 150, 200, 250))] * precision)
  num.h <- floor(num/2)
  angles <- seq(0, pi, length = num.h)
  ang <- tan(pi/2 - angles)
  kkk <- k
  if (kkk <= max(d.k[, 2])) {
    ia <- 1
    a <- angles[ia]
    xyt <- xyxy %*% c(cos(a), -sin(a))
    xyto <- order(xyt)
    ind.k <- xyto[kkk]
    cutp <- c(xyxy[ind.k, 1], -10)
    dxy <- diff(range(xyxy))
    pg <- rbind(c(cutp[1], -dxy, Inf), c(cutp[1], dxy, NA))
    ind.kk <- xyto[n + 1 - kkk]
    cutpl <- c(xyxy[ind.kk, 1], 10)
    pgl <- rbind(c(cutpl[1], dxy, -Inf), c(cutpl[1], -dxy, NA))
    for (ia in seq(angles)[-1]) {
      a <- angles[ia]
      angtan <- ang[ia]
      xyt <- xyxy %*% c(cos(a), -sin(a))
      xyto <- order(xyt)
      ind.k <- xyto[kkk]
      ind.kk <- xyto[n + 1 - kkk]
      pnew <- xyxy[ind.k, ]
      pnewl <- xyxy[ind.kk, ]
      if (abs(angtan) > 1e+10) {
        pg.no <- sum(pg[, 1] < pnew[1])
        if (0 < pg.no) { cutp <- c(pnew[1], pg[pg.no, 2] + pg[pg.no, 3] * (pnew[1] - pg[pg.no, 1]))
        pg <- rbind(pg[1:pg.no, ], c(cutp, angtan), c(cutp[1] + dxy, cutp[2] + angtan * dxy, NA))
        }
        else {
          pg <- rbind(pg[1, ], c(pg[2, 1:2], NA))
        }
        pg.nol <- sum(pgl[, 1] >= pnewl[1])
        if (0 < pg.nol) { cutpl <- c(pnewl[1], pgl[pg.nol, 2] + pgl[pg.nol, 3] * (pnewl[1] - pgl[pg.nol, 1]))
        pgl <- rbind(pgl[1:pg.nol, ], c(cutpl, angtan), c(cutpl[1] - dxy, cutpl[2] - angtan * dxy, NA))
        }
        else {
          pgl <- rbind(pgl[1, ], c(pgl[2, 1:2], NA))
        }
      }
      else {
        pg.inter <- pg[, 2] - angtan * pg[, 1]
        pnew.inter <- pnew[2] - angtan * pnew[1]
        pg.no <- sum(pg.inter < pnew.inter)
        if (is.na(pg[pg.no, 3])) pg[pg.no, 3] <- -Inf
        cutp <- cut.p.sl.p.sl(pnew, ang[ia], pg[pg.no, 1:2], pg[pg.no, 3])
        pg <- rbind(pg[1:pg.no, ], c(cutp, angtan), c(cutp[1] + dxy, cutp[2] + angtan * dxy, NA))
        pg.interl <- pgl[, 2] - angtan * pgl[, 1]
        pnew.interl <- pnewl[2] - angtan * pnewl[1]
        pg.nol <- sum(pg.interl > pnew.interl)
        if (is.na(pgl[pg.nol, 3])) pgl[pg.nol, 3] <- Inf
        cutpl <- cut.p.sl.p.sl(pnewl, angtan, pgl[pg.nol, 1:2], pgl[pg.nol, 3])
        pgl <- rbind(pgl[1:pg.nol, ], c(cutpl, angtan), c(cutpl[1] - dxy, cutpl[2] - angtan * dxy, NA))
      }
    }
    if (2 < nrow(pg) && 2 < nrow(pgl)) {
      limit <- 1e-10
      idx <- c(TRUE, (abs(diff(pg[, 1])) > limit) | (abs(diff(pg[, 2])) > limit))
      if (any(idx == FALSE)) {
        pg <- pg[idx, ]
        pg[, 3] <- c(diff(pg[, 2])/diff(pg[, 1]), NA)
      }
      idx <- c((abs(diff(pgl[, 1])) > limit) | (abs(diff(pgl[, 2])) > limit), TRUE)
      if (any(idx == FALSE)) {
        pgl <- pgl[idx, ]
        pgl[, 3] <- c(diff(pgl[, 2])/diff(pgl[, 1]), NA)
      }
      pgl[, 2] <- pgl[, 2] - 1e-05
      pg <- pg[-nrow(pg), ][-1, , drop = FALSE]
      pgl <- pgl[-nrow(pgl), ][-1, , drop = FALSE]
      indl <- pos.to.pg(round(pgl, digits = 10), round(pg, digits = 10))
      indu <- pos.to.pg(round(pg, digits = 10), round(pgl, digits = 10), TRUE)
      sr <- sl <- NULL
      if (indu[(npg <- nrow(pg))] == "lower" & indl[1] == "higher") {
        rnuml <- which(indl == "lower")[1] - 1
        rnumu <- npg + 1 - which(rev(indu == "higher"))[1]
        if (is.na(rnuml)) rnuml <- sum(pg[rnumu, 1] < pgl[, 1])
        if (is.na(rnumu)) rnumu <- sum(pg[, 1] < pgl[rnuml, 1])
        xyl <- pgl[rnuml, ]
        xyu <- pg[rnumu, ]
        sr <- cut.p.sl.p.sl(xyl[1:2], xyl[3], xyu[1:2], xyu[3])
      }
      if (indl[(npgl <- nrow(pgl))] == "higher" & indu[1] == "lower") {
        lnuml <- npgl + 1 - which(rev(indl == "lower"))[1]
        lnumu <- which(indu == "higher")[1] - 1
        if (is.na(lnuml)) lnuml <- sum(pg[lnumu, 1] < pgl[, 1])
        if (is.na(lnumu)) lnumu <- sum(pg[, 1] < pgl[lnuml, 1])
        xyl <- pgl[lnuml, ]
        xyu <- pg[lnumu, ]
        sl <- cut.p.sl.p.sl(xyl[1:2], xyl[3], xyu[1:2], xyu[3])
      }
      pg <- rbind(pg[indu == "higher", 1:2, drop = FALSE], sr, pgl[indl == "lower", 1:2, drop = FALSE], sl)
      if (!any(is.na(pg))) pg <- pg[chull(pg[, 1], pg[, 2]), ]
    }
    else {
      if (2 < nrow(pgl)) { pg <- rbind(pg[2, 1:2], pgl[-c(1, length(pgl[, 1])), 1:2])  }
      else {  pg <- rbind(pg[-c(1, length(pg[, 1])), 1:2], pgl[2, 1:2])  }
    }
    exp.dk <- cbind(pg[, 1] * xysd[1] + xym[1], pg[, 2] * xysd[2] + xym[2])
  }
  else {
    exp.dk <- NULL
  }
  if (1 < kkk) kkk <- kkk - 1
  ia <- 1
  a <- angles[ia]
  xyt <- xyxy %*% c(cos(a), -sin(a))
  xyto <- order(xyt)
  ind.k <- xyto[kkk]
  cutp <- c(xyxy[ind.k, 1], -10)
  dxy <- diff(range(xyxy))
  pg <- rbind(c(cutp[1], -dxy, Inf), c(cutp[1], dxy, NA))
  ind.kk <- xyto[n + 1 - kkk]
  cutpl <- c(xyxy[ind.kk, 1], 10)
  pgl <- rbind(c(cutpl[1], dxy, -Inf), c(cutpl[1], -dxy, NA))
  for (ia in seq(angles)[-1]) {
    a <- angles[ia]
    angtan <- ang[ia]
    xyt <- xyxy %*% c(cos(a), -sin(a))
    xyto <- order(xyt)
    ind.k <- xyto[kkk]
    ind.kk <- xyto[n + 1 - kkk]
    pnew <- xyxy[ind.k, ]
    pnewl <- xyxy[ind.kk, ]
    if (abs(angtan) > 1e+10) {
      pg.no <- sum(pg[, 1] < pnew[1])
      if (0 < pg.no) { cutp <- c(pnew[1], pg[pg.no, 2] + pg[pg.no, 3] * (pnew[1] - pg[pg.no, 1]))
      pg <- rbind(pg[1:pg.no, ], c(cutp, angtan), c(cutp[1] + dxy, cutp[2] + angtan * dxy, NA))
      }
      else {
        pg <- rbind(pg[1, ], c(pg[2, 1:2], NA))
      }
      pg.nol <- sum(pgl[, 1] >= pnewl[1])
      if (0 < pg.nol) { cutpl <- c(pnewl[1], pgl[pg.nol, 2] + pgl[pg.nol, 3] * (pnewl[1] - pgl[pg.nol, 1]))
      pgl <- rbind(pgl[1:pg.nol, ], c(cutpl, angtan), c(cutpl[1] - dxy, cutpl[2] - angtan * dxy, NA))
      }
      else {
        pgl <- rbind(pgl[1, ], c(pgl[2, 1:2], NA))
      }
    }
    else {
      pg.inter <- pg[, 2] - angtan * pg[, 1]
      pnew.inter <- pnew[2] - angtan * pnew[1]
      pg.no <- sum(pg.inter < pnew.inter)
      if (is.na(pg[pg.no, 3])) pg[pg.no, 3] <- -Inf
      cutp <- cut.p.sl.p.sl(pnew, ang[ia], pg[pg.no, 1:2], pg[pg.no, 3])
      pg <- rbind(pg[1:pg.no, ], c(cutp, angtan), c(cutp[1] + dxy, cutp[2] + angtan * dxy, NA))
      pg.interl <- pgl[, 2] - angtan * pgl[, 1]
      pnew.interl <- pnewl[2] - angtan * pnewl[1]
      pg.nol <- sum(pg.interl > pnew.interl)
      if (is.na(pgl[pg.nol, 3])) pgl[pg.nol, 3] <- Inf
      cutpl <- cut.p.sl.p.sl(pnewl, angtan, pgl[pg.nol, 1:2], pgl[pg.nol, 3])
      pgl <- rbind(pgl[1:pg.nol, ], c(cutpl, angtan), c(cutpl[1] - dxy, cutpl[2] - angtan * dxy, NA))
    }
  }
  if (2 < nrow(pg) && 2 < nrow(pgl)) {
    limit <- 1e-10
    idx <- c(TRUE, (abs(diff(pg[, 1])) > limit) | (abs(diff(pg[, 2])) > limit))
    if (any(idx == FALSE)) {
      pg <- pg[idx, ]
      pg[, 3] <- c(diff(pg[, 2])/diff(pg[, 1]), NA)
    }
    idx <- c((abs(diff(pgl[, 1])) > limit) | (abs(diff(pgl[, 2])) > limit), TRUE)
    if (any(idx == FALSE)) {
      pgl <- pgl[idx, ]
      pgl[, 3] <- c(diff(pgl[, 2])/diff(pgl[, 1]), NA)
    }
    pgl[, 2] <- pgl[, 2] - 1e-05
    pg <- pg[-nrow(pg), ][-1, , drop = FALSE]
    pgl <- pgl[-nrow(pgl), ][-1, , drop = FALSE]
    indl <- pos.to.pg(round(pgl, digits = 10), round(pg, digits = 10))
    indu <- pos.to.pg(round(pg, digits = 10), round(pgl, digits = 10), TRUE)
    sr <- sl <- NULL
    if (indu[(npg <- nrow(pg))] == "lower" & indl[1] == "higher") {
      rnuml <- which(indl == "lower")[1] - 1
      rnumu <- npg + 1 - which(rev(indu == "higher"))[1]
      if (is.na(rnuml)) rnuml <- sum(pg[rnumu, 1] < pgl[, 1])
      if (is.na(rnumu)) rnumu <- sum(pg[, 1] < pgl[rnuml, 1])
      xyl <- pgl[rnuml, ]
      xyu <- pg[rnumu, ]
      sr <- cut.p.sl.p.sl(xyl[1:2], xyl[3], xyu[1:2], xyu[3])
    }
    if (indl[(npgl <- nrow(pgl))] == "higher" & indu[1] == "lower") {
      lnuml <- npgl + 1 - which(rev(indl == "lower"))[1]
      lnumu <- which(indu == "higher")[1] - 1
      if (is.na(lnuml)) lnuml <- sum(pg[lnumu, 1] < pgl[, 1])
      if (is.na(lnumu)) lnumu <- sum(pg[, 1] < pgl[lnuml, 1])
      xyl <- pgl[lnuml, ]
      xyu <- pg[lnumu, ]
      sl <- cut.p.sl.p.sl(xyl[1:2], xyl[3], xyu[1:2], xyu[3])
    }
    pg <- rbind(pg[indu == "higher", 1:2, drop = FALSE], sr, pgl[indl == "lower", 1:2, drop = FALSE], sl)
    if (!any(is.na(pg))) pg <- pg[chull(pg[, 1], pg[, 2]), ]
  }
  else {
    if (2 < nrow(pgl)) { pg <- rbind(pg[2, 1:2], pgl[-c(1, length(pgl[, 1])), 1:2]) }
    else { pg <- rbind(pg[-c(1, length(pg[, 1])), 1:2], pgl[2, 1:2]) }
  }
  exp.dk.1 <- cbind(pg[, 1] * xysd[1] + xym[1], pg[, 2] * xysd[2] + xym[2])
  if (is.null(exp.dk)) exp.dk <- exp.dk.1

  if (nrow(d.k) == k.1 || nrow(d.k) == 1) lambda <- 0
  else {
    ind <- sum(d.k[, 2] <= k.1)
    ind <- k.1
    ndk.1 <- d.k[ind, 1]
    ndk <- d.k[ind + 1, 1]
    lambda <- (n*aa - ndk)/(ndk.1 - ndk)
  }
  cut.on.pdk.1 <- find.cut.z.pg(exp.dk, exp.dk.1, center = center)
  cut.on.pdk <- find.cut.z.pg(exp.dk.1, exp.dk, center = center)
  h1 <- (1 - lambda) * exp.dk + lambda * cut.on.pdk.1
  h2 <- (1 - lambda) * cut.on.pdk + lambda * exp.dk.1
  h <- rbind(h1, h2)
  h <- h[!is.nan(h[, 1]) & !is.nan(h[, 2]), ]
  list (xy=h[chull(h[, 1], h[, 2]), ])
}

#' Aesthetics for kappa ellipses
#'
#' @param g number of groups
#' @param ellipse.names ellipse names, typically group names
#' @param df degrees of freedom
#' @param kappa value to construct k-ellipse
#' @param which which group to select for ellipse construction
#' @param alpha size of alpha bag
#' @param col colour of ellipse
#' @param lty line type of ellipse
#' @param lwd line width of ellipse
#' @param alpha.transparency opacity
#'
#' @noRd
#'
control.concentration.ellipse <- function (g, g.names, df, kappa, which,
                                           col, lty, lwd, alpha.transparency)
{
  if (!all(is.numeric(which))) which <- match(which, g.names, nomatch = 0)
  which <- which[which <= g]
  which <- which[which > 0]
  rep.num <- length(which)
  rep.kappa <- length(kappa)

  if (length(kappa)>1)
  {
    temp.mat <- expand.grid(which, kappa)
    which <- temp.mat[,1]
    kappa <- temp.mat[,2]
  }
  ellipse.num <- length(which)
  while (length(kappa) < ellipse.num) kappa <- c(kappa, kappa)
  kappa <- as.vector(kappa[1:ellipse.num])
  while (length(col) < ellipse.num) col <- c(col, col)
  col <- as.vector(rep(col[1:rep.num],rep.kappa))
  while (length(lty) < ellipse.num) lty <- c(lty, lty)
  lty <- as.vector(rep(lty[1:rep.kappa],each=rep.num))
  while (length(lwd) < ellipse.num) lwd <- c(lwd, lwd)
  #lwd <- as.vector(lwd[1:ellipse.num])
  lwd <- as.vector(rep(lwd[1:rep.kappa],each=rep.num))
  while (length(alpha.transparency) < ellipse.num) alpha.transparency <- c(alpha.transparency, alpha.transparency)
  alpha.transparency <- as.vector(rep(alpha.transparency[1:rep.kappa],each=rep.num))

  list(which = which, kappa = kappa, col = col, lty = lty, lwd = lwd, alpha.transparency = alpha.transparency)
}

#' Concentration ellipses (\eqn{\kappa}-ellipses)
#'
#' @description
#' This function produces \eqn{\kappa}-ellipses, which is a useful geometrical description of the
#' data points about the sample mean.
#'
#' @param bp An object of class \code{biplot}.
#' @param df Degrees of freedom, with default \code{2}.
#' @param kappa Value to construct \eqn{\kappa}-ellipse (the value of \eqn{\kappa}).
#' @param which The selection of the group for ellipse construction.
#' @param alpha Size of \eqn{\alpha}-bag, with default \code{0.95}.
#' @param col Colour of ellipse. Multiple \eqn{\kappa}-ellipse for one group will be displayed in the same colour.
#' @param lty Line type of ellipse. The same line type will be used per value of \eqn{\kappa}.
#' @param lwd Line width of ellipse. The same line width will be used per value of \eqn{\kappa}.
#' @param alpha.transparency Level of opacity, with default \code{0.5}.
#'
#' @return A list with the following components is available:
#' \item{conc.ellipses}{List of coordinates for the \eqn{\kappa}-ellipses for each group.}
#' \item{col}{Vector of colours for the \eqn{\kappa}-ellipses.}
#' \item{lty}{Vector of line types for the \eqn{\kappa}-ellipses.}
#' \item{lwd}{Vector of line widths for the \eqn{\kappa}-ellipses.}
#' \item{alpha}{Vector of \eqn{\alpha} values.}
#'
#' @references
#' Gower, J., Gardner-Lubbe, S. & Le Roux, N. 2011. <em>Understanding Biplots.<em> Chichester, England: John Wiley & Sons Ltd.<br><br>
#' @export
#' @usage concentration.ellipse(bp, df=2, kappa = NULL, which = NULL,
#' alpha = 0.95, col = ez.col, lty = 1, lwd = 1, alpha.transparency = 0.5)
#'
#' @examples
#' biplot (iris[,1:4]) |> PCA(group.aes=iris[,5]) |> concentration.ellipse(kappa=2) |> plot()
#'
concentration.ellipse <- function(bp, df=2, kappa = NULL, which = NULL, alpha = 0.95,
                                  col = ez.col, lty = 1, lwd = 1, alpha.transparency = 0.5)
{

  g <- bp$g
  g.names <- bp$g.names
  if (is.null(which)) which <- 1:g
  if (any(alpha < 0 | alpha > 0.99)) stop(message = "alpha not to be negative or larger than 0.99")
  if (is.null(kappa)) kappa <- sqrt(stats::qchisq(alpha,df))
  control.output <- control.concentration.ellipse (g=g, g.names=g.names, df=df, kappa = kappa, which = which,
                                                   col = col, lty = lty, lwd = lwd,
                                                   alpha.transparency = alpha.transparency)

  all.ellipses <- list()
  for(a in 1:length(control.output$which))
  {
    cat (paste("Computing", round(control.output$kappa[a],2), "-ellipse for",g.names[control.output$which[a]], "\n"))
    Zgroup <- bp$Z[bp$group.aes==g.names[control.output$which[a]],]
    calc <- calc.concentration.ellipse(Zgroup, kappa=control.output$kappa[a])
    all.ellipses[[a]] <- calc
  }
  names(all.ellipses) <- paste (g.names[control.output$which], round(control.output$kappa,2), sep="-")

  if (is.null(bp$conc.ellipses))  bp$conc.ellipses <- all.ellipses
  else bp$conc.ellipses <- append(bp$conc.ellipses, all.ellipses)
  if (is.null(bp$conc.ellipse.aes))  bp$conc.ellipse.aes <- list(col=control.output$col,
                                                                 lty=control.output$lty,
                                                                 lwd=control.output$lwd,
                                                                 alpha=control.output$alpha.transparency)
  else { bp$conc.ellipse.aes$col <- c(bp$conc.ellipse.aes$col, control.output$col)
  bp$conc.ellipse.aes$lty <- c(bp$conc.ellipse.aes$lty, control.output$lty)
  bp$conc.ellipse.aes$lwd <- c(bp$conc.ellipse.aes$lwd, control.output$lwd)
  bp$conc.ellipse.aes$alpha <- c(bp$conc.ellipse.aes$alpha, control.output$alpha.transparency)
  }
  bp
}

#' Calculate concentration ellipses
#'
#' @param X matrix of coordinates
#' @param kappa quantile of the chi-squared distribution determining the size of the ellipse
#' @param covmat a covariance matrix to use in place of computing a covariance matrix from X
#'
#' @return matrix of coordinates
#'
#' @noRd
calc.concentration.ellipse <- function (X, kappa=2, covmat = NULL)
{
  means <- matrix(apply(X, 2, mean), nrow = 2)
  if (is.null(covmat)) covmat <- stats::cov(X)
  range.vec <- apply(X, 2, range)
  mid.vec <- apply(range.vec, 2, function(x) (x[2] + x[1])/2)
  dif <- max(range.vec[2, ] - range.vec[1, ])/2
  xlim <- c(mid.vec[1] - dif, mid.vec[1] + dif)
  ylim <- c(mid.vec[2] - dif, mid.vec[2] + dif)
  svd.covmat <- svd(covmat)
  a <- (0:6283)/1000
  Y <- cbind(cos(a), sin(a))
  Y <- Y %*% diag(sqrt(svd.covmat$d)) %*% t(svd.covmat$v) * kappa
  Y + matrix(rep(1, 6284), ncol = 1) %*% t(means)
}

#'Legend type
#'
#' @description
#' This function enables the user to format the legend and make a required selection to display.
#'
#' @param bp An object of class \code{biplot}.
#' @param samples Logical argument indicating whether legend should be printed for samples, with default \code{FALSE}.
#' @param means Logical argument indicating whether legend should be printed for means, with default \code{FLASE}.
#' @param bags Logical argument indicating whether legend should be printed for bags, with default \code{FALSE}.
#' @param ellipses Logical argument indicating whether legend should be printed for concentration ellipses, with default \code{FALSE}.
#' @param new Logical argument indicating whether the legend should appear in a new window, with default \code{FALSE}.
#' @param ... addition arguments to be sent to \code{legend()}.
#'
#' @return A list with the following components is available:
#' \item{samples}{TRUE or FALSE, as specified.}
#' \item{means}{TRUE or FALSE, as specified.}
#' \item{bags}{TRUE or FALSE, as specified.}
#' \item{ellipses}{TRUE or FALSE, as specified.}
#' \item{new}{TRUE or FALSE, as specified.}
#'
#' @export
#' @usage legend.type(bp, samples = FALSE, means = FALSE, bags = FALSE, ellipses = FALSE,
#' new = FALSE)
#'
#' @examples
#' biplot (iris[,1:4], Title="Test biplot") |> PCA(group.aes = iris[,5]) |>
#'     legend.type(samples=TRUE) |> plot()
legend.type <- function (bp, samples = FALSE, means = FALSE, bags = FALSE, ellipses=FALSE, new=FALSE, ...)
{
  bp$legend <- list(samples=samples, means=means, bags=bags,ellipses = ellipses, new=new, arglist=list(...))
  bp
}

#' Constructs the biplot legend
#'
#' @param bp An object of class \code{biplot}.
#' @param ... more arguments to be sent to `legend`.
#'
#' @noRd
biplot.legend <- function(bp, ...)
{
  legend.type <- c(bp$legend$samples, bp$legend$means, bp$legend$bags, bp$legend$ellipses)

  if (all(legend.type == FALSE)) return(cat("Change at least one of samples, means, bags or ellipses to TRUE to obtain a legend\n"))

  if(bp$legend$new)
  {
    plot(x = c(0, 10), y = c(0, 10), type = "n", axes = FALSE, xlab = "", ylab = "", xaxs = "i", yaxs = "i")
    usr <- graphics::par("usr")
    x <- usr[1]
    y <- usr[4]
  }

  if(bp$legend$bags & !is.null(bp$alpha.bags))
  { #formatting of legend names
    graphics::legend("topleft",col=bp$alpha.bag.aes$col,lty=bp$alpha.bag.aes$lty,lwd=bp$alpha.bag.aes$lwd,
                     legend=names(bp$alpha.bags), ...)
  }

  if(bp$legend$means) graphics::legend("bottomright",col=bp$means$col,pch=bp$means$pch,legend=bp$classes, ...)

  if(bp$legend$samples) graphics::legend("topright",col=bp$samples$col,pch=bp$samples$pch,legend=bp$g.names, ...)

  if(bp$legend$ellipses & !is.null(bp$conc.ellipses))
  { #formatting of legend names
    graphics::legend("bottomleft",col=bp$conc.ellipse.aes$col,lty=bp$conc.ellipse.aes$lty,lwd=bp$conc.ellipse.aes$lwd,
                     legend=names(bp$conc.ellipses), ...)
  }

}

# ----------------------------------------------------------------------------------------------
#' Generic print function of objects of class biplot
#'
#' @param x An object of class \code{biplot}.
#' @param ... additional arguments
#'
#' @export
#' @examples
#' out <- biplot (iris[,1:4]) |> PCA() |> plot()
#' out

print.biplot <- function (x, ...)
{
  cat ("Object of class biplot, based on", x$n, "samples and", ncol(x$raw.X), "variables.\n")
  if (!is.null(x$na.action))
    cat ("The following", length(x$na.action), "sample-rows where removed due to missing values\n", x$na.action, "\n")
  if (x$g>1)
    cat (x$g, "groups:", x$g.names, "\n")
}

# ----------------------------------------------------------------------------------------------
#' Generic Plotting function of objects of class biplot
#'
#' @param x An object of class \code{biplot}.
#' @param exp.factor factor to expand plotting area beyond samples
#' @param ... additional arguments
#'
#' @export
#' @examples
#' biplot (iris[,1:4]) |> PCA() |> plot()
plot.biplot <- function(x, exp.factor=1.2, ...)
{
  .samples.plot <- function(Z, group.aes, sample.aes)
  {
    x.vals <- Z[, 1]
    y.vals <- Z[, 2]
    invals <- x.vals < usr[2] & x.vals > usr[1] & y.vals < usr[4] & y.vals > usr[3]
    Z <- Z[invals, ]
    groups <- levels(group.aes)
    group.aes <- group.aes[invals]
    for (j in 1:length(groups))
    {   Z.class <- Z[group.aes==groups[j], , drop = FALSE]
    text.pos <- match(sample.aes$label.side[j], c("bottom", "left", "top", "right"))
    if (sample.aes$label[j]) graphics::text(Z.class[, 1], Z.class[, 2], labels = dimnames(Z.class)[[1]],
                                            cex = sample.aes$label.cex[j], col = sample.aes$col[j], pos = text.pos)
    graphics::points(x = Z.class[, 1], y = Z.class[, 2], pch = sample.aes$pch[j], col = sample.aes$col[j],
                     cex = sample.aes$cex[j])
    }
  }
  .calibrate.axis <- function (j, Xhat, means, sd, axes.rows, ax.which, ax.tickvec,
                               ax.orthogxvec, ax.orthogyvec)
  {

    ax.num <- ax.which[j]
    tick <- ax.tickvec[j]
    ax.direction <- axes.rows[ax.num,]
    r <- ncol(axes.rows)
    ax.orthog <- rbind(ax.orthogxvec, ax.orthogyvec)
    if (nrow(ax.orthog) < r)    ax.orthog <- rbind(ax.orthog, 0)
    if (nrow(axes.rows) > 1)    phi.vec <- diag(1 / diag(axes.rows %*% t(axes.rows))) %*% axes.rows %*% ax.orthog[, ax.num]
    else  phi.vec <- (1 / (axes.rows %*% t(axes.rows))) %*% axes.rows %*% ax.orthog[, ax.num]

    std.ax.tick.label <- pretty(range(Xhat[, ax.num]), n = tick)
    std.range <- range(std.ax.tick.label)
    std.ax.tick.label.min <-  std.ax.tick.label - (std.range[2] - std.range[1])
    std.ax.tick.label.max <-  std.ax.tick.label + (std.range[2] - std.range[1])
    std.ax.tick.label <-  c(std.ax.tick.label,  std.ax.tick.label.min, std.ax.tick.label.max)
    interval <- (std.ax.tick.label - means[ax.num]) / sd[ax.num]
    axis.vals <- sort(unique(interval))

    number.points <- length(axis.vals)
    axis.points <- matrix(0, nrow = number.points, ncol = r)
    for (i in 1:r)
      axis.points[, i] <-  ax.orthog[i, ax.num] + (axis.vals - phi.vec[ax.num]) * ax.direction[i]
    axis.points <- cbind(axis.points, axis.vals * sd[ax.num] + means[ax.num])

    #slope = delta y / delta x of two datapoints
    slope <- (axis.points[1, 2] - axis.points[2, 2]) / (axis.points[1, 1] - axis.points[2, 1])
    #if slope is infinite then all x-values are same
    v <- NULL
    if (is.na(slope)) {
      v <- axis.points[1, 1]
      slope = NULL
    }
    else if (abs(slope) == Inf) {
      v <- axis.points[1, 1]
      slope = NULL
    }

    #y=mx+c... c=y-mx
    intercept <- axis.points[1, 2] - slope * axis.points[1, 1]

    details <- list(a = intercept, b = slope, v = v)
    retvals <- list(coords = axis.points, a = intercept, b = slope, v = v)
    return(retvals)
  }
  .marker.label.cm <- function(x, y, grad, marker.val, expand = 1, col, label.on.off, side, pos, offset, label.col, cex)
  {
    uin <- graphics::par("pin")/c(usr[2] - usr[1], usr[4] - usr[3])
    mm <- 1/(uin[1] * 25.4)
    d <- expand * mm
    if (grad == "v")
    {  graphics::lines(rep(x, 2), c(y - d, y + d), col = col)
      if (label.on.off == 1) graphics::text(x, y - d, label = marker.val, pos = pos, offset = offset, col = label.col, cex = cex)
    }
    if (grad == "h")
    {  graphics::lines(c(x - d, x + d), rep(y, 2), col = col)
      if (label.on.off == 1) if (side == "right") graphics::text(x + d, y, label = marker.val, pos = pos, offset = offset, col = label.col, cex = cex)
      else graphics::text(x - d, y, label = marker.val, pos = pos, offset = offset, col = label.col, cex = cex)
    }
    if (is.numeric(grad))
    {  b <- d * sqrt(1/(1 + grad * grad))
    a <- b * grad
    graphics::lines(c(x - b, x + b), c(y - a, y + a), col = col)
    if (label.on.off == 1) if (side == "right") graphics::text(x + b, y + a, label = marker.val, pos = pos, offset = offset, col = label.col, cex = cex)
    else graphics::text(x - b, y - a, label = marker.val, pos = pos, offset = offset, col = label.col, cex = cex)
    }
  }
  .marker.func <- function(vec, coef, col, tick.size, side, pos, offset, label.col, cex)
  {
    x <- vec[1]
    y <- vec[2]
    marker.val <- vec[3]
    label.on.off <- vec[4]
    if (is.na(coef[2]))
      .marker.label.cm(x, y, grad = "h", marker.val, expand = tick.size, col = col, label.on.off = label.on.off, side = side, pos = pos, offset = offset, label.col = label.col, cex = cex)
    else if (coef[2] == 0)
      .marker.label.cm(x, y, grad = "v", marker.val, expand = tick.size, col = col, label.on.off = label.on.off, side = side, pos = pos, offset = offset, label.col = label.col, cex = cex)
    else
      .marker.label.cm(x, y, grad = -1/coef[2], marker.val, expand = tick.size, col = col, label.on.off = label.on.off, side = side, pos = pos, offset = offset, label.col = label.col, cex = cex)
  }

  .lin.axes.plot <- function(z.axes, ax.aes, predict.mat)
  {
    for (i in 1:length(ax.aes$which))
    {  ax.num <- ax.aes$which[i]
    this.axis<-z.axes[[i]]
    marker.mat <- this.axis$coords
    marker.mat <- marker.mat[rev(order(marker.mat[, 3])), ]
    x.vals <- marker.mat[, 1]
    y.vals <- marker.mat[, 2]

    lin.coef<-c(a=this.axis$a,b=this.axis$b)
    if (is.null(this.axis$b))
      graphics::abline(v = this.axis$v, col = ax.aes$col[i], lwd = ax.aes$lwd[i], lty = ax.aes$lty[i])
    else
      graphics::abline(coef=lin.coef, col = ax.aes$col[i], lwd = ax.aes$lwd[i], lty = ax.aes$lty[i])

    if (ax.aes$label.dir == "Hor") {  graphics::par(las = 1)
      adjust <- c(0.5, 1, 0.5, 0)       }
    if (ax.aes$label.dir == "Orthog") { graphics::par(las = 2)
      adjust <- c(1, 1, 0, 0)         }
    if (ax.aes$label.dir == "Paral") {  graphics::par(las = 0)
      adjust <- c(0.5, 0.5, 0.5, 0.5) }

    h <- nrow(marker.mat)
    if (is.null(this.axis$b))
    { if (y.vals[1] < y.vals[h])
      graphics::mtext(text = ax.aes$names[i], side = 1, line = ax.aes$label.dist[i], adj = adjust[1], at = x.vals[1], col = ax.aes$label.col[i], cex = ax.aes$label.cex[i])
      else
        graphics::mtext(text = ax.aes$names[i], side = 3, line = ax.aes$label.dist[i], adj = adjust[3], at = y.vals[1], col = ax.aes$label.col[i], cex = ax.aes$label.cex[i])
    }
    else
    { y1.ster <- lin.coef[2] * usr[1] + lin.coef[1]
    y2.ster <- lin.coef[2] * usr[2] + lin.coef[1]
    x1.ster <- (usr[3] - lin.coef[1])/lin.coef[2]
    x2.ster <- (usr[4] - lin.coef[1])/lin.coef[2]
    if (lin.coef[2] == 0)
    { if (x.vals[1] < x.vals[h])
      graphics::mtext(text = ax.aes$names[i], side = 2, line = ax.aes$label.dist[i], adj = adjust[2], at = y.vals[1], col = ax.aes$label.col[i], cex = ax.aes$label.cex[i])
      else
        graphics::mtext(text = ax.aes$names[i], side = 4, line = ax.aes$label.dist[i], adj = adjust[4], at = y.vals[1], col = ax.aes$label.col[i], cex = ax.aes$label.cex[i])
    }
    if (lin.coef[2] > 0)
    {  if (x.vals[1] < x.vals[h])
      if (y1.ster <= usr[4] & y1.ster >= usr[3])
        graphics::mtext(text = ax.aes$names[i], side = 2, line = ax.aes$label.dist[i], adj = adjust[2], at = y1.ster, col = ax.aes$label.col[i], cex = ax.aes$label.cex[i])
      else
        graphics::mtext(text = ax.aes$names[i], side = 1, line = ax.aes$label.dist[i], adj = adjust[1], at = x1.ster, col = ax.aes$label.col[i], cex = ax.aes$label.cex[i])
      else if (y2.ster <= usr[4] & y2.ster >= usr[3])
        graphics::mtext(text = ax.aes$names[i], side = 4, line = ax.aes$label.dist[i], adj = adjust[4], at = y2.ster, col = ax.aes$label.col[i], cex = ax.aes$label.cex[i])
      else
        graphics::mtext(text = ax.aes$names[i], side = 3, line = ax.aes$label.dist[i], adj = adjust[3], at = x2.ster, col = ax.aes$label.col[i], cex = ax.aes$label.cex[i])
    }
    if (lin.coef[2] < 0)
    {  if (x.vals[1] < x.vals[h])
      if (y1.ster <= usr[4] & y1.ster >= usr[3])
        graphics::mtext(text = ax.aes$names[i], side = 2, line = ax.aes$label.dist[i], adj = adjust[2], at = y1.ster, col = ax.aes$label.col[i], cex = ax.aes$label.cex[i])
      else
        graphics::mtext(text = ax.aes$names[i], side = 3, line = ax.aes$label.dist[i], adj = adjust[3], at = x2.ster, col = ax.aes$label.col[i], cex = ax.aes$label.cex[i])
      else if (y2.ster <= usr[4] & y2.ster >= usr[3])
        graphics::mtext(text = ax.aes$names[i], side = 4, line = ax.aes$label.dist[i], adj = adjust[4], at = y2.ster, col = ax.aes$label.col[i], cex = ax.aes$label.cex[i])
      else
        graphics::mtext(text = ax.aes$names[i], side = 1, line = ax.aes$label.dist[i], adj = adjust[1], at = x1.ster, col = ax.aes$label.col[i], cex = ax.aes$label.cex[i])
    }
    }

    invals <- x.vals < usr[2] & x.vals > usr[1] & y.vals < usr[4] & y.vals > usr[3]
    std.markers <- marker.mat[invals, 3]
    if (is.numeric(std.markers)) std.markers <- zapsmall(std.markers)
    x.vals <- x.vals[invals]
    y.vals <- y.vals[invals]
    if (ax.aes$tick.label[i]) label.on.off <- rep(1, sum(invals))  else label.on.off <- rep(0, sum(invals))
    # if (!ax.aes$tick.label[i]) label.on.off[c(1, length(label.on.off))] <- 1
    if(sum(invals)>0) apply(data.frame(x.vals, y.vals, std.markers, label.on.off), 1, .marker.func,
                            coef = lin.coef, col = ax.aes$tick.col[i], tick.size = ax.aes$tick.size[i],
                            side = ax.aes$tick.label.side[i], pos = ax.aes$tick.label.pos[i],
                            offset = ax.aes$tick.label.offset[i], label.col = ax.aes$tick.label.col[i],
                            cex = ax.aes$tick.label.cex[i])
    #  if (!is.null(predict.mat)) apply(cbind(predict.mat, y.vals[1]), 1, .predict.func, coef = lin.coef, col = ax.aes$predict.col[i], lty = ax.aes$predict.lty[i], lwd = ax.aes$predict.lwd[i])
    }
  }


  .bags.plot <- function(z.bags, bag.aes)
  {
    for (i in 1:length(z.bags))
      graphics::polygon (z.bags[[i]], border=bag.aes$col[i], lty=bag.aes$lty[i], lwd=bag.aes$lwd[i])
  }

  .conc.ellipse.plot <- function(z.ellipse, ellipse.aes)
  {
    for (i in 1:length(z.ellipse))
      graphics::polygon(z.ellipse[[i]], border=ellipse.aes$col[i], lty=ellipse.aes$lty[i], lwd = ellipse.aes$lwd[i])
  }

  Z <- x$Z

  old.par <- graphics::par(pty = "s", ...)
  on.exit(graphics::par(old.par))

  plot(Z[, 1] * exp.factor, Z[, 2] * exp.factor, xlim = range(Z[, 1] * exp.factor), ylim = range(Z[, 2] * exp.factor),
       xaxt = "n", yaxt = "n", xlab = "", ylab = "", type = "n", xaxs = "i", yaxs = "i", asp = 1)
  usr <- graphics::par("usr")

  if (is.null(x$axes)) x <- axes(x)
  ax.aes <- x$axes

  if (length(ax.aes$which) > 0)
  {
    z.axes <- lapply(1:length(ax.aes$which), .calibrate.axis, x$Xhat, x$means, x$sd, x$ax.one.unit, ax.aes$which,
                     ax.aes$ticks, ax.aes$orthogx, ax.aes$orthogy)
    .lin.axes.plot(z.axes, ax.aes, predict.mat=NULL)

  }

  if (is.null(x$samples)) x <- samples(x)
  .samples.plot(Z, x$group.aes, x$samples)

  if (!is.null(x$alpha.bags)) .bags.plot (x$alpha.bags, x$alpha.bag.aes)

  if (!is.null(x$conc.ellipses)) .conc.ellipse.plot (x$conc.ellipses, x$conc.ellipse.aes)

  if (!is.null(x$Title)) graphics::title(main=x$Title)

  if (!is.null(x$legend)) do.call(biplot.legend, list(bp=x, x$legend$arglist))

  invisible(x)
}

