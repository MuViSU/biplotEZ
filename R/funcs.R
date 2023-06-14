### MuViSU (Centre for Multi-Dimensional Data Visualisation
### biplotEZ functions
### ========================================================

#' Create a new biplot
#'
#' @description
#'  This function produces a list of elements to be used when producing a biplot
#'
#' @param data A dataframe or matrix containing all variables the user wants to analyse
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
#' biplot(data = iris)
biplot <- function(data, group.aes = NULL, center = TRUE, scaled = FALSE, Title = NULL)
{
  dim.mat<-dim(data)
  if(is.null(dim.mat)) stop("Not enough variables to construct a biplot \n Consider using data with more columns")
  if(ncol(data)<2) stop("Not enough variables to construct a biplot \n Consider using data with more columns")

  # check for missing values
  na.vec.df<-na.action(na.omit(data))
  if (length(na.vec.df) == nrow(data)) stop("No observations left after deleting missing observations")
  else if (!is.null(na.vec.df))  warning(paste(length(na.vec.df), "rows deleted due to missing values"))
  data<-data[complete.cases(data),]

  # Separating numeric and categorical data
  type.vec <- unlist(lapply(data, is.numeric), use.names = FALSE)
  if (sum(type.vec)>0) X <- as.matrix(data[, type.vec, drop=F])
  else X <- NULL
  if (sum(type.vec)<length(type.vec)) Xcat <- as.data.frame(data[, !type.vec, drop=F])
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
    ax.one.unit <- (sqrt(n - 1)/(diag(Vr %*% lambda.r %*% t(Vr)))) * Vr %*% sqrt(lambda.r)
  else
    ax.one.unit <- 1/(diag(Vr %*% t(Vr))) * Vr

  bp$Z <- Z
  bp$Vr <- Vr
  bp$Xhat <- Z %*% t(bp$Vr)
  bp$ax.one.unit <- ax.one.unit
  if (bp$scaled) bp$Xhat <- scale(bp$Xhat, center=F, scale=1/bp$sd)
  if (bp$center) bp$Xhat <- scale(bp$Xhat, center=-1*bp$means, scale=F)
  class(bp)<-append(class(bp),"PCA")
  bp
}
# ---------------------------------------------------------------------------------------------

ez.col <- c("blue","green","gold","cyan","magenta","black","red","grey","purple","salmon")

# ----------------------------------------------------------------------------------------------
#' Formatting of samples in biplot
#'
#' @description
#' Formatting of samples in biplot
#'
#' @param bp biplot object
#' @param col sample colour
#' @param pch sample plotting character
#' @param cex sample character expansion
#' @param label logical, whether samples should be labelled or not
#' @param label.cex label text expansion
#' @param label.side side of the plotting character where label appears
#' @param connected logical, whether samples are connected in order of rows of data matrix
#' @param alpha opacity
#'
#' @return A list with the following components
#' {
#' \item{col}{Colour of the sample}
#' \item{pch}{Plotting character of the sample}
#' \item{cex}{Expansion of the plotting character}
#' \item{label}{Logical parameter whether labels appear}
#' \item{label.cex}{}
#' \item{label.side}{}
#' \item{connected}{}
#' \item{alpha}{}
#' \item{g}{number of groups}
#' }
#' @export
#'
#' @examples biplot(iris[,1:4]) |> PCA() |> samples(col="purple",pch=15) |> plot()
samples <- function (bp,  col = ez.col,
                     pch = 3, cex = 1, label = F, label.cex = 0.75, label.side = "bottom",
                     connected=F, alpha = 1)
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

axes <- function (bp, X.names=colnames(bp$X), which = 1:bp$p, col = grey(0.7), lwd = 1, lty = 1,
                label.dir = "Orthog", label.col = col, label.cex = 0.75, label.dist = 0, ticks = 5,
                tick.col = col, tick.size = 1, tick.label = T, tick.label.col = tick.col, tick.label.cex = 0.6,
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
plot.biplot <- function(x, exp.factor=1.2, ...)
{
.samples.plot <- function(Z, group.aes, sample.style)
{
    x.vals <- Z[, 1]
    y.vals <- Z[, 2]
    invals <- x.vals < usr[2] & x.vals > usr[1] & y.vals < usr[4] & y.vals > usr[3]
    Z <- Z[invals, ]
    groups <- levels(group.aes)
    group.aes <- group.aes[invals]
    for (j in 1:length(groups))
      {   Z.class <- Z[group.aes==groups[j], , drop = FALSE]
          text.pos <- match(sample.style$label.side[j], c("bottom", "left", "top", "right"))
          if (sample.style$label[j]) text(Z.class[, 1], Z.class[, 2], labels = dimnames(Z.class)[[1]],
                                          cex = sample.style$label.cex[j], col = sample.style$col[j], pos = text.pos)
          points(x = Z.class[, 1], y = Z.class[, 2], pch = sample.style$pch[j], col = sample.style$col[j],
                  cex = sample.style$cex[j])
      }
}
#' Title
#'
#' @param j Position in ax.which
#' @param Xhat Predicted values
#' @param means colmeans
#' @param sd col sd
#' @param axes.rows Vr matrix... px2
#' @param ax.which COlumn numbers of raw.X- vector fixed
#' @param ax.tickvec Vector of number of tickmarks
#' @param ax.orthogxvec orthogonal parallel translation
#' @param ax.orthogyvec orthogonal parallel translation
#' @param ax.oblique Axis rotation
#'
#' @return A list. First element the plotting coors of tickmarks. Second
#'        Intercept. Third slope (NULL if infinite). Third v (null if slope<inf)
#' @export
#' @examples
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
  uin <- par("pin")/c(usr[2] - usr[1], usr[4] - usr[3])
  mm <- 1/(uin[1] * 25.4)
  d <- expand * mm
  if (grad == "v")
  {  lines(rep(x, 2), c(y - d, y + d), col = col)
    if (label.on.off == 1) text(x, y - d, label = marker.val, pos = pos, offset = offset, col = label.col, cex = cex)
  }
  if (grad == "h")
  {  lines(c(x - d, x + d), rep(y, 2), col = col)
    if (label.on.off == 1) if (side == "right") text(x + d, y, label = marker.val, pos = pos, offset = offset, col = label.col, cex = cex)
    else text(x - d, y, label = marker.val, pos = pos, offset = offset, col = label.col, cex = cex)
  }
  if (is.numeric(grad))
  {  b <- d * sqrt(1/(1 + grad * grad))
     a <- b * grad
     lines(c(x - b, x + b), c(y - a, y + a), col = col)
     if (label.on.off == 1) if (side == "right") text(x + b, y + a, label = marker.val, pos = pos, offset = offset, col = label.col, cex = cex)
     else text(x - b, y - a, label = marker.val, pos = pos, offset = offset, col = label.col, cex = cex)
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

.lin.axes.plot <- function(z.axes, ax.style, predict.mat)
{
  for (i in 1:length(ax.style$which))
  {  ax.num <- ax.style$which[i]
     this.axis<-z.axes[[i]]
     marker.mat <- this.axis$coords
     marker.mat <- marker.mat[rev(order(marker.mat[, 3])), ]
     x.vals <- marker.mat[, 1]
     y.vals <- marker.mat[, 2]

     lin.coef<-c(a=this.axis$a,b=this.axis$b)
     if (is.null(this.axis$b))
       abline(v = this.axis$v, col = ax.style$col[i], lwd = ax.style$lwd[i], lty = ax.style$lty[i])
     else
       abline(coef=lin.coef, col = ax.style$col[i], lwd = ax.style$lwd[i], lty = ax.style$lty[i])

     if (ax.style$label.dir == "Hor") {  par(las = 1)
                                     adjust <- c(0.5, 1, 0.5, 0)       }
     if (ax.style$label.dir == "Orthog") { par(las = 2)
                                       adjust <- c(1, 1, 0, 0)         }
     if (ax.style$label.dir == "Paral") {  par(las = 0)
                                       adjust <- c(0.5, 0.5, 0.5, 0.5) }

     h <- nrow(marker.mat)
     if (is.null(this.axis$b))
       { if (y.vals[1] < y.vals[h])
           mtext(text = ax.style$names[i], side = 1, line = ax.style$label.dist[i], adj = adjust[1], at = x.vals[1], col = ax.style$label.col[i], cex = ax.style$label.cex[i])
         else
           mtext(text = ax.style$names[i], side = 3, line = ax.style$label.dist[i], adj = adjust[3], at = y.vals[1], col = ax.style$label.col[i], cex = ax.style$label.cex[i])
       }
     else
       { y1.ster <- lin.coef[2] * usr[1] + lin.coef[1]
         y2.ster <- lin.coef[2] * usr[2] + lin.coef[1]
         x1.ster <- (usr[3] - lin.coef[1])/lin.coef[2]
         x2.ster <- (usr[4] - lin.coef[1])/lin.coef[2]
         if (lin.coef[2] == 0)
           { if (x.vals[1] < x.vals[h])
               mtext(text = ax.style$names[i], side = 2, line = ax.style$label.dist[i], adj = adjust[2], at = y.vals[1], col = ax.style$label.col[i], cex = ax.style$label.cex[i])
             else
               mtext(text = ax.style$names[i], side = 4, line = ax.style$label.dist[i], adj = adjust[4], at = y.vals[1], col = ax.style$label.col[i], cex = ax.style$label.cex[i])
           }
         if (lin.coef[2] > 0)
           {  if (x.vals[1] < x.vals[h])
                if (y1.ster <= usr[4] & y1.ster >= usr[3])
                  mtext(text = ax.style$names[i], side = 2, line = ax.style$label.dist[i], adj = adjust[2], at = y1.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                else
                  mtext(text = ax.style$names[i], side = 1, line = ax.style$label.dist[i], adj = adjust[1], at = x1.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
              else if (y2.ster <= usr[4] & y2.ster >= usr[3])
                     mtext(text = ax.style$names[i], side = 4, line = ax.style$label.dist[i], adj = adjust[4], at = y2.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                   else
                     mtext(text = ax.style$names[i], side = 3, line = ax.style$label.dist[i], adj = adjust[3], at = x2.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
           }
        if (lin.coef[2] < 0)
          {  if (x.vals[1] < x.vals[h])
               if (y1.ster <= usr[4] & y1.ster >= usr[3])
                 mtext(text = ax.style$names[i], side = 2, line = ax.style$label.dist[i], adj = adjust[2], at = y1.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
               else
                 mtext(text = ax.style$names[i], side = 3, line = ax.style$label.dist[i], adj = adjust[3], at = x2.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
             else if (y2.ster <= usr[4] & y2.ster >= usr[3])
                     mtext(text = ax.style$names[i], side = 4, line = ax.style$label.dist[i], adj = adjust[4], at = y2.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                  else
                     mtext(text = ax.style$names[i], side = 1, line = ax.style$label.dist[i], adj = adjust[1], at = x1.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
          }
      }

  invals <- x.vals < usr[2] & x.vals > usr[1] & y.vals < usr[4] & y.vals > usr[3]
  std.markers <- marker.mat[invals, 3]
  if (is.numeric(std.markers)) std.markers <- zapsmall(std.markers)
  x.vals <- x.vals[invals]
  y.vals <- y.vals[invals]
  if (ax.style$tick.label[i]) label.on.off <- rep(1, sum(invals)) else rep(0, sum(invals))
  if (!ax.style$tick.label[i]) label.on.off[c(1, length(label.on.off))] <- 1
  if(sum(invals)>0) apply(data.frame(x.vals, y.vals, std.markers, label.on.off), 1, .marker.func,
                          coef = lin.coef, col = ax.style$tick.col[i], tick.size = ax.style$tick.size[i],
                          side = ax.style$tick.label.side[i], pos = ax.style$tick.label.pos[i],
                          offset = ax.style$tick.label.offset[i], label.col = ax.style$tick.label.col[i],
                          cex = ax.style$tick.label.cex[i])
  if (!is.null(predict.mat)) apply(cbind(predict.mat, y.vals[1]), 1, .predict.func, coef = lin.coef, col = ax.style$predict.col[i], lty = ax.style$predict.lty[i], lwd = ax.style$predict.lwd[i])
  }
}



  Z <- x$Z

  old.par <- par(pty = "s", ...)
  on.exit(par(old.par))

  plot(Z[, 1] * exp.factor, Z[, 2] * exp.factor, xlim = range(Z[, 1] * exp.factor), ylim = range(Z[, 2] * exp.factor),
          xaxt = "n", yaxt = "n", xlab = "", ylab = "", type = "n", xaxs = "i", yaxs = "i", asp = 1)
  usr <- par("usr")

  if (is.null(x$axes)) x <- axes(x)
  ax.style <- x$axes

  if (length(ax.style$which) > 0)
  {
    z.axes <- lapply(1:length(ax.style$which), .calibrate.axis, x$Xhat, x$means, x$sd, x$ax.one.unit, ax.style$which,
                     ax.style$ticks, ax.style$orthogx, ax.style$orthogy)
    .lin.axes.plot(z.axes, ax.style, predict.mat=NULL)

  }

  if (is.null(x$samples)) x <- samples(x)
  .samples.plot(Z, x$group.aes, x$samples)

  invisible(NULL)
}

