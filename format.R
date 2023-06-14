# previous function biplot.sample.control
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
#previous function biplot.mean.control
#replace class.names with classes for CVA ("group")
#function only applicable to CVA biplot, assuming CVA classes

#' Formatting of mean in biplot
#'
#' @param bp biplot object
#' @param classes CVA classes, will be null otherwise
#' @param which selected class
#' @param col mean colour
#' @param pch mean plotting character
#' @param cex mean character expansion
#' @param label logical, whether label should be printed
#' @param label.cex label text expansion
#' @param label.side side of the plotting character where label appears
#' @param alpha opacity
#'
#' @return
#' @export
#'
#' @examples
means <- function (bp, classes=NULL, which = NULL,
                                 col = ez.col,
                                 pch = 16, cex = 1.5, label = T, label.cex = 1, label.side = "bottom", alpha = 1)
{
g <- bp$g
if(is.null(classes)){classes <- bp$g.names}
if (!all(is.numeric(which))) which <- match(which, classes, nomatch = 0)
which <- which[which <= g]
which <- which[which > 0]
mean.num <- length(which)
while (length(col) < mean.num) col <- c(col, col)
col <- as.vector(col[1:mean.num])
while (length(pch) < mean.num) pch <- c(pch, pch)
pch <- as.vector(pch[1:mean.num])
while (length(cex) < mean.num) cex <- c(cex, cex)
cex <- as.vector(cex[1:mean.num])
while (length(label) < mean.num) label <- c(label, label)
label <- as.vector(label[1:mean.num])
while (length(label.cex) < mean.num) label.cex <- c(label.cex, label.cex)
label.cex <- as.vector(label.cex[1:mean.num])
while (length(label.side) < mean.num) label.side <- c(label.side, label.side)
label.side <- as.vector(label.side[1:mean.num])
while (length(alpha) < mean.num) alpha <- c(alpha, alpha)
alpha <- as.vector(alpha[1:mean.num])

bp$means = list(which = which, col = col, pch = pch, cex = cex, label = label, label.cex = label.cex, label.side = label.side,
     alpha = alpha)

bp
}
###
#previous biplot.new.sample.control
#' Title
#'
#' @param bp biplot object
#' @param n number of rows matrix of size samples x p of observations
#' @param col colour for new sample points
#' @param pch plotting character for new sample points
#' @param cex plotting character expansion for new sample points
#' @param label logical, whether label should be printed
#' @param label.cex label text expansion
#' @param label.side side of the plotting character where label appears
#' @param alpha opacity
#'
#' @return
#' @export
#'
#' @examples
#question, will n be in PCAbiplot list from nrow(new.X)?
new.samples <- function (bp, n, col = "black", pch = 1, cex = 1, label = F, label.cex = 0.75,
                                       label.side = "bottom", alpha = 1)
{  while (length(col) < n) col <- c(col, col)
col <- as.vector(col[1:n])
while (length(pch) < n) pch <- c(pch, pch)
pch <- as.vector(pch[1:n])
while (length(cex) < n) cex <- c(cex, cex)
cex <- as.vector(cex[1:n])
while (length(label) < n) label <- c(label, label)
label <- as.vector(label[1:n])
while (length(label.cex) < n) label.cex <- c(label.cex, label.cex)
label.cex <- as.vector(label.cex[1:n])
while (length(label.side) < n) label.side <- c(label.side, label.side)
label.side <- as.vector(label.side[1:n])
while (length(alpha) < n) alpha <- c(alpha, alpha)
alpha <- as.vector(alpha[1:n])

bp$new.samples = list(col = col, pch = pch, cex = cex, label = label, label.cex = label.cex, label.side = label.side, alpha = alpha)

bp
}

###
#previous biplot.ax.control
#taking out X.names since it is ax.names
#taking out p since it is in bp
#taking out type, since only prediction biplot
#label.dir previously just label
#oblique taken out for future rotation
ax <- function (bp, which = 1:p, col = grey(0.7), lwd = 1, lty = 1,
                               label.dir = "Orthog", label.col = col, label.cex = 0.75, label.dist = 0, ticks = 5,
                               tick.col = col, tick.size = 1, tick.label = T, tick.label.col = tick.col, tick.label.cex = 0.6,
                               tick.label.side = "left", tick.label.offset = 0.5, tick.label.pos = 1,
                               predict.col = col, predict.lwd = lwd, predict.lty = lty, ax.names = X.names,
                               orthogx = 0, orthogy = 0)
{  if (!all(is.numeric(which))) which <- match(which, X.names, nomatch = 0)
which <- which[which <= p]
which <- which[which > 0]
ax.num <- length(which)
while (length(col) < ax.num) col <- c(col, col)
col <- as.vector(col[1:ax.num])
while (length(lwd) < ax.num) lwd <- c(lwd, lwd)
lwd <- as.vector(lwd[1:ax.num])
while (length(lty) < ax.num) lty <- c(lty, lty)
lty <- as.vector(lty[1:ax.num])
if (label.dir != "Orthog" & label.dir != "Hor" & label.dir != "Paral") stop("Incorrect specification of axis label direction")
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
bp$ax = list(which = which, type = type, col = col, lwd = lwd, lty = lty, label.dir = label.dir, label.col = label.col, label.cex = label.cex,
     label.dist = label.dist, ticks = ticks, tick.col = tick.col, tick.size = tick.size, tick.label = tick.label,
     tick.label.col = tick.label.col, tick.label.cex = tick.label.cex, tick.label.side = tick.label.side,
     tick.label.offset = tick.label.offset, tick.label.pos = tick.label.pos,
     predict.col = predict.col, predict.lty = predict.lty, predict.lwd = predict.lwd,
     names = ax.names, orthogx = orthogx, orthogy = orthogy)
bp
}

###previous biplot.alpha.bag.control
#' Title
#'
#' @param g number of groups
#' @param g.names names of groups
#' @param alpha size of alpha bag
#' @param which which group to select
#' @param col colour of bags
#' @param lty line type of bags
#' @param lwd line width of bags
#' @param max maximum number of samples to be included in a bag
#' @param pch plotting character for Tukey median
#' @param cex character expansion
#'
#' @return
#' @export
#'
#' @examples
control.alpha.bags <- function (g, g.names, alpha=0.95, which = NULL,
                                      col = ez.col, lty = 1, lwd = 1, max = 2500, pch = 15, cex = 1)
{ if (!all(is.numeric(which))) which <- match(which, bag.names, nomatch = 0)
which <- which[which <= g]
which <- which[which > 0]
bag.num <- length(which)
while (length(alpha) < bag.num) alpha <- c(alpha, alpha)
alpha <- as.vector(alpha[1:bag.num])
if (any(alpha < 0 | alpha > 0.99)) stop(message = "alpha not to be negative or larger than 0.99")
alpha.entered <- alpha
while (length(col) < bag.num) col <- c(col, col)
col <- as.vector(col[1:bag.num])
while (length(lty) < bag.num) lty <- c(lty, lty)
lty <- as.vector(lty[1:bag.num])
while (length(lwd) < bag.num) lwd <- c(lwd, lwd)
lwd <- as.vector(lwd[1:bag.num])
while (length(cex) < bag.num) cex <- c(cex, cex)
cex <- as.vector(cex[1:bag.num])
while (length(pch) < bag.num) pch <- c(pch, pch)
pch <- as.vector(pch[1:bag.num])
while (length(max) < bag.num) max <- c(max, max)
max <- as.vector(max[1:bag.num])

list(which = which, alpha = alpha, col = col, lty = lty, lwd = lwd, max = max, pch = pch, cex = cex)
}

##previous function biplot.kappa.ellipse.control
#' Title
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
#' @return
#' @export
#'
#' @examples
concentration.ellipse.control <- function (g, ellipse.names, df=2, kappa = NULL, which = NULL, alpha = 0.95,
                           col = ez.col, lty = 1, lwd = 1, alpha.transparency = 0.5)
{if (!all(is.numeric(which))) which <- match(which, ellipse.names, nomatch = 0)
which <- which[which <= g]
which <- which[which > 0]
ellipse.num <- length(which)
if (!is.null(alpha)) { while (length(alpha) < ellipse.num) alpha <- c(alpha, alpha)
alpha <- as.vector(alpha[1:ellipse.num])
if (any(alpha < 0 | alpha > 0.99)) stop(message = "alpha not to be negative or larger than 0.99")
alpha.entered <- alpha
if (is.null(kappa)) kappa <- sqrt(qchisq(alpha, df))                                               }
while (length(kappa) < ellipse.num) kappa <- c(kappa, kappa)
kappa <- as.vector(kappa[1:ellipse.num])
while (length(col) < ellipse.num) col <- c(col, col)
col <- as.vector(col[1:ellipse.num])
while (length(lty) < ellipse.num) lty <- c(lty, lty)
lty <- as.vector(lty[1:ellipse.num])
while (length(lwd) < ellipse.num) lwd <- c(lwd, lwd)
lwd <- as.vector(lwd[1:ellipse.num])
while (length(alpha.transparency) < ellipse.num) alpha.transparency <- c(alpha.transparency, alpha.transparency)
alpha.transparency <- as.vector(alpha.transparency[1:ellipse.num])

list(which = which, kappa = kappa, col = col, lty = lty, lwd = lwd, alpha.transparency = alpha.transparency)

}

#previous function biplot.density.2D.control

#' Title
#'
#' @param bp biplot object
#' @param classes CVA classes, will be null otherwise
#' @param which selected class
#' @param contours logical, to specify whether contours should be included
#' @param h h
#' @param n n
#' @param col A vector of at least two components specifying the colours of the density response surface.
#' @param contour.col A vector of at least two components specifying the colours of the density response surface
#' @param cuts Number of interpolated colours to be used in the density response surface. Default is 50.
#' @param cex character expansion
#' @param tcl fine-tuning the colour key
#' @param mgp fine-tuning the colour key
#' @param layout.heights A two-component vector specifying the top and bottom panels in the graph window. The top panel is where the biplot is constructed and the bottom panel provides for the colour key.
#' @param legend.mar control margins surrounding the colour key
#'
#' @return
#' @export
#'
#' @examples
density.2D <- function (bp, classes=NULL, which = NULL, contours = F, h = NULL, n = 100,
                                       col = c("green", "yellow", "red"), contour.col = "black", cuts = 50, cex = 0.6,
                                       tcl = -0.2, mgp = c(0, -0.25, 0), layout.heights = c(100, 10), legend.mar = c(2, 5, 0, 5))
{ g <- bp$g
if(is.null(classes)){classes <- bp$g.names}
if (!is.null(which)) if (which == "all") which <- 0
else if (!all(is.numeric(which))) which <- match(which, classes, nomatch = 0)
which <- which[which <= g]
which <- which[which >= 0]
if (!is.null(which)) which <- which[1]
list(which = which, contours = contours, h = h, n = n, col = col, contour.col = contour.col, cuts = cuts, cex = cex,
     tcl = tcl, mgp = mgp, layout.heights = layout.heights, legend.mar = legend.mar)
}

###previous biplot.legend.type.control
#' Title
#'
#' @param samples logical, whether legend should be printed for samples
#' @param means logical, whether legend should be printed for means
#' @param bags logical, whether legend should be printed for bags
#'
#' @return
#' @export
#'
#' @examples
legend.type <- function (samples = F, means = F, bags = F)
{  list(samples = samples, means = means, bags = bags)
}
