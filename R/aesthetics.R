# ----------------------------------------------------------------------------------------------
#' Aesthetics for biplot samples
#'
#' @description
#' This function allows formatting changes to samples.
#'
#' @param bp an object of class \code{biplot}.
#' @param which vector of which groups of samples to display, with default \code{bp$g}.
#' @param col sample colour, with default \code{blue}.
#' @param pch sample plotting character, with default \code{+}.
#' @param cex sample character expansion, with default \code{1}.
#' @param label logical, whether samples should be labelled or not, with default \code{FALSE}.
#' @param label.col vector of length number of samples with the colour of the labels, defaulting to the
#'                  colour of the sample points.
#' @param label.cex label text expansion, with default \code{0.75}.
#' @param label.side side of the plotting character where label appears, with default \code{bottom}. Note that unlike
#'                   the argument `pos` in `text()`, options are "bottom", "left", "top", "right" and not 1, 2, 3, 4.
#' @param label.offset offset of the label from the data point. See ?text for a detailed explanation of the
#'                     argument `offset`.
#' @param connected logical, whether samples are connected in order of rows of data matrix, with default \code{FALSE}.
#' @param connect.col colour of the connecting line, with default \code{black}.
#' @param connect.lty line type of the connecting line, with default \code{1}.
#' @param connect.lwd line width of the connecting line, with default \code{1}.
#'
#' @details
#' The arguments `which`, `col`, `pch` and `cex` are based on the specification of `group.aes` or `classes`. If no groups
#' are specified, a single colour, plotting character and / or character expansion is expected. If g groups are
#' specified, vectors of length g is expected, or values are recycled to length g.
#'
#' The arguments `label`, `label.cex`, `label.side` and `label.offset` are based on the sample size n. A single value
#' will be recycled n times or a vector of length n is expected.
#'
#' @return A list with the following components is available:
#' \item{which}{which means to display.}
#' \item{col}{colour of the samples.}
#' \item{pch}{plotting character of the samples.}
#' \item{cex}{expansion of the plotting character of the samples.}
#' \item{label}{TRUE or FALSE, whether samples should be labelled.}
#' \item{label.col}{colour of the label.}
#' \item{label.cex}{expansion of the label.}
#' \item{label.side}{side at which to plot the label of samples.}
#' \item{label.offset}{offset of the label from the data point.}
#' \item{connected}{TRUE or FALSE, whether samples should be connected in row order of X.}
#' \item{connect.col}{colour of the connecting line.}
#' \item{connect.lty}{line type of the connecting line.}
#' \item{connect.lwd}{line width of the connecting line.}
#'
#' @usage
#' samples (bp,  which = 1:bp$g, col = ez.col, pch = 3, cex = 1, label = FALSE,
#' label.col = NULL,label.cex = 0.75, label.side = "bottom", label.offset = 0.5,
#' connected = FALSE, connect.col = "black", connect.lty = 1, connect.lwd = 1)
#' @aliases samples
#'
#' @export
#'
#' @examples biplot(iris[,1:4]) |> PCA() |> samples(col="purple",pch=15) |> plot()
samples <- function (bp,  which = 1:bp$g, col = ez.col, pch = 3, cex = 1,
                     label = FALSE, label.col=NULL, label.cex = 0.75, label.side = "bottom", label.offset = 0.5,
                     connected=FALSE, connect.col = "black", connect.lty = 1, connect.lwd = 1)
{
  g <- bp$g
  if (!is.null(which))
  {
    if (!all(is.numeric(which))) which <- match(which, bp$g.names, nomatch = 0)
    which <- which[which <= g]
    which <- which[which > 0]
  }
  if (is.null(which)) sample.group.num <- g else sample.group.num <- length(which)
  while (length(col) < sample.group.num) col <- c(col, col)
  col <- as.vector(col[1:sample.group.num])
  while (length(pch) < sample.group.num) pch <- c(pch, pch)
  pch <- as.vector(pch[1:sample.group.num])
  while (length(cex) < sample.group.num) cex <- c(cex, cex)
  cex <- as.vector(cex[1:sample.group.num])
  n <- bp$n
  if (label[1] == "ggrepel")
  {
     label <- label[1]
     label.side <- NULL
     label.offset <- NULL
   }
  else
  {
    while (length(label) < n) label <- c(label, label)
    label <- as.vector(label[1:n])
    for (i in 1:g) if (is.na(match(i, which))) label[bp$group.aes==bp$g.names[i]] <- NA
#    label <- stats::na.omit(label)

    while (length(label.side) < n) label.side <- c(label.side, label.side)
    label.side <- as.vector(label.side[1:n])
    for (i in 1:g) if (is.na(match(i, which))) label.side[bp$group.aes==bp$g.names[i]] <- NA
#    label.side <- stats::na.omit(label.side)

    while (length(label.offset) < n) label.offset <- c(label.offset, label.offset)
    label.offset <- as.vector(label.offset[1:n])
    for (i in 1:g) if (is.na(match(i, which))) label.offset[bp$group.aes==bp$g.names[i]] <- NA
#    label.offset <- stats::na.omit(label.offset)
  }

  while (length(label.cex) < n) label.cex <- c(label.cex, label.cex)
  label.cex <- as.vector(label.cex[1:n])
  for (i in 1:g) if (is.na(match(i, which))) label.cex[bp$group.aes==bp$g.names[i]] <- NA
#  label.cex <- stats::na.omit(label.cex)

  if (is.null(label.col))
  {
    label.col <- rep(NA, n)
    for (j in 1:g)
      if (!is.na(match(j, which))) label.col[bp$group.aes==bp$g.names[j]] <- col[which==j]
#    label.col <- stats::na.omit(label.col)
  }
  else
  {
    while (length(label.col) < n) label.col <- c(label.col, label.col)
    label.col <- as.vector(label.col[1:n])
  }

  if (length(connected)>1) connected <- connected[1]
  if (length(connect.col)>1) connect.col <- connect.col[1]
  if (length(connect.lty)>1) connect.lty <- connect.lty[1]
  if (length(connect.lwd)>1) connect.lwd <- connect.lwd[1]

  bp$samples = list(which = which, col = col, pch = pch, cex = cex, label = label, label.col = label.col,
                    label.cex = label.cex, label.side = label.side, label.offset = label.offset,
                    connected=connected, connect.col = connect.col, connect.lty = connect.lty,
                    connect.lwd = connect.lwd)
  bp
}

# ----------------------------------------------------------------------------------------------
#' Aesthetics for biplot class / group means
#'
#' @description
#' This function allows formatting changes to class means or group means.
#'
#' @param bp an object of class \code{biplot}.
#' @param which vector of which means to display, with default \code{bp$g}.
#' @param col mean colour, with default to sample colour.
#' @param pch mean plotting character, with default \code{o}.
#' @param cex mean character expansion, with default \code{1}.
#' @param label logical, whether means should be labelled or not, with default \code{TRUE}.
#' @param label.col vector of length g with the colour of the labels, defaulting to the
#'                  colour of the means.
#' @param label.cex label text expansion, with default \code{0.75}.
#' @param label.side side of the plotting character where label appears, with default \code{bottom}. Note that unlike
#'                   the argument `pos` in `text()`, options are "bottom", "left", "top", "right" and not 1, 2, 3, 4.
#' @param label.offset offset of the label from the mean point. See ?text for a detailed explanation of the
#'                     argument `offset`.
#' @details
#' The number of classes or groups (defined by group.aes) is indicated as \code{g}. If an argument is not of length \code{g},
#' recycling is used.
#'
#' @return A list with the following components is available:
#' \item{which}{which means to display.}
#' \item{col}{colour of the means.}
#' \item{pch}{plotting character of the means.}
#' \item{cex}{expansion of the plotting character of the means.}
#' \item{label}{logical, whether means should be labelled.}
#' \item{label.col}{colour of the label.}
#' \item{label.cex}{expansion of the label.}
#' \item{label.side}{side at which to plot the label of means.}
#' \item{label.offset}{offset of the label from the mean point.}
#'
#' @usage
#' means (bp,  which = NULL, col = NULL, pch = 1, cex = 1, label = FALSE,
#' label.col = NULL,label.cex = 0.75, label.side = "bottom", label.offset = 0.5)
#' @aliases means
#'
#' @export
#'
#' @examples biplot(iris[,1:4]) |> PCA() |>
#'           means(col = "purple", pch = 15, cex = 2) |> plot()
means <- function (bp,  which = NULL, col = NULL,
                     pch = 1, cex = 1, label = FALSE, label.col=NULL, label.cex = 0.75,
                     label.side = "bottom", label.offset = 0.5)
{
  if (is.null(bp$samples)) bp <- samples(bp)
  g <- bp$g
  if (!is.null(which))
  {
    if (!all(is.numeric(which))) which <- match(which, bp$g.names, nomatch = 0)
    which <- which[which <= g]
    which <- which[which > 0]
  }
  else which <- bp$samples$which
  class.num <- length(which)

  if (is.null(col))
    if (max(which)<=length(bp$samples$col)) col <- bp$samples$col[which] else col <- bp$samples$col
  while (length(col) < class.num) col <- c(col, col)
  col <- as.vector(col[1:class.num])

  while (length(pch) < class.num) pch <- c(pch, pch)
  pch <- as.vector(pch[1:class.num])
  while (length(cex) < class.num) cex <- c(cex, cex)
  cex <- as.vector(cex[1:class.num])
  if (label[1] == "ggrepel")
  {
    label <- label[1]
    label.side <- NULL
    label.offset <- NULL
  }
  else
  {
    while (length(label) < class.num) label <- c(label, label)
    label <- as.vector(label[1:class.num])
    while (length(label.side) < class.num) label.side <- c(label.side, label.side)
    label.side <- as.vector(label.side[1:class.num])
    while (length(label.offset) < class.num) label.offset <- c(label.offset, label.offset)
    label.offset <- as.vector(label.offset[1:class.num])
  }
  while (length(label.cex) < class.num) label.cex <- c(label.cex, label.cex)
  label.cex <- as.vector(label.cex[1:class.num])

  if (is.null(label.col)) label.col <- col
  else
  {
    while (length(label.col) < class.num) label.col <- c(label.col, label.col)
    label.col <- as.vector(label.col[1:class.num])
  }

  bp$means.aes = list(which = which, col = col, pch = pch, cex = cex, label = label, label.col = label.col,
                      label.cex = label.cex, label.side = label.side, label.offset = label.offset)
  bp
}

# ----------------------------------------------------------------------------------------------
#' Aesthetics for biplot axes
#'
#' @description
#' This function allows formatting changes to axes.
#'
#'
#' @param bp an object of class \code{biplot}.
#' @param X.names refers to the column names of \code{bp} to specify which axes to label.
#' @param which  vector of columns to be displayed in the biplot, with default \code{1:bp$p}.
#' @param col axis colour, with default \code{grey(0.7)}.
#' @param lwd axis line width, with default \code{1}.
#' @param lty axis line type, with default \code{1}.
#' @param label.dir direction of axis label, with default \code{Orthog}.
#' @param label.col axis label colour, with default, \code{col}.
#' @param label.cex axis label expansion, with default \code{0.75}.
#' @param label.line axis label written on which margin line, with default \code{0.1}.
#' @param ticks number of tick marks per axis, with default \code{5}.
#' @param tick.col tick mark colour, with default \code{col}.
#' @param tick.size tick mark size, with default \code{1}.
#' @param tick.label logical, whether axes should be labelled or not, with default \code{TRUE}.
#' @param tick.label.col tick mark label colour, with default \code{tick.col}.
#' @param tick.label.cex tick mark label expansion, with default \code{0.6}.
#' @param tick.label.side side of the tick mark label, with default \code{left}.
#' @param tick.label.offset tick mark label offset, with default \code{0.5}.
#' @param tick.label.pos side of the tick mark label, with default \code{below}.
#' @param predict.col predicted samples colour, with default \code{col}.
#' @param predict.lwd predicted samples line width, with default \code{lwd}.
#' @param predict.lty predicted samples line type, with default \code{lty}.
#' @param ax.names vector of size \code{p} containing user defined names for the variables.
#' @param orthogx the horizontal translation, with default \code{0}.
#' @param orthogy the vertical translation with default \code{0}.
#'
#' @return A list with the following components is available:
#' \item{which}{vector of the columns displayed as axes.}
#' \item{col}{vector of axis colours.}
#' \item{lwd}{vector of axis line widths.}
#' \item{lty}{vector of axis line types.}
#' \item{label.dir}{direction of the axis labels.}
#' \item{label.col}{vector of axis label colours.}
#' \item{label.cex}{vector of axis labels expansions.}
#' \item{label.line}{vector of axis label margin lines from axes.}
#' \item{ticks}{vector representing the number of tick marks per axis.}
#' \item{tick.col}{vector of tick mark colours.}
#' \item{tick.size}{vector of tick mark sizes.}
#' \item{tick.label}{vector of logical values indicating whether axes are labelled.}
#' \item{tick.label.col}{vector of tick mark label colours.}
#' \item{tick.label.cex}{vector of tick mark label expansions.}
#' \item{tick.label.side}{vector of the side of tick mark labels.}
#' \item{tick.label.offset}{vector of tick mark label offsets.}
#' \item{tick.label.pos}{vector of the side of tick mark labels.}
#' \item{predict.col}{vector of colours for the predicted samples.}
#' \item{predict.lty}{vector of line types for the predicted samples.}
#' \item{predict.lwd}{vector of line widths for the predicted samples.}
#' \item{names}{vector of variable names defined by the user.}
#' \item{orthogx}{vector of the horisontal translations for each axis.}
#' \item{orthogy}{vector of the vertical translations for each axis.}
#'
#' @usage
#' axes(bp, X.names=colnames(bp$X), which = 1:bp$p, col = grey(0.7),
#' lwd = 1, lty = 1, label.dir = "Orthog", label.col = col, label.cex = 0.75,
#'  label.line = 0.1, ticks = 5, tick.col = col, tick.size = 1, tick.label = TRUE,
#'  tick.label.col = tick.col, tick.label.cex = 0.6, tick.label.side = "left",
#'  tick.label.offset = 0.5, tick.label.pos = 1, predict.col = col, predict.lwd = lwd,
#'   predict.lty = lty, ax.names = X.names, orthogx = 0, orthogy = 0)
#' @aliases axes
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
                  label.dir = "Orthog", label.col = col, label.cex = 0.75, label.line = 0.1, ticks = 5,
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
  while (length(label.line) < ax.num) label.line <- c(label.line, label.line)
  label.line <- as.vector(label.line[1:ax.num])
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
                 label.line = label.line, ticks = ticks, tick.col = tick.col, tick.size = tick.size, tick.label = tick.label,
                 tick.label.col = tick.label.col, tick.label.cex = tick.label.cex, tick.label.side = tick.label.side,
                 tick.label.offset = tick.label.offset, tick.label.pos = tick.label.pos,
                 predict.col = predict.col, predict.lty = predict.lty, predict.lwd = predict.lwd,
                 names = ax.names, orthogx = orthogx, orthogy = orthogy)
  bp
}

# ----------------------------------------------------------------------------------------------
#' Aesthetics for alpha-bags
#'
#' @param g number of groups.
#' @param g.names names of groups.
#' @param alpha size of alpha bag.
#' @param which which group to select.
#' @param col colour of bags.
#' @param lty line type of bags.
#' @param lwd line width of bags.
#' @param max maximum number of samples to be included in a bag.
#'
#' @noRd
#'
control.alpha.bags <- function (g, g.names, alpha, which, col, lty, lwd, max)
{
  if (!all(is.numeric(which)))
    which <- match(which, g.names, nomatch = 0)
  which <- which[which <= g]
  which <- which[which > 0]
  ww <- length(which)

  if ((length(alpha) > 1) & (length(alpha) != length(which)))
  {
    temp.mat <- expand.grid(which, alpha)
    which <- temp.mat[,1]
    alpha <- temp.mat[,2]
  }
  ww <- length(which)

  bag.num <- length(which)
  while (length(alpha) < bag.num)
    alpha <- c(alpha, alpha)
  alpha <- as.vector(alpha[1:bag.num])
  if (any(alpha < 0 |
          alpha > 0.99))
    stop(message = "alpha not to be negative or larger than 0.99")
  if (is.null(col)) col <- ez.col[which]
  while (length(col) < bag.num)
    col <- c(col, col)
  col <- col[1:bag.num]
  while (length(lty) < bag.num)
    lty <- rep(lty, each=ww)
  lty <- lty[1:bag.num]
  while (length(lwd) < bag.num)
    lwd <- rep(lwd, each=ww)
  lwd <- lwd[1:bag.num]
  list(which = which, alpha = alpha, col = col, lty = lty, lwd = lwd, max = max)
}


# ----------------------------------------------------------------------------------------------
#' Aesthetics for kappa ellipses
#'
#' @param g number of groups.
#' @param ellipse.names ellipse names, typically group names.
#' @param df degrees of freedom.
#' @param kappa value to construct k-ellipse.
#' @param which which group to select for ellipse construction.
#' @param alpha size of alpha bag.
#' @param col colour of ellipse.
#' @param lty line type of ellipse.
#' @param lwd line width of ellipse.
#' @param alpha.transparency opacity.
#'
#' @noRd
#'
control.concentration.ellipse <- function (g, g.names, df, kappa, which,
                                           col, lty, lwd, alpha.transparency)
{
  if (!all(is.numeric(which))) which <- match(which, g.names, nomatch = 0)
  which <- which[which <= g]
  which <- which[which > 0]
  ww <- length(which)

  if ((length(kappa) > 1) & (length(kappa) != length(which)))
  {
    temp.mat <- expand.grid(which, kappa)
    which <- temp.mat[,1]
    kappa <- temp.mat[,2]
  }
  ellipse.num <- length(which)
  while (length(kappa) < ellipse.num) kappa <- c(kappa, kappa)
  kappa <- as.vector(kappa[1:ellipse.num])

  if (is.null(col)) col <- ez.col[which]
  while (length(col) < ellipse.num) col <- c(col, col)
  col <- col[1:ellipse.num]
  while (length(lty) < ellipse.num) lty <- rep(lty, each=ww)
  lty <- lty[1:ellipse.num]
  while (length(lwd) < ellipse.num) lwd <- rep(lwd, each=ww)
  lwd <- as.vector(lwd[1:ellipse.num])
  while (length(alpha.transparency) < ellipse.num) alpha.transparency <- c(alpha.transparency, alpha.transparency)

  list(which = which, kappa = kappa, col = col, lty = lty, lwd = lwd, alpha.transparency = alpha.transparency)
}

# ----------------------------------------------------------------------------------------------
#' Aesthetics for supplementary (new) biplot samples
#'
#' @description
#' This function allows formatting changes to new samples.
#'
#' @param bp an object of class \code{biplot}.
#' @param col new sample colour, with default \code{darkorange1}.
#' @param pch new sample plotting character, with default \code{o}.
#' @param cex new sample character expansion, with default \code{1}.
#' @param label logical, whether samples should be labelled or not, with default \code{FALSE}.
#' @param label.col vector of length number of new samples with the colour of the labels, defaulting to the
#'                  colour of the sample points.
#' @param label.cex label text expansion, with default \code{0.75}.
#' @param label.side side of the plotting character where label appears, with default \code{bottom}. Note that unlike
#'                   the argument `pos` in `text()`, options are "bottom", "left", "top", "right" and not 1, 2, 3, 4.
#' @param label.offset offset of the label from the data point. See ?text for a detailed explanation of the
#'                     argument `offset`.
#' @param connected logical, whether samples are connected in order of rows of data matrix, with default \code{FALSE}.
#' @param connect.col colour of the connecting line, with default \code{black}.
#' @param connect.lty line type of the connecting line, with default \code{1}.
#' @param connect.lwd line width of the connecting line, with default \code{1}.
#'
#' @return A list with the following components is available:
#' \item{col}{colour of the samples.}
#' \item{pch}{plotting character of the samples.}
#' \item{cex}{expansion of the plotting character of the samples.}
#' \item{label}{TRUE or FALSE, whether samples should be labelled.}
#' \item{label.col}{colour of the label.}
#' \item{label.cex}{expansion of the label.}
#' \item{label.side}{side at which to plot the label of samples.}
#' \item{label.offset}{offset of the label from the data point.}
#' \item{connected}{TRUE or FALSE, whether samples should be connected in row order of X.}
#' \item{connect.col}{colour of the connecting line.}
#' \item{connect.lty}{line type of the connecting line.}
#' \item{connect.lwd}{line width of the connecting line.}
#'
#' @usage
#' newsamples (bp,  col = "darkorange1", pch = 1, cex = 1, label = FALSE,
#' label.col = NULL,label.cex = 0.75, label.side = "bottom", label.offset = 0.5,
#' connected = FALSE, connect.col = "black", connect.lty=1, connect.lwd=1)
#' @aliases newsamples
#'
#' @export
#'
#' @examples
#' biplot(data = iris[1:145,]) |> PCA() |> samples(col = "grey") |>
#' interpolate(newdata = iris[146:150,]) |> newsamples(col = rainbow(6), pch=15) |> plot()

newsamples <- function (bp,  col = "darkorange1", pch = 1, cex = 1,
                        label = FALSE, label.col=NULL, label.cex = 0.75, label.side = "bottom", label.offset = 0.5,
                        connected=FALSE, connect.col = "black", connect.lty = 1, connect.lwd = 1)
{
  nn <- nrow(bp$Xnew)
  while (length(col) < nn) col <- c(col, col)
  col <- as.vector(col[1:nn])
  while (length(pch) < nn) pch <- c(pch, pch)
  pch <- as.vector(pch[1:nn])
  while (length(cex) < nn) cex <- c(cex, cex)
  cex <- as.vector(cex[1:nn])
  if (label[1] == "ggrepel")
  {
    label <- label[1]
    label.side <- NULL
    label.offset <- NULL
  }
  else
  {
    while (length(label) < nn) label <- c(label, label)
    label <- as.vector(label[1:nn])
    while (length(label.side) < nn) label.side <- c(label.side, label.side)
    label.side <- as.vector(label.side[1:nn])
    while (length(label.offset) < nn) label.offset <- c(label.offset, label.offset)
    label.offset <- as.vector(label.offset[1:nn])
  }
  while (length(label.cex) < nn) label.cex <- c(label.cex, label.cex)
  label.cex <- as.vector(label.cex[1:nn])
  if (is.null(label.col)) label.col <- col
  else
  {
    while (length(label.col) < nn) label.col <- c(label.col, label.col)
    label.col <- as.vector(label.col[1:nn])
  }
  if (length(connected)>1) connected <- connected[1]
  if (length(connect.col)>1) connect.col <- connect.col[1]
  if (length(connect.lty)>1) connect.lty <- connect.lty[1]
  if (length(connect.lwd)>1) connect.lwd <- connect.lwd[1]

  bp$newsamples = list(col = col, pch = pch, cex = cex, label = label, label.col = label.col,
                    label.cex = label.cex, label.side = label.side, label.offset = label.offset,
                    connected=connected, connect.col = connect.col, connect.lty = connect.lty,
                    connect.lwd = connect.lwd)
  bp
}
