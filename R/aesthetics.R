# ----------------------------------------------------------------------------------------------
#' Format aesthetics for the biplot samples
#'
#' @description
#' This function allows the user to format the aesthetics for the samples.
#'
#' @param bp an object of class \code{biplot}.
#' @param which a vector containing the groups or classes for which the samples should be displayed, with default \code{bp$g}.
#' @param col the colour(s) for the samples, with default \code{blue}.
#' @param pch the plotting character(s) for the samples, with default •.
#' @param cex the character expansion(s) for the samples, with default \code{1}.
#' @param label a logical value indicating whether the samples should be labelled, with default \code{FALSE}. Alternatively, specify \code{"ggrepel"} for non-overlapping placement of labels.
#' @param label.name a vector of the same length as \code{which} with label names for the samples, with default \code{NULL}. If \code{NULL}, the \code{rownames(bp)} are used. Alternatively, a custom vector of length \code{n} should be used.
#' @param label.col a vector of the same length as \code{which} with label colours for the samples, with default as the same colour of the sample points.
#' @param label.cex a vector of the same length as \code{which} with label text expansions for the samples, with default \code{0.75}.
#' @param label.side the side at which the label of the plotted point appears, with default \code{bottom}. Note that unlike the argument \code{pos} in \code{text()}, options are "\code{bottom}", "\code{left}", "\code{top}", "\code{right}" and not \code{1}, \code{2}, \code{3}, \code{4}.
#' @param label.offset the offset of the label from the plotted point. See \code{?text} for a detailed explanation of the argument \code{offset}.
#' @param connected a logical value indicating whether samples are connected in order of rows of the data matrix, with default \code{FALSE}.
#' @param connect.col the colour of the connecting line, with default \code{black}.
#' @param connect.lty the line type of the connecting line, with default \code{1}.
#' @param connect.lwd the line width of the connecting line, with default \code{1}.
#' @param opacity the opacity level of the plotted points, with default \code{1} for an opaque point.
#'
#' @details
#' The arguments \code{which}, \code{col}, \code{pch} and \code{cex} are based on the specification of \code{group.aes} or \code{classes}. If no groups are specified, a single colour, plotting character and / or character expansion is expected. If \eqn{g} groups are
#' specified, vectors of length \eqn{g} is expected, or values are recycled to length \eqn{g}.
#' 
#' The arguments \code{label}, \code{label.cex}, \code{label.side} and \code{label.offset} are based on the sample size \eqn{n}. A single value
#' will be recycled \eqn{n} times or a vector of length \eqn{n} is expected.
#'
#' @return The object of class \code{biplot} will be appended with a list called \code{samples} containing the following elements:
#' \item{which}{a vector containing the groups or classes for which the samples (and means) are displayed.}
#' \item{col}{the colour(s) of the samples.}
#' \item{pch}{the plotting character(s) of the samples.}
#' \item{cex}{the character expansion(s) of the plotting character(s) of the samples.}
#' \item{label}{a logical value indicating whether samples are labelled.}
#' \item{label.name}{the label names of the samples.}
#' \item{label.col}{the label colours of the samples.}
#' \item{label.cex}{the label text expansions of the samples.}
#' \item{label.side}{the side at which the label of the plotted point appears..}
#' \item{label.offset}{the offset of the label from the plotted point.}
#' \item{connected}{a logical value indicating whether samples are connected in order of the rows of the data matrix.}
#' \item{connect.col}{the colour of the connecting line.}
#' \item{connect.lty}{the line type of the connecting line.}
#' \item{connect.lwd}{the line width of the connecting line.}
#' \item{opacity}{the opacity level of the plotted points.}
#'
#' @seealso [biplot()]
#'
#' @usage
#' samples (bp,  which = 1:bp$g, col = ez.col, pch = 16, cex = 1,
#' label = FALSE, label.name = NULL, label.col=NULL, label.cex = 0.75, 
#' label.side = "bottom", label.offset = 0.5,
#' connected=FALSE, connect.col = "black", connect.lty = 1, 
#' connect.lwd = 1, opacity = 1)
#' @aliases samples
#'
#' @export
#'
#' @examples 
#' biplot(iris[,1:4]) |> PCA() |> samples(col="purple",pch=15, opacity=0.5) |> plot()
#' biplot(iris[,1:4]) |> PCA() |> 
#'   samples(col="purple",pch=NA, opacity=0.5, label = TRUE) |> plot()
#' biplot(iris[,1:4]) |> PCA() |> 
#'   samples(col="purple",pch=NA, opacity=0.5, label = TRUE, 
#'           label.name = paste("s:",1:150, sep="")) |> 
#'   plot()
#' biplot(iris[,1:4]) |> PCA() |> 
#'   samples(col="purple",pch=NA, opacity=0.5, label = "ggrepel") |> plot()
#' 
samples <- function (bp,  which = 1:bp$g, col = ez.col, pch = 16, 
                     cex = 1, label = FALSE, label.name = NULL, 
                     label.col=NULL, label.cex = 0.75, 
                     label.side = "bottom", label.offset = 0.5,
                     connected=FALSE, connect.col = "black", 
                     connect.lty = 1,  connect.lwd = 1, opacity = 1)
{
  g <- bp$g
  n <- bp$n
  p <- bp$p
  
  if(is.null(which) & length(col)==0) col <- ez.col
  
 if(inherits(bp,"CA")) 
 {label <- TRUE}
  
 if(!is.null(label.name) | !is.null(label.col) | 
    any(label.side!="bottom") | any(label.offset !=0.5) | any(label.cex!=0.75))
   label<-TRUE
  
 if(is.null(label.name)) label.name <- rownames(bp$Z)
  else label <- TRUE
  
  #This piece of code is just to ensure which arguments in samples() and alpha.bag() lines up
  # to plot only the specified alpha bags and points
  if(!is.null(bp$alpha.bag.aes$which) & !is.null(bp$alpha.bag.outside)){
    if(length(which) != length(bp$alpha.bag.aes$which)){
      message("NOTE in samples(): 'which' argument overwritten in alpha.bags()")
      which<-bp$alpha.bag.aes$which
    }
    else if(all(sort(which) != sort(bp$alpha.bag.aes$which))){
      message("NOTE in samples(): 'which' argument overwritten in alpha.bags()")
      which<-bp$alpha.bag.aes$which
    }
  }
  
  
  if (!is.null(which))
  {
    if (!all(is.numeric(which))) which <- match(which, bp$g.names, nomatch = 0)
    which <- which[which <= g]
    which <- which[which > 0]
  }
  if (is.null(which)) 
    sample.group.num <- g 
  else 
    sample.group.num <- length(which)
  #if(!is.null(which))
    
  # Expand col to length g
  col.len <- length(col)
  col <- col[ifelse(1:g%%col.len==0,col.len,1:g%%col.len)]
  if(is.null(col)){col <- rep(NA, g)}
  col <- grDevices::adjustcolor(col,opacity)

  # Expand pch to length g
  pch.len <- length(pch)
  pch <- pch[ifelse(1:g%%pch.len==0,pch.len,1:g%%pch.len)]
  if(is.null(pch)){pch <- rep("", g)}

  # Expand cex to length g
  cex.len <- length(cex)
  cex <- cex[ifelse(1:g%%cex.len==0,cex.len,1:g%%cex.len)]
  if(is.null(cex)){cex <- rep(0, g)}
  
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

  bp$samples = list(which = which, col = col, pch = pch, cex = cex, label = label, label.name = label.name, label.col = label.col,
                    label.cex = label.cex, label.side = label.side, label.offset = label.offset,
                    connected=connected, connect.col = connect.col, connect.lty = connect.lty,
                    connect.lwd = connect.lwd, opacity = opacity)
  bp
}

# ----------------------------------------------------------------------------------------------
#' Format aesthetics for the class or group means
#'
#' @description
#' This function allows the user to format the aesthetics for the class means or group means.
#'
#' @param bp an object of class \code{biplot}.
#' @param which a vector containing the groups or classes for which the means should be displayed, with default \code{bp$g}.
#' @param col the colour(s) for the means, with default as the colour of the samples.
#' @param pch the plotting character(s) for the means, with default ■.
#' @param cex the character expansion(s) for the means, with default \code{1}.
#' @param label a logical value indicating whether the means should be labelled, with default \code{TRUE}.
#' @param label.col a vector of the same length as \code{which} with label colours for the means, with default as the colour of the means.
#' @param label.cex a vector of the same length as \code{which} with label text expansions for the means, with default \code{0.75}.
#' @param label.side the side at which the label of the plotted mean point appears, with default \code{bottom}. Note that unlike the argument \code{pos} in \code{text()}, options are "\code{bottom}", "\code{left}", "\code{top}", "\code{right}" and not \code{1}, \code{2}, \code{3}, \code{4}.
#' @param label.offset the offset of the label from the plotted mean point. See \code{?text} for a detailed explanation of the argument \code{offset}.
#' @param opacity transparency of means.
#' @param shade.darker a logical value indicating whether the colour of the mean points should be made a shade darker than the default or specified colour, with default \code{TRUE}.
#' 
#' @details
#' The number of classes or groups (defined by group.aes) is indicated as \code{g}. If an argument is not of length \code{g},
#' recycling is used.
#'
#' @return The object of class \code{biplot} will be appended with a list called \code{means} containing the following elements:
#' \item{which}{a vector containing the groups or classes for which the means are displayed.}
#' \item{col}{the colour(s) of the means.}
#' \item{pch}{the plotting character(s) of the means.}
#' \item{cex}{the character expansion(s) of the plotting character(s) of the means.}
#' \item{label}{a logical value indicating whether means are labelled.}
#' \item{label.col}{the label colours of the means.}
#' \item{label.cex}{the label text expansions of the samples.}
#' \item{label.side}{the side at which the label of the plotted mean point appears.}
#' \item{label.offset}{the offset of the label from the plotted mean point.}
#' \item{opacity}{the opacity level of the plotted points.}
#'
#' @seealso [biplot()]
#' 
#' @usage
#' means (bp,  which = NULL, col = NULL, pch = 15, cex = 1, label = FALSE,
#' label.col = NULL,label.cex = 0.75, label.side = "bottom", label.offset = 0.5,
#' opacity = 1, shade.darker = TRUE)
#' @aliases means
#'
#' @export
#'
#' @examples biplot(iris[,1:4]) |> PCA() |>
#'           means(col = "purple", pch = 15, cex = 2) |> plot()
means <- function (bp,  which = NULL, col = NULL,
                     pch = 15, cex = 1, label = FALSE, label.col=NULL, label.cex = 0.75,
                     label.side = "bottom", label.offset = 0.5, opacity = 1, shade.darker = TRUE)
{
  bp$show.class.means <- TRUE
  if (is.null(bp$samples)) bp <- samples(bp)
  g <- bp$g
  
  if(!is.null(label.col) | label.side!="bottom" | label.offset !=0.5 | label.cex!=0.75)
    label<-TRUE
  if (!is.null(which))
  {
    if (!all(is.numeric(which))) which <- match(which, bp$g.names, nomatch = 0)
    which <- which[which <= g]
    which <- which[which > 0]
  }
  else if (!is.null(bp$samples$which)) which <- bp$samples$which else which <- 1:g
  class.num <- length(which)


  if (is.null(col))
  {
    if (max(which)<=length(bp$samples$col)) col <- bp$samples$col[which] else col <- bp$samples$col
    if (shade.darker) for (j in 1:length(col))  col[j] <- grDevices::colorRampPalette(c(col[j],"black"))(5)[2]
  }
  # Expand col to length g
  col.len <- length(col)
  col <- col[ifelse(1:g%%col.len==0,col.len,1:g%%col.len)]
  if(is.null(col)){col <- rep(NA, g)}
  if(!is.null(opacity))col <- grDevices::adjustcolor(col,opacity)

  # Expand pch to length g
  pch.len <- length(pch)
  pch <- pch[ifelse(1:g%%pch.len==0,pch.len,1:g%%pch.len)]
  if(is.null(pch)){pch <- rep("", g)}

  # Expand cex to length g
  cex.len <- length(cex)
  cex <- cex[ifelse(1:g%%cex.len==0,cex.len,1:g%%cex.len)]
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
                      label.cex = label.cex, label.side = label.side, label.offset = label.offset, opacity = opacity)
  bp
}

# ----------------------------------------------------------------------------------------------
#' Format aesthetics for the biplot axes
#'
#' @description
#' This function allows the user to format the aesthetics for the biplot axes.
#'
#'
#' @param bp an object of class \code{biplot}.
#' @param X.names a vector of column names of \code{bp} to specify which axes should be labelled.
#' @param which a vector containing the columns or variables for which the axes should be displayed, with default \code{1:p}.
#' @param col the colour(s) for the axes, with default \code{grey(0.7)}. Alternatively, provide a vector of colours corresponding to \code{X.names}.
#' @param lwd the line width(s) for the axes, with default \code{1}.
#' @param lty the line type(s) for the axes, with default \code{1}.
#' @param label.dir a character string indicating the placement of the axis titles to the side of the figure. One of "\code{Orthog}" for axis titles to appear orthogonal to the side of the figure (default) , "\code{Hor}" for axis titles to appear horizontally or "\code{Paral}" for axis titles to appear parallel to the side of the figure.
#' @param label.col the colour(s) for the axis labels, with default, \code{col}.
#' @param label.cex the label expansion for the axis labels, with default \code{0.75}.
#' @param label.line the distance of the axis title from the side of the figure, with default \code{0.1}.
#' @param label.offset a four-component numeric vector controlling the distances axis titles are displayed from the side of the figure, with default \code{rep(0,4)}. Sides are numbered \code{1} to \code{4} according to \code{R} conventions.
#' @param ticks an integer-valued vector indicating the number of tickmarks for each axis, with default \code{5} for each axis.
#' @param tick.col the colour(s) for the tick marks, with default \code{col}.
#' @param tick.size a vector specifying the sizes of tick marks for each axis, with default \code{1} for each .
#' @param tick.label a logical value indicating whether the axes should be labelled, with default \code{TRUE}.
#' @param tick.label.side a character string indicating the position of the tick label. One of "\code{below}" for the label to appear below the tick mark (default) or "\code{above}" for the label to appear above the tick mark.
#' @param tick.label.col the colour(s) for the tick mark labels, with default \code{tick.col}.
#' @param tick.label.cex the label expansion for the tick mark labels, with default \code{0.6}.
#' @param predict.col the colour(s) for the predicted samples, with default \code{col}.
#' @param predict.lwd the line width(s) for the predicted samples, with default \code{lwd}.
#' @param predict.lty the line type(s) for the predicted samples, with default \code{lty}.
#' @param ax.names a vector of size \code{p} containing user defined titles for the axes.
#' @param orthogx a numeric vector of size \code{p} specifying the x-coordinate of the parallel transformation of each axis, with default \code{0} for each axis. This is only used when \code{dim.biplot = 2}. 
#' @param orthogy a numeric vector of size \code{p} specifying the y-coordinate of the parallel transformation of each axis, with default \code{0} for each axis. This is only used when \code{dim.biplot = 2}.
#' @param vectors a logical value indicating whether vector representation (calibrated axes) should be displayed on the biplot. This is only used when a PCA biplot is produced.
#' @param unit.circle a logical value indicating whether a unit circle should be displayed on the biplot.
#'
#' @return The object of class \code{biplot} will be appended with a list called \code{axes} containing the following elements:
#' \item{which}{a vector containing the columns for which the axes are displayed.}
#' \item{col}{the colour(s) of the axes.}
#' \item{lwd}{the line width(s) of the axes.}
#' \item{lty}{the line type(s) of the axes.}
#' \item{label.dir}{the placement of the axis titles to the side of the figure.}
#' \item{label.col}{the colour(s) of the axis titles.}
#' \item{label.cex}{the label expansion(s) of the axis titles.}
#' \item{label.line}{the distance(s) of the axis titles from the side of the figure.}
#' \item{ticks}{the number of tick marks per axis.}
#' \item{tick.col}{the colour(s) of the tick marks.}
#' \item{tick.size}{the size(s) of the tick marks.}
#' \item{tick.label}{logical value(s) indicating whether axes are labelled.}
#' \item{tick.label.side}{the position of the tick mark labels.}
#' \item{tick.label.col}{the colour(s) of the tick mark labels.}
#' \item{tick.label.cex}{the expansion(s) of the tick mark labels.}
#' \item{predict.col}{the colour(s) of the predicted samples.}
#' \item{predict.lty}{the line type(s) of the predicted samples.}
#' \item{predict.lwd}{the line width(s) of the predicted samples.}
#' \item{names}{the user defined axis titles.}
#' \item{orthogx}{the horizontal translations for each axis.}
#' \item{orthogy}{the vertical translations for each axis.}
#' \item{vectors}{a logical value indicating whether calibrated axes are plotted.}
#' 
#' @seealso [biplot()]
#'
#' @usage
#' axes(bp, X.names=colnames(bp$X), which = 1:bp$p, col = grey(0.7), lwd = 1, lty = 1,
#' label.dir = "Orthog", label.col = col, label.cex = 0.75, label.line = 0.1, 
#' label.offset=rep(0,4), ticks = 5, tick.col = col, tick.size = 1, tick.label = TRUE, 
#' tick.label.side = "below", tick.label.col = tick.col, tick.label.cex = 0.6,
#' predict.col = col, predict.lwd = lwd, predict.lty = lty, ax.names = X.names,
#' orthogx = 0, orthogy = 0, vectors = FALSE, unit.circle=FALSE)
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
                  label.dir = "Orthog", label.col = col, label.cex = 0.75, label.line = 0.1, 
                  label.offset=rep(0,4), ticks = 5,
                  tick.col = col, tick.size = 1, tick.label = TRUE, tick.label.side = "below",
                  tick.label.col = tick.col, tick.label.cex = 0.6,
                  predict.col = col, predict.lwd = lwd, predict.lty = lty, ax.names = X.names,
                  orthogx = 0, orthogy = 0,vectors = FALSE, unit.circle=FALSE)
{
  p <- bp$p
  if (!is.null(p))
  {
    if (!all(is.numeric(which))) which <- match(which, X.names, nomatch = 0)
    which <- which[which <= p]
    which <- which[which > 0]
    ax.num <- length(which)
    
    
    col.len <- length(col)
    col <- col[ifelse(1:ax.num%%col.len==0,col.len,1:ax.num%%col.len)]
    if(is.null(col)){col <- rep(NA, ax.num)}
    
    lwd.len <- length(lwd)
    lwd <- lwd[ifelse(1:ax.num%%lwd.len==0,lwd.len,1:ax.num%%lwd.len)]
    if(is.null(lwd)){lwd <- rep(0, ax.num)}
    
    lty.len <- length(lty)
    lty <- lty[ifelse(1:ax.num%%lty.len==0,lty.len,1:ax.num%%lty.len)]
    if(is.null(lty)){lty <- rep(0, ax.num)}
    if (label.dir != "Orthog" & label.dir != "Hor" & label.dir != "Paral")
      stop("Incorrect specification of axis label direction")
    
    label.col.len <- length(label.col)
    label.col <- label.col[ifelse(1:ax.num%%label.col.len==0,label.col.len,1:ax.num%%label.col.len)]
    if(is.null(label.col)){label.col <- rep(0, ax.num)}
    
    label.cex.len <- length(label.cex)
    label.cex <- label.cex[ifelse(1:ax.num%%label.cex.len==0,label.cex.len,1:ax.num%%label.cex.len)]
    if(is.null(label.cex)){label.cex <- rep(0, ax.num)}
    
    label.line.len <- length(label.line)
    label.line <- label.line[ifelse(1:ax.num%%label.line.len==0,label.line.len,1:ax.num%%label.line.len)]
    if(is.null(label.line)){label.line <- rep(0, ax.num)}
    
    ticks.len <- length(ticks)
    ticks <- ticks[ifelse(1:ax.num%%ticks.len==0,ticks.len,1:ax.num%%ticks.len)]
    if(is.null(ticks)){ticks <- rep(0, ax.num)}
    
    tick.col.len <- length(tick.col)
    tick.col <- tick.col[ifelse(1:ax.num%%tick.col.len==0,tick.col.len,1:ax.num%%tick.col.len)]
    if(is.null(tick.col)){tick.col <- rep(0, ax.num)}
    
    tick.size.len <- length(tick.size)
    tick.size <- tick.size[ifelse(1:ax.num%%tick.size.len==0,tick.size.len,1:ax.num%%tick.size.len)]
    if(is.null(tick.size)){tick.size <- rep(0, ax.num)}
    
    tick.label.len <- length(tick.label)
    tick.label <- tick.label[ifelse(1:ax.num%%tick.label.len==0,tick.label.len,1:ax.num%%tick.label.len)]
    if(is.null(tick.label)){tick.label <- rep("", ax.num)}

    tick.label.col.len <- length(tick.label.col)
    tick.label.col <- tick.label.col[ifelse(1:ax.num%%tick.label.col.len==0,tick.label.col.len,1:ax.num%%tick.label.col.len)]
    if(is.null(tick.label.col)){tick.label.col <- rep(0, ax.num)}

    tick.label.cex.len <- length(tick.label.cex)
    tick.label.cex <- tick.label.cex[ifelse(1:ax.num%%tick.label.cex.len==0,tick.label.cex.len,1:ax.num%%tick.label.cex.len)]
    if(is.null(tick.label.cex)){tick.label.cex <- rep(0, ax.num)}

    tick.label.side.len <- length(tick.label.side)
    tick.label.side <- tick.label.side[ifelse(1:ax.num%%tick.label.side.len==0,tick.label.side.len,1:ax.num%%tick.label.side.len)]
    if(is.null(tick.label.side)){tick.label.side <- rep(0, ax.num)}
    
    predict.col.len <- length(predict.col)
    predict.col <- predict.col[ifelse(1:ax.num%%predict.col.len==0,predict.col.len,1:ax.num%%predict.col.len)]
    if(is.null(predict.col)){predict.col <- rep(0, ax.num)}
    
    predict.lwd.len <- length(predict.lwd)
    predict.lwd <- predict.lwd[ifelse(1:ax.num%%predict.lwd.len==0,predict.lwd.len,1:ax.num%%predict.lwd.len)]
    if(is.null(predict.lwd)){predict.lwd <- rep(0, ax.num)}

    predict.lty.len <- length(predict.lty)
    predict.lty <- predict.lty[ifelse(1:ax.num%%predict.lty.len==0,predict.lty.len,1:ax.num%%predict.lty.len)]
    if(is.null(predict.lty)){predict.lty <- rep(0, ax.num)}
    
    ax.names.len <- length(ax.names)
    ax.names <- ax.names[ifelse(1:p%%ax.names.len==0,ax.names.len,1:p%%ax.names.len)]
    if(is.null(ax.names)){ax.names <- rep("", p)}

    orthogx.len <- length(orthogx)
    orthogx <- orthogx[ifelse(1:p%%orthogx.len==0,orthogx.len,1:p%%orthogx.len)]
    if(is.null(orthogx)){orthogx <- rep("", p)}
    
    orthogy.len <- length(orthogy)
    orthogy <- orthogx[ifelse(1:p%%orthogy.len==0,orthogy.len,1:p%%orthogy.len)]
    if(is.null(orthogy)){orthogy <- rep("", p)}

    bp$axes = list(which = which, col = col, lwd = lwd, lty = lty, label.dir = label.dir, label.col = label.col, label.cex = label.cex,
                   label.line = label.line, ticks = ticks, tick.col = tick.col, tick.size = tick.size, tick.label = tick.label,
                   tick.label.col = tick.label.col, tick.label.cex = tick.label.cex, tick.label.side=tick.label.side,
                   predict.col = predict.col, predict.lty = predict.lty, predict.lwd = predict.lwd,
                   names = ax.names, orthogx = orthogx, orthogy = orthogy,vectors = vectors,
                   unit.circle=unit.circle)
    
  }
  else bp$axes <- list(which = NULL)
  bp
}

# ----------------------------------------------------------------------------------------------
#' Aesthetics for alpha-bags
#'
#' @param g number of groups. Numeric
#' @param g.names names of groups. Character vector
#' @param alpha size of alpha bag. Numeric vector of length >=1
#' @param which which group to select.
#' @param col colour of bags.
#' @param lty line type of bags.
#' @param lwd line width of bags.
#' @param max maximum number of samples to be included in a bag.
#'
#' @noRd
#'
control.alpha.bags <- function (g, g.names, alpha, which=NULL, col, lty, lwd, max, opacity)
{
  if (!all(is.numeric(which))) which <- match(which, g.names, nomatch = 0)
  which <- which[which <= g]
  which <- which[which > 0]

  ww <- length(which)
  if ((length(alpha) > 1) & (length(alpha) != length(which)))
  {
    temp.mat <- expand.grid(which, alpha)
    which <- temp.mat[,1]
    alpha <- temp.mat[,2]
  }
  bag.num <- length(which)

  while (length(alpha) < bag.num) alpha <- c(alpha, alpha)
  alpha <- alpha[1:bag.num]
  if (any(alpha < 0 | alpha > 0.99))
    stop(message = "alpha not to be negative or larger than 0.99")

  if (is.null(col)) col <- ez.col[which]
  col <- stats::na.omit(col)
  while (length(col) < bag.num) col <- c(col, col)
  col <- col[1:bag.num]
  while (length(lty) < bag.num) lty <- rep(lty, each=ww)
  lty <- lty[1:bag.num]
  while (length(lwd) < bag.num) lwd <- rep(lwd, each=ww)
  lwd <- lwd[1:bag.num]
  while (length(opacity) < bag.num) opacity <- c(opacity, opacity)
  opacity <- opacity[1:bag.num]
  
  list(which = which, alpha = alpha, col = col, lty = lty, lwd = lwd, max = max, opacity = opacity)
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
#' @param opacity opacity of sample points, with default 1 (opaque).
#'
#' @noRd
#'
control.concentration.ellipse <- function (g, g.names, df, kappa, which,
                                           col, lty, lwd, opacity)
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
  kappa <- kappa[1:ellipse.num]
  if (is.null(col)) col <- ez.col[which]
  col <- stats::na.omit(col)
  while (length(col) < ellipse.num) col <- c(col, col)
  col <- col[1:ellipse.num]
  while (length(lty) < ellipse.num) lty <- rep(lty, each=ww)
  lty <- lty[1:ellipse.num]
  while (length(lwd) < ellipse.num) lwd <- rep(lwd, each=ww)
  lwd <- as.vector(lwd[1:ellipse.num])
  while (length(opacity) < ellipse.num) opacity <- c(opacity, opacity)
  opacity <- opacity[1:ellipse.num]

  list(which = which, kappa = kappa, col = col, lty = lty, lwd = lwd, opacity = opacity)
}

# ----------------------------------------------------------------------------------------------
#' Format aesthetics for the category level points
#'
#' @description
#' This function allows the user to format the aesthetics for the category level points (CLPs).
#'
#' @param bp an object of class \code{biplot}.
#' @param which a vector containing the columns or variables for which the CLPs should be displayed, with default \code{ncol(Xcat)}.
#' @param col the colour(s) for the CLPs, with default \code{black}.
#' @param cex the character expansion(s) for the CLPs, with default \code{0.6}.
#'
#' @return The object of class \code{biplot} will be appended with a list called \code{CLP.aes} containing the following elements  A list with the following components is available:
#' \item{which}{a vector containing the columns or variables for which the CLPs are displayed.}
#' \item{col}{the colour(s) of the CLPs.}
#' \item{cex}{the character expansion(s) of the plotting characters of the CLPs.}
#'
#' @seealso [biplot()]
#' 
#' @usage
#' CLPs (bp,  which = 1:ncol(bp$Xcat), col = "black", cex = 0.6)
#' @aliases CLPs
#'
#' @export
#'
#' @examples 
#' mtdf <- as.data.frame(mtcars)
#' mtdf$cyl <- factor(mtdf$cyl)
#' mtdf$vs <- factor(mtdf$vs)
#' mtdf$am <- factor(mtdf$am)
#' mtdf$gear <- factor(mtdf$gear)
#' mtdf$carb <- factor(mtdf$carb)
#' biplot(mtdf[,-11], scaled = TRUE) |> AoD(classes = mtdf[,11]) |> 
#' CLPs(col = list(rep("olivedrab",3), rep("orange",2),
#'                 rep("coral",2), rep("brown",3))) |> 
#' plot()
#' 
CLPs <- function (bp,  which = 1:ncol(bp$Xcat), col = "black", cex = 0.6)
{
  p2 <- ncol(bp$Xcat)
  if(is.null(which) & length(col)==0) col <- ez.col
  
  if (!is.null(which))
  {
    if (!all(is.numeric(which))) which <- match(which, bp$colnames(bp$Xcat), nomatch = 0)
    which <- which[which <= p2]
    which <- which[which > 0]
  }

  # Expand col to length p2
  if (!is.list(col)) col <- list(col)
  while (length(col) < length(which)) col[[length(col)+1]] <- col[[1]]
  for (j in 1:length(col))
  {
    col.len <- length(col[[j]])
    num.lev <- nlevels(bp$Xcat[,j])
    col[[j]] <- col[[j]][ifelse(1:num.lev%%col.len==0, col.len, 1:num.lev%%col.len)]
  }

  # Expand cex to length p2
  if (!is.list(cex)) cex <- list(cex)
  while (length(cex) < length(which)) cex[[length(cex)+1]] <- cex[[1]]
  for (j in 1:length(cex))
  {
    cex.len <- length(cex[[j]])
    num.lev <- nlevels(bp$Xcat[,j])
    cex[[j]] <- cex[[j]][ifelse(1:num.lev%%cex.len==0, cex.len, 1:num.lev%%cex.len)]
  }

  bp$CLP.aes = list(which = which, col = col, cex = cex)
  bp
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
#' @param label.name names for the new samples
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
#' label.name = NULL, label.col = NULL,label.cex = 0.75, label.side = "bottom", 
#' label.offset = 0.5, connected = FALSE, connect.col = "black", connect.lty=1, 
#' connect.lwd=1)
#' @aliases newsamples
#'
#' @export
#'
#' @examples
#' biplot(data = iris[1:145,]) |> PCA() |> samples(col = "grey") |>
#' interpolate(newdata = iris[146:150,]) |> newsamples(col = rainbow(6), pch=15) |> plot()
#' 
newsamples <- function (bp,  col = "darkorange1", pch = 1, cex = 1,
                        label = FALSE, label.name = NULL, label.col=NULL, 
                        label.cex = 0.75, label.side = "bottom", 
                        label.offset = 0.5, connected=FALSE, 
                        connect.col = "black", connect.lty = 1, connect.lwd = 1)
{ 
  
  if(!is.null(label.name) | !is.null(label.col) | 
     any(label.side!="bottom") | any(label.offset !=0.5) | any(label.cex!=0.75))
    label<-TRUE
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
                       label.name=label.name, label.cex = label.cex, label.side = label.side, label.offset = label.offset,
                       connected=connected, connect.col = connect.col, connect.lty = connect.lty,
                       connect.lwd = connect.lwd)
  bp
}


#' Aesthetics for supplementary (new) biplot axes
#'
#' @param bp object of class `biplot`
#' @param X.new.names refers to the new column names of \code{bp} to specify which axes to label.
#' @param which vector of new columns to be displayed in the biplot.
#' @inheritParams axes
#'
#' @return an object of class `biplot`
#' @export
#'
#' @usage
#' newaxes(bp, X.new.names=bp$var.names, which = 1:bp$num.vars, col = "orange", lwd = 1, 
#' lty = 1, label.dir = "Orthog", label.col = col, label.cex = 0.75, label.line = 0.1, 
#' ticks = 5, tick.col = col, tick.size = 1, tick.label = TRUE, tick.label.col = tick.col, 
#' tick.label.cex = 0.6, tick.label.side = "below", predict.col = col, predict.lwd = lwd, 
#' predict.lty = lty, ax.names = X.new.names, orthogx = 0, orthogy = 0)
#' @aliases newaxes
#'
#' @examples
#' biplot(data = iris[,1:2]) |> PCA() |> interpolate(newvariable = iris[3:4]) |> 
#'   newaxes(col="gold") |> plot()
newaxes <- function (bp, X.new.names=bp$var.names, which = 1:bp$num.vars, col = "orange", lwd = 1, lty = 1,
                          label.dir = "Orthog", label.col = col, label.cex = 0.75, label.line = 0.1, ticks = 5,
                          tick.col = col, tick.size = 1, tick.label = TRUE, tick.label.col = tick.col, 
                     tick.label.cex = 0.6, tick.label.side = "below",
                          predict.col = col, predict.lwd = lwd, predict.lty = lty, ax.names = X.new.names,
                          orthogx = 0, orthogy = 0)
{
  if (!all(is.numeric(which))) which <- match(which, X.new.names, nomatch = 0)
  p <- bp$num.vars 
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
  #while (length(tick.label.offset) < ax.num) tick.label.offset <- c(tick.label.offset, tick.label.offset)
  #tick.label.offset <- as.vector(tick.label.offset[1:ax.num])
  #while (length(tick.label.pos) < ax.num) tick.label.pos <- c(tick.label.pos, tick.label.pos)
  #tick.label.pos <- as.vector(tick.label.pos[1:ax.num])
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
  bp$newaxes = list(which = which, col = col, lwd = lwd, lty = lty, label.dir = label.dir, label.col = label.col, label.cex = label.cex,
                 label.line = label.line, ticks = ticks, tick.col = tick.col, tick.size = tick.size, tick.label = tick.label,
                 tick.label.col = tick.label.col, tick.label.cex = tick.label.cex, tick.label.side=tick.label.side,
                 predict.col = predict.col, predict.lty = predict.lty, predict.lwd = predict.lwd,
                 names = ax.names, orthogx = orthogx, orthogy = orthogy)
  bp
}

# ----------------------------------------------------------------------------------------------
#' Aesthetics for 2D density control
#'
#' @param g number of groups.
#' @param g.names names of groups.
#' @param which which group.
#' @param contours logical - whether to add contours.
#' @param h vector of bandwidths for x and y directions.
#' @param n number of grid points in each direction. Can be scalar or a length-2 integer vector.
#' @param col vector of colours to use to form a 'continuous' sequence of colours.
#' @param contour.col colour of the contours.
#' @param cuts number of colours in `col`.
#' @param cex character expansion.
#' @param tcl The length of tick marks as a fraction of the height of a line of text. 
#' @param mgp The margin line.
#' @param layout.heights A vector of values for the heights of rows.
#' @param legend.mar The margin line of the legend.
#'
#'@noRd
biplot.density.2D.control <- function(g, g.names, which, contours = F, h = NULL, n = 100, 
                                      col = c("green", "yellow", "red"), contour.col = "black", cuts = 50, cex = 0.6, 
                                      tcl = -0.2, mgp = c(0, -0.25, 0), layout.heights = c(100, 10), legend.mar = c(2, 5, 0, 5)) 
{
  if (!is.null(which))
    if (which == "all") which <- 0
  else if (!all(is.numeric(which))) which <- match(which, g.names, nomatch = 0)
  which <- which[which <= g]
  which <- which[which >= 0]
  if (!is.null(which)) which <- which[1]
  list(which = which, contours = contours, h = h, n = n, col = col, contour.col = contour.col,
       cuts = cuts, cex = cex, tcl = tcl, mgp = mgp, layout.heights = layout.heights, legend.mar = legend.mar)
}



#' Spline Axis Control
#'
#' @param tau Value of tau
#' @param nmu Value of nmu
#' @param u Value of u
#' @param v Value of v
#' @param lambda Value of lambda
#' @param smallsigma Value of small sigma
#' @param bigsigma Value of big sigma
#' @param gamma Value of gamma
#' @param bigsigmaactivate Value of big sigma activate
#' @param eps Value of eps
#' @param tiny Value of tiny
#' @param itmax Value of itmax
#' @param ftol Value of ftol
#'
#' @noRd
biplot.spline.axis.control <- function (tau = 0.5, nmu = 100, u = 2, v = 3, lambda = 0, smallsigma = 0.01, 
                                        bigsigma = smallsigma * 10, gamma = 250, bigsigmaactivate = floor(gamma * 0.1), 
                                        eps = 0.01, tiny = 10^-30, itmax = 10000, ftol = 1.5 * (.Machine$double.eps)) 
{
  if (!is.numeric(tau) || tau <= 0) stop("value of 'tau' must be > 0")
  if (!is.numeric(nmu) || nmu <= 0) stop("value of 'nmu' must be > 0")
  if (!is.numeric(u) || u < 0) stop("value of 'u' must not be less than zero")
  if (!is.numeric(v) || v < 0) stop("value of 'v' must not be less than zero")
  if (!is.numeric(lambda) || lambda < 0) stop("value of 'lambda' must be >= 0")
  if (!is.numeric(smallsigma) || smallsigma <= 0) stop("value of 'smallsigma' must be > 0")
  if (!is.numeric(bigsigma) || bigsigma <= 0) stop("value of 'bigsigma' must be > 0")
  if (!is.numeric(gamma) || gamma <= 0) stop("value of 'gamma' must be > 0")
  if (!is.numeric(bigsigmaactivate) || bigsigmaactivate <= 0) stop("value of 'bigsigmaactivate' must be > 0")
  if (!is.numeric(eps) || eps < 0) stop("value of 'eps' must be > 0")
  if (!is.numeric(tiny) || tiny < 0) stop("value of 'tiny' must be > 0")
  if (!is.numeric(itmax) || itmax <= 0) stop("itmax number of iterations must be > 0")
  if (!is.numeric(ftol) || ftol <= 0) stop("value of 'ftol' must be > 0")
  list(tau = tau, nmu = nmu, u = u, v = v, lambda = lambda, smallsigma = smallsigma, bigsigma = bigsigma, 
       gamma = gamma, bigsigmaactivate = bigsigmaactivate, eps = eps, tiny = tiny, itmax = itmax, ftol = ftol)
}


