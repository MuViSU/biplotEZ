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
#' @param data a dataframe or matrix containing all variables the user wants to analyse.
#' @param classes vector identifying class membership.
#' @param group.aes vector identifying groups for aesthetic formatting.
#' @param center logical, indicating whether {data} should be column centered, with default \code{TRUE}.
#' @param scaled logical, indicating whether {data} should be standardized to unit column variances, with default \code{FALSE}.
#' @param Title title of the biplot to be rendered, enter text in "  ".
#'
#' @return A list with the following components is available:
#' \item{X}{matrix of the centered and scaled numeric variables.}
#' \item{Xcat}{data frame of the categorical variables.}
#' \item{raw.X}{original data.}
#' \item{center}{TRUE or FALSE, whether X is centered.}
#' \item{scaled}{TRUE or FALSE, wether X is scaled.}
#' \item{means}{vector of means for each numeric variable.}
#' \item{sd}{vector of standard deviations for each numeric variable.}
#' \item{group.aes}{vector of category levels for the grouping variable. This is to be used for colour, pch and cex specifications.}
#' \item{Title}{title of the biplot to be rendered}
#'
#' @references
#' Gabriel, K.R. (1971) The biplot graphic display of matrices with application to principal component analysis. \emph{Biometrika.} 58(3):453–467.<br><br>
#' Gower, J., Gardner-Lubbe, S. & Le Roux, N. (2011, ISBN: 978-0-470-01255-0) \emph{Understanding Biplots.} Chichester, England: John Wiley & Sons Ltd.<br><br>
#' Gower, J.C. & Hand, D.J.(1996, ISBN: 0-412-71630-5) \emph{Biplots.} London: Chapman & Hall.
#'
#' @usage biplot(data, classes = NULL, group.aes = NULL, center = TRUE, scaled = FALSE,
#' Title = NULL)
#' @aliases biplot
#'
#' @export
#'
#' @examples
#' biplot(data = iris)
#' # create a PCA biplot
#' biplot(data = iris) |> PCA() |> plot()
biplot <- function(data, classes = NULL, group.aes = NULL, center = TRUE,
                   scaled = FALSE, Title = NULL)
{
  # make provision for an object of class prcomp or princomp
  if ((inherits(data, "prcomp")) | (inherits(data, "princomp")))
  {
    if (inherits(data, "princomp"))
    {
      if (is.null(data$scores)) stop ("Your need to specify scores=TRUE.")
       X <- data$scores %*% t(data$loadings)
       n <- nrow(X)
       p <- ncol(X)
       if (all(data$scale[1]==1)) scaled <- FALSE else scaled <- TRUE
       sd <- data$scale
       means <- data$center
       raw.X <- scale(scale(X, center=F, scale=1/sd), center=-1*means, scale=F)
       Vr <- data$loadings[,1:2]
       ax.one.unit <- 1/(diag(Vr %*% t(Vr))) * Vr
       Z <- data$scores[,1:2]
       Xhat <- Z %*% t(Vr)
       if (scaled) Xhat <- scale(Xhat, center=FALSE, scale=1/sd)
       if (center) Xhat <- scale(Xhat, center=-1*means, scale=FALSE)
    }

    if (inherits(data, "prcomp"))
    {  if (is.null(data$x)) stop ("You need to specify retx=TRUE.")
       if (ncol(data$rotation)<2) stop ("rank needs to be at least 2")
       X <- data$x %*% t(data$rotation)
       n <- nrow(X)
       p <- ncol(X)
       if (!data$scale[1]) { scaled <- FALSE
                             sd <- rep(1, p)
                             raw.X <- X           }
      else { raw.X <- scale(X, center=F, scale=1/data$scale)
             scaled <- TRUE
             sd <- data$scale                     }
    if (!data$center[1]) { center <- FALSE
                           means <- rep(0, p)     }
    else { raw.X <- scale(raw.X, center=-1*data$center, scale=F)
           center <- TRUE
           means <- data$center                   }
     Vr <- data$rotation[,1:2]
     ax.one.unit <- 1/(diag(Vr %*% t(Vr))) * Vr
     Z <- data$x[,1:2]
     Xhat <- Z %*% t(Vr)
     if (scaled) Xhat <- scale(Xhat, center=FALSE, scale=1/sd)
     if (center) Xhat <- scale(Xhat, center=-1*means, scale=FALSE)
    }
    na.vec.df <- NULL

    if(is.null(group.aes)) group.aes <- factor(rep(1,n))
    else group.aes <- factor(group.aes)
    g.names <-levels(group.aes)
    g <- length(g.names)

   object <- list(X = X, Xcat = NULL, raw.X = raw.X, na.action=na.vec.df, center=center, scaled=scaled,
                   means = means, sd = sd, n=nrow(X), p=ncol(X), group.aes = group.aes, g.names = g.names,g = g,
                   Title = Title, Z=Z, Vr=Vr, ax.one.unit=ax.one.unit, Xhat=Xhat)
    class(object) <- "biplot"
    class(object)<-append(class(object),"PCA")
  }
  else
  {
    dim.mat<-dim(data)
    if(is.null(dim.mat)) stop("Not enough variables to construct a biplot \n Consider using data with more columns")
    if(ncol(data)<2) stop("Not enough variables to construct a biplot \n Consider using data with more columns")

    # check for missing values
    na.vec.df <- stats::na.action(stats::na.omit(data))
    if (length(na.vec.df) == nrow(data)) stop("No observations left after deleting missing observations")
    else if (!is.null(na.vec.df))  warning(paste(length(na.vec.df), "rows deleted due to missing values"))
    data<-data[stats::complete.cases(data),]
    if (!is.null(group.aes) & length(na.vec.df) > 0) group.aes <- group.aes[-na.vec.df]

    # Separating numeric and categorical data
    if (is.matrix(data))
    {
      X <- data
      Xcat <- NULL
    }
    else
    {
      type.vec <- unlist(lapply(data, is.numeric), use.names = FALSE)
      if (sum(type.vec)>0) X <- as.matrix(data[, type.vec, drop=FALSE])
      else X <- NULL
      if (sum(type.vec)<length(type.vec)) Xcat <- as.data.frame(data[, !type.vec, drop=FALSE])
      else Xcat <- NULL
    }

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

    if(!is.null(classes))
      classes <- factor(classes)

    if(is.null(group.aes)) group.aes <- factor(rep(1,nrow(data)))
    else group.aes <- factor(group.aes)
    g.names <-levels(group.aes)
    g <- length(g.names)

    object <- list(X = X, Xcat = Xcat, raw.X = data, classes=classes, na.action=na.vec.df, center=center, scaled=scaled,
                   means = means, sd = sd, n=nrow(X), p=ncol(X), group.aes = group.aes,g.names = g.names,g = g,
                   Title = Title)
    class(object) <- "biplot"
  }

  object
}

# ---------------------------------------------------------------------------------------------

ez.col <- c("blue","green","gold","cyan","magenta","black","red","grey","purple","salmon")

# ----------------------------------------------------------------------------------------------

#'Legend type
#'
#' @description
#' This function enables the user to format the legend and make a required selection to display.
#'
#' @usage legend.type(bp, samples = FALSE, means = FALSE, bags = FALSE, ellipses = FALSE,
#' new = FALSE, ...)
#' @aliases legend.type
#'
#' @param bp an object of class \code{biplot}.
#' @param samples logical, indicating whether legend should be printed for samples, with default \code{FALSE}.
#' @param means logical, indicating whether legend should be printed for means, with default \code{FLASE}.
#' @param bags logical, indicating whether legend should be printed for bags, with default \code{FALSE}.
#' @param ellipses logical, indicating whether legend should be printed for concentration ellipses, with default \code{FALSE}.
#' @param new logical, indicating whether the legend should appear in a new window, with default \code{FALSE}.
#' @param ... additional arguments to be sent to \code{legend()}.
#'
#' @return A list with the following components is available:
#' \item{samples}{TRUE or FALSE, whether legend for samples are provided.}
#' \item{means}{TRUE or FALSE, whether legend for class means are provided.}
#' \item{bags}{TRUE or FALSE, whether legend for alpha-bags are provided.}
#' \item{ellipses}{TRUE or FALSE, whether legend for kappa-ellipses are provided.}
#' \item{new}{TRUE or FALSE, whether legend appears on new plot.}
#'
#' @export
#' @examples
#' biplot (iris[,1:4], Title="Test biplot") |> PCA(group.aes = iris[,5]) |>
#'     legend.type(samples=TRUE) |> plot()
legend.type <- function (bp, samples = FALSE, means = FALSE, bags = FALSE, ellipses=FALSE, new=FALSE, ...)
{
  bp$legend <- list(samples=samples, means=means, bags=bags,ellipses = ellipses, new=new)
  bp$legend.arglist <- list(...)
  bp
}

# ----------------------------------------------------------------------------------------------
#' Constructs the biplot legend
#'
#' @param bp an object of class \code{biplot}.
#' @param ... more arguments to be sent to `legend`.
#'
#' @noRd
biplot.legend <- function(bp, ...)
{
  legend.type <- c(bp$legend$samples, bp$legend$means, bp$legend$bags, bp$legend$ellipses)

  if (all(legend.type == FALSE)) stop("Change at least one of samples, means, bags or ellipses to TRUE to obtain a legend")

  if(bp$legend$new)
  {
    plot(x = c(0, 10), y = c(0, 10), type = "n", axes = FALSE, xlab = "", ylab = "", xaxs = "i", yaxs = "i")
    usr <- graphics::par("usr")
    x <- usr[1]
    y <- usr[4]
  }

  if(bp$legend$bags & !is.null(bp$alpha.bags))
  { #formatting of legend names
    graphics::legend("topleft", col = bp$alpha.bag.aes$col, lty = bp$alpha.bag.aes$lty,
                     lwd = bp$alpha.bag.aes$lwd, legend = names(bp$alpha.bags),
                     ...)
  }

  if(bp$legend$means) graphics::legend("bottomright",col=bp$means$col,pch=bp$means$pch,legend=bp$classes, ...)

  if(bp$legend$samples) graphics::legend("topright", col=bp$samples$col[1:length(bp$samples$which)],
                                                     pch=bp$samples$pch[1:length(bp$samples$which)],
                                                     legend=bp$g.names[bp$samples$which], ...)

  if(bp$legend$ellipses & !is.null(bp$conc.ellipses))
  { #formatting of legend names
    graphics::legend("bottomleft",col=bp$conc.ellipse.aes$col,lty=bp$conc.ellipse.aes$lty,lwd=bp$conc.ellipse.aes$lwd,
                     legend=names(bp$conc.ellipses), ...)
  }

}

# ----------------------------------------------------------------------------------------------
#' Generic print function of objects of class biplot
#'
#' @param x an object of class \code{biplot}.
#' @param ... additional arguments.
#'
#' @return no return value, called for side effects.
#'
#' @export
#' @examples
#' out <- biplot (iris[,1:4]) |> PCA() |> plot()
#' out

print.biplot <- function (x, ...)
{
  cat ("Object of class biplot, based on", x$n, "samples and", ncol(x$raw.X), "variables.\n")
  if (!is.null(x$X)) if (ncol(x$X) > 1) cat (ncol(x$X), "numeric variables.\n") else cat (ncol(x$X), "numeric variable.\n")
  if (!is.null(x$Xcat)) if (ncol(x$Xcat) > 1) cat (ncol(x$Xcat), "categorical variables.\n") else cat (ncol(x$Xcat), "categorical variable.\n")
  if (!is.null(x$na.action))
    cat ("The following", length(x$na.action), "sample-rows where removed due to missing values\n", x$na.action, "\n")
  if (!is.null(x$classes))
    cat (nlevels(x$classes), "classes:", levels(x$classes), "\n")
}

