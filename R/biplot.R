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
#' @param center logical, indicating whether \code{data} should be column centered, with default \code{TRUE}.
#' @param scaled logical, indicating whether \code{data} should be standardized to unit column variances, with default \code{FALSE}.
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
       Lmat <- data$loadings
       ax.one.unit <- 1/(diag(Vr %*% t(Vr))) * Vr
       e.vects <- 1:2
       Z <- data$scores[,1:2]
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
     Lmat <- data$rotation
     e.vects <- 1:2
     ax.one.unit <- 1/(diag(Vr %*% t(Vr))) * Vr
     Z <- data$x[,1:2]
    }
    na.vec.df <- NULL

    if(is.null(group.aes)) group.aes <- factor(rep(1,n))
    else group.aes <- factor(group.aes)
    g.names <-levels(group.aes)
    g <- length(g.names)

   object <- list(X = X, Xcat = NULL, raw.X = raw.X, na.action=na.vec.df, center=center, scaled=scaled,
                   means = means, sd = sd, n=nrow(X), p=ncol(X), group.aes = group.aes, g.names = g.names,g = g,
                   Title = Title, Z=Z, Lmat=Lmat, e.vects=e.vects, ax.one.unit=ax.one.unit)
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

#' Compute measures of fit for biplot.
#'
#' @param bp an object of class \code{biplot}.
#'
#' @return An object of class \code{biplot}. The object is augmented with
#'         additional items, depending on the type of biplot object.
#' \item{quality}{overall quality of fit.}
#' \item{adequacy}{adequacy of representation of variables.}
#'
#' For an object of class \code{PCA}:
#' \item{axis.predictivity}{fit measure of each individual axis.}
#' \item{sample.predictivity}{fit measure for each individual sample.}

#' For an object of class \code{CVA}:
#' \item{axis.predictivity}{fit measure of each individual axis.}
#' \item{class.predictivity}{fit measure for each class mean.}
#' \item{within.class.axis.predictivity}{fit measure for each axis based on values
#'                                       expressed as deviations from their class means.}
#' \item{within.class.sample.predictivity}{fit measure for each sample expressed as
#'                                         deviation from its class mean.}
#' @export
#'
#' @examples
#' out <- biplot (iris[,1:4]) |> PCA() |> fit.measures()
#' summary(out)
fit.measures <- function (bp)
{
  if (inherits(bp, "PCA"))
  {
    bp$quality <- sum(bp$eigenvalues[bp$e.vects])/sum(bp$eigenvalues)
    bp$adequacy <- apply(bp$Lmat[,bp$e.vects],1,function(x)sum(x^2))
    names(bp$adequacy) <- colnames(bp$X)
    Xhat <- bp$Z %*% t(bp$Lmat[,bp$e.vects])
    bp$axis.predictivity <- diag(t(Xhat)%*%Xhat)/diag(t(bp$X)%*%bp$X)
    names(bp$axis.predictivity) <- colnames(bp$X)
    bp$sample.predictivity <- diag(Xhat%*%t(Xhat))/diag(bp$X%*%t(bp$X))
    names(bp$sample.predictivity) <- rownames(bp$X)
  }
  if (inherits(bp, "CVA"))
  {
    Xbar.hat <- bp$Zmeans %*% solve(bp$Lmat)[bp$e.vects,,drop=F]
    svd.out <- svd(bp$Cmat)
    C.half <- svd.out$u %*% diag(sqrt(svd.out$d)) %*% t(svd.out$v)
    G <- bp$Gmat
    Xwithin <- (diag(bp$n)-G%*%solve(t(G)%*%G)%*%t(G))%*%bp$X
    Xwithin.hat <- Xwithin %*% bp$Lmat[,bp$e.vects,drop=F] %*% solve(bp$Lmat)[bp$e.vects,,drop=F]

    bp$quality <- list ("canonical variables" =
                          sum(bp$eigenvalues[bp$e.vects])/sum(bp$eigenvalues),
                        "original variables" =
                          sum(diag(t(Xbar.hat)%*%bp$Cmat%*%Xbar.hat))/
                          sum(diag(t(bp$Xmeans)%*%bp$Cmat%*%bp$Xmeans)))
    bp$adequacy <- apply(bp$Lmat[,bp$e.vects,drop=F],1,function(x)sum(x^2)) /
                             diag(bp$Lmat%*%t(bp$Lmat))
    names(bp$adequacy) <- colnames(bp$X)
    bp$axis.predictivity <- diag(t(Xbar.hat)%*%bp$Cmat%*%Xbar.hat)/
                            diag(t(bp$Xmeans)%*%bp$Cmat%*%bp$Xmeans)
    names(bp$axis.predictivity) <- colnames(bp$X)
    bp$class.predictivity <- diag(C.half%*%Xbar.hat%*%solve(bp$Wmat)%*%t(Xbar.hat)%*%C.half)/
                             diag(C.half%*%bp$Xmeans%*%solve(bp$Wmat)%*%t(bp$Xmeans)%*%C.half)
    names(bp$class.predictivity) <- bp$g.names
    bp$within.class.axis.predictivity <- diag(t(Xwithin.hat)%*%Xwithin.hat)/
                                         diag(t(Xwithin)%*%Xwithin)
    names(bp$within.class.axis.predictivity) <- colnames(bp$X)
    bp$within.class.sample.predictivity <- diag(Xwithin.hat%*%solve(bp$Wmat)%*%t(Xwithin.hat))/
                                           diag(Xwithin%*%solve(bp$Wmat)%*%t(Xwithin))
    names(bp$within.class.sample.predictivity) <- rownames(bp$X)
  }
  bp
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

  if(bp$legend$means) graphics::legend("bottomright",col=bp$means.aes$col,pch=bp$means.aes$pch,
                                       legend=bp$g.names[bp$means.aes$which], ...)

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
#' out <- biplot (iris[,1:4]) |> PCA()
#' out

print.biplot <- function (x, ...)
{
  cat ("Object of class biplot, based on", x$n, "samples and", ncol(x$raw.X), "variables.\n")
  if (!is.null(x$X)) if (ncol(x$X) > 1) cat (ncol(x$X), "numeric variables.\n") else cat (ncol(x$X), "numeric variable.\n")
  if (!is.null(x$Xcat)) if (ncol(x$Xcat) > 1) cat (ncol(x$Xcat), "categorical variables.\n") else cat (ncol(x$Xcat), "categorical variable.\n")
  if (!is.null(x$classes))
    cat (nlevels(x$classes), "classes:", levels(x$classes), "\n")

  if (!is.null(x$na.action))
    cat ("\nThe following", length(x$na.action), "sample-rows where removed due to missing values\n", x$na.action, "\n")

  if (!is.null(x$quality))
    if (!is.list(x$quality))
      cat ("\nQuality of fit =", paste(round(x$quality*100,1),"%",sep=""), "\n")
  else
  {
    cat("\n")
    for (i in 1:length(x$quality))
      cat ("Quality of fit of", names(x$quality)[i], "=",
           paste(round(x$quality[[i]]*100,1),"%",sep=""), "\n")
  }
}

# ----------------------------------------------------------------------------------------------
#' Generic summary function of objects of class biplot
#'
#' @param object an object of class \code{biplot}.
#' @param adequacy logical, whether variable adequacies should be reported.
#' @param axis.predictivity logical, whether axis predictivities should be reported.
#' @param sample.predictivity logical, whether sample predictivities should be reported.
#' @param class.predictivity logical, whether class predictivities should be reported
#'                           (only applicable to objects of class \code{CVA}).
#' @param within.class.axis.predictivity logical, whether within class axis predictivity
#'                                       should be reported(only applicable to objects
#'                                       of class \code{CVA}).
#' @param within.class.sample.predictivity logical, whether within class sample predictivity
#'                                       should be reported(only applicable to objects
#'                                       of class \code{CVA}).
#' @param ... additional arguments.
#'
#' @return no return value, called for side effects.
#'
#' @export
#' @examples
#' out <- biplot (iris[,1:4]) |> PCA() |> fit.measures()
#' summary(out)

summary.biplot <- function (object, adequacy = TRUE, axis.predictivity = TRUE,
                            sample.predictivity = TRUE, class.predictivity = TRUE,
                            within.class.axis.predictivity = TRUE,
                            within.class.sample.predictivity = TRUE, ...)
{
  cat ("Object of class biplot, based on", object$n, "samples and", ncol(object$raw.X), "variables.\n")
  if (!is.null(object$X)) if (ncol(object$X) > 1) cat (ncol(object$X), "numeric variables.\n") else cat (ncol(object$X), "numeric variable.\n")
  if (!is.null(object$Xcat)) if (ncol(object$Xcat) > 1) cat (ncol(object$Xcat), "categorical variables.\n") else cat (ncol(object$Xcat), "categorical variable.\n")
  if (!is.null(object$classes))
    cat (nlevels(object$classes), "classes:", levels(object$classes), "\n")

  if (!is.null(object$na.action))
    cat ("\nThe following", length(object$na.action), "sample-rows where removed due to missing values\n", object$na.action, "\n")

  if (!is.null(object$quality))
    if (!is.list(object$quality))
      cat ("\nQuality of fit =", paste(round(object$quality*100,1),"%",sep=""), "\n")
  else
  {
    cat("\n")
    for (i in 1:length(object$quality))
      cat ("Quality of fit of", names(object$quality)[i], "=",
           paste(round(object$quality[[i]]*100,1),"%",sep=""), "\n")
  }
  if (!is.null(object$adequacy) & adequacy)
  {
    cat ("Adequacy of variables:\n")
    print (object$adequacy)
  }
  if (!is.null(object$axis.predictivity) & axis.predictivity)
  {
    cat ("Axis predictivity:\n")
    print (object$axis.predictivity)
  }
  if (!is.null(object$sample.predictivity) & sample.predictivity)
  {
    cat ("Sample predictivity:\n")
    print (object$sample.predictivity)
  }
  if (!is.null(object$class.predictivity) & class.predictivity)
  {
    cat ("Class predictivity:\n")
    print (object$class.predictivity)
  }
  if (!is.null(object$within.class.axis.predictivity) & within.class.axis.predictivity)
  {
    cat ("Within class axis predictivity:\n")
    print (object$within.class.axis.predictivity)
  }
  if (!is.null(object$within.class.sample.predictivity) & within.class.sample.predictivity)
  {
    cat ("Within class sample predictivity:\n")
    print (object$within.class.sample.predictivity)
  }
}

# -----------------------------------------------------------------------------------------------------

#' Interpolation of new samples
#'
#' @param bp an object of class \code{biplot} obtained from preceding function \code{biplot()}.
#' @param newdata a new data set, similar in structure to the data set supplied to \code{biplot()}
#'                containing supplementary data points to be added on the biplot.
#'
#' @return Object of class PCA with the following elements:
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
#' \item{group.aes}{vector of the same length as the number of rows in the data matrix for differentiated aesthetics for samples.}
#' \item{g.names}{descriptive name to be used for group labels.}
#' \item{g}{number of groups.}
#' \item{Title}{title of the biplot to be rendered}
#' \item{Z}{matrix with each row containing the details of the point to be plotted (i.e. coordinates).}
#' \item{Lmat}{matrix for transformation to the principal components.}
#' \item{e.vects}{vector indicating which principal components are plotted in the biplot.}
#' \item{ax.one.unit}{one unit in the positive direction of each biplot axis.}
#' \item{Xnew.raw}{ newdata numerical variables.}
#' \item{Xnew}{matrix of the centered and scaled new numeric variables.}
#' \item{Xnew.cat}{matrix of the new categorical variables.}
#' \item{Znew}{matrix of the coordinates of the newdata in the biplot.}
#'
#' @export
#'
#' @examples
#' biplot(data = iris[1:145,]) |> PCA() |> interpolate(newdata = iris[146:150,]) |> plot()
#'
interpolate <- function (bp, newdata)
{
  dim.mat<-dim(newdata)
  if(is.null(dim.mat)) stop("Not enough variables to interpolate.")
  if(ncol(newdata)<2) stop("Not enough variables to interpolate.")

  # check for missing values
  na.vec.df <- stats::na.action(stats::na.omit(newdata))
  if (length(na.vec.df) == nrow(newdata))
    stop("No observations left after deleting missing observations")
  else if (!is.null(na.vec.df))
    warning(paste(length(na.vec.df), "rows deleted due to missing values"))
  newdata <- newdata[stats::complete.cases(newdata),]

  # Separating numeric and categorical data
  if (is.matrix(newdata))
  {
    Xnew <- newdata
    Xcat.new <- NULL
  }
  else
  {
    type.vec <- unlist(lapply(newdata, is.numeric), use.names = FALSE)
    if (sum(type.vec)>0) Xnew <- as.matrix(newdata[, type.vec, drop=FALSE])
    else Xnew <- NULL
    if (sum(type.vec)<length(type.vec)) Xnew.cat <- as.data.frame(newdata[, !type.vec, drop=FALSE])
    else Xnew.cat <- NULL
  }

  Xnew.raw <- Xnew
  if (!is.null(Xnew)) Xnew <- scale(Xnew, bp$means, bp$sd)
  if (!is.null(Xnew))
    if (is.null(rownames(Xnew))) rownames(Xnew) <- paste("new",1:nrow(Xnew))

  if(!is.null(Xnew.cat))
    if (is.null(rownames(Xnew.cat))) rownames(Xnew.cat) <- paste("new",1:nrow(Xnew.cat))

  if (inherits(bp, "PCA") | inherits(bp, "CVA"))
  {
    Znew <- Xnew %*% bp$Lmat[,bp$e.vects]
    if (nrow(Xnew)==1) Znew <- matrix(Znew, nrow=1)
    rownames(Znew) <- rownames(Xnew)
  }

  bp$Xnew.raw <- Xnew.raw
  bp$Xnew <- Xnew
  bp$Xnew.cat <- Xnew.cat
  bp$Znew <- Znew
  bp
}

# -----------------------------------------------------------------------------------------------------

#' Prediction of samples
#'
#' @param bp an object of class \code{biplot} obtained from preceding function \code{biplot()}.
#' @param predict.samples a vector specifying which samples to predict.
#' @param predict.means a vector specifying which group means to predict.
#'
#' @return Object of class PCA with the following elements:
#' \item{predict.samples}{which samples are predicted.}
#' \item{predict.mat}{matrix of predicted samples.}
#' \item{predict.means}{which group means are predicted.}
#' \item{predict.means.mat}{matrix of predicted group means.}
#'
#'
#' @export
#'
#' @examples
#' biplot(data = iris[,1:4]) |> PCA(group.aes=iris[,5]) |> prediction(1:145,1:3) |> plot()
#'
prediction <- function (bp, predict.samples=NULL,predict.means=NULL)
{
  Z <- bp$Z
  means <- bp$means
  sd <- bp$sd
  Zmeans <- bp$Zmeans
  
  if (inherits(bp, "PCA"))
  {
    Vr <- bp$Vr
    if (!is.null(predict.samples)) {
      predict.mat <- scale(Z[predict.samples, , drop = F] %*% t(Vr), center = F, scale = 1 / sd)
    } else predict.mat <- NULL
    if (!is.null(predict.mat))
      predict.mat <- scale(predict.mat, center = -means, scale = F)
    if (!is.null(predict.means)) {
      predict.means.mat <- scale(Zmeans[predict.means, , drop = F] %*% t(Vr),center = F, scale = 1 / sd)
    } else
      predict.means.mat <- NULL
    if (!is.null(predict.means.mat))
      predict.mat <- rbind(predict.mat, scale(predict.means.mat, center = -means, scale = F))
    if (!is.null(predict.mat)) 
      dimnames(predict.mat) <- list(c(dimnames(bp$raw.X)[[1]][predict.samples], bp$g.names[predict.means]), dimnames(bp$raw.X)[[2]])
    
    bp$predict.samples <- predict.samples
    bp$predict.means <- predict.means
    bp$predict.mat <- predict.mat
    bp$predict.means.mat <- predict.means.mat
  }
  
   if (inherits(bp, "CVA"))
   {
     Mrr <- bp$Mrr
     
     if (!is.null(predict.samples)) 
       predict.mat <- scale(Z[predict.samples, , drop = F] %*% Mrr, center = -means, scale = F) else predict.mat <- NULL
       if (!is.null(predict.means)) 
         predict.mat <- rbind(predict.mat, scale(Zmeans[predict.means, , drop = F] %*% Mrr, center = -means, scale = F))
       if (!is.null(predict.mat)) 
         dimnames(predict.mat) <- list(c(dimnames(bp$raw.X)[[1]][predict.samples], bp$g.names[predict.means]), dimnames(bp$raw.X)[[2]])
       
       bp$predict.samples <- predict.samples
       bp$predict.means <- predict.means
       bp$predict.mat <- predict.mat

   }
  
  bp
  
}



