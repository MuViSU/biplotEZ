### MuViSU (Centre for Multi-Dimensional Data Visualisation)
### biplotEZ functions
### ========================================================

.onAttach <- function(libname,pkgname){
  packageStartupMessage("Welcome to biplotEZ! \nThis package is used to construct biplots \nRun ?biplot or vignette() for more information")
}

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
#' @param data a data frame or numeric matrix containing all variables the user wants to analyse.
#' @param classes a vector identifying class membership.
#' @param group.aes a vector identifying groups for aesthetic formatting.
#' @param center a logical value indicating whether \code{data} should be column centered, with default \code{TRUE}.
#' @param scaled a logical value indicating whether \code{data} should be standardised to unit column variances, with default \code{FALSE}.
#' @param Title the title of the biplot to be rendered, enter text in "  ".
#'
#' @details
#'This function is the entry-level function in \code{biplotEZ} to construct a biplot display.
#'It initialises an object of class \code{biplot} which can then be piped to various other functions
#'to build up the biplot display.
#'
#' @return A list with the following components is available:
#' \item{X}{the matrix of the centered and scaled numeric variables.}
#' \item{Xcat}{the data frame of the categorical variables.}
#' \item{raw.X}{the original data.}
#' \item{classes}{the vector of category levels for the class variable. This is to be used for \code{colour}, \code{pch} and \code{cex} specifications.}
#' \item{na.action}{the vector of observations that have been removed.}
#' \item{center}{a logical value indicating whether \eqn{\mathbf{X}} is centered.}
#' \item{scaled}{a logical value indicating whether \eqn{\mathbf{X}} is scaled.}
#' \item{means}{the vector of means for each numeric variable.}
#' \item{sd}{the vector of standard deviations for each numeric variable.}
#' \item{n}{the number of observations.}
#' \item{p}{the number of variables.}
#' \item{group.aes}{the vector of category levels for the grouping variable. This is to be used for \code{colour}, \code{pch} and \code{cex} specifications.}
#' \item{g.names}{the descriptive names to be used for group labels.}
#' \item{g}{the number of groups.}
#' \item{Title}{the title of the biplot rendered}
#'
#' @section Useful links: 
#' 
#' The biplot display can be built up in four broad steps depending on the needs for the display. 
#' Firstly, choose an appropriate method to construct the display;
#' Secondly, change the aesthetics of the display; 
#' Thirdly, append the display with supplementary features such as axes, samples and means;
#' Finally, superimpose shapes, characters or elements onto the display.
#'
#' \strong{1. Different types of biplots:}
#' * [PCA()]: Principal Component Analysis biplot of various dimensions
#' * [CVA()]: Canonical Variate Analysis biplot
#' * [PCO()]: Principal Coordinate Analysis biplot
#' * [CA()]: Correspondence Analysis biplot
#' * [regress()]: Regression biplot method
#'
#' \strong{2. Customise the biplot display with aesthetic functions:}
#' * [samples()]: Change the formatting of sample points on the biplot display
#' * [axes()]: Change the formatting of the biplot axes
#'
#' \strong{3. Supplement the existing biplot with additional axes, samples and group means:}
#' * [newsamples()]: Add and change formatting of additional samples
#' * [newaxes()]: Add and change formatting of additional axes
#' * [means()]: Insert class means to the display, and format appropriately
#' 
#' \strong{4. Append the biplot display:}
#' * [alpha.bags()]: Add \eqn{\alpha}-bags
#' * [ellipses()]: Add ellipses
#' * [density2D()]: Add 2D density regions
#' 
#' \strong{Other useful links:}
#'  * [plot()]
#'  * [fit.measures()]
#'  * [legend.type()]
#'  * [interpolate()]
#'  * [prediction()]
#'  * [classify()]
#'  * [reflect()]
#'  * [rotate()]
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
#' 
biplot <- function(data, classes = NULL, group.aes = NULL, center = TRUE,
                   scaled = FALSE, Title = NULL)
{
  # make provision for an object of class prcomp or princomp
  if ((inherits(data, "prcomp")) | (inherits(data, "princomp") | (inherits(data,"PCA") | (inherits(data, "dudi")))))
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

    if (inherits(data, "PCA")){
      if(data$call$ncp == 1)
        stop ("Rank needs to be at least 2. Change ncp paramater")
      raw.X<-data$call$X
      qual.sup.variable<-NULL
      quant.sup.variable <- NULL
      if(!is.null(data$quali.sup))
        qual.sup.variable<-data$call$quali.sup$numero
      if(!is.null(data$quanti.sup)){#-----------------Needs attention
        used<-rownames(data$var$coord)
        all<-colnames(raw.X)[-qual.sup.variable]
        index<- which (!(all %in% used))
        quant.sup.variable <- index
      }
      if(is.null(qual.sup.variable) & is.null(quant.sup.variable))
        X <- raw.X
      else if(is.null(qual.sup.variable))
        X <- raw.X[,-quant.sup.variable]
      else if(is.null(quant.sup.variable))
        X<- raw.X[,-qual.sup.variable]
      else
        X<- raw.X[,-c(quant.sup.variable,qual.sup.variable)]
      means<-data$call$centre
      center<-TRUE
      n <- nrow(X)
      p <- ncol(X)
      if(data$call$scale.unit){
        scaled <- TRUE
        sd <- apply(X, 2, sd)
      }
      else{
        scaled <- FALSE
        sd <- rep(1,p)
      }
      X <- scale(X, scale=scaled)
      Vr <- data$svd$V[,1:2]
      Lmat <- data$svd$V
      e.vects <- 1:2
      ax.one.unit <- 1/(diag(Vr %*% t(Vr))) * Vr
      if (is.null(rownames(X))) rownames(X) <- paste(1:nrow(X))
      Z <- X %*% Vr
      if (is.null(rownames(Z))) rownames(Z) <- paste(1:nrow(Z))

    }

    if (inherits(data, "dudi")){
      cent<- data$cent
      norm<- data$norm
      raw.X<- sweep(data$tab,2,norm,"*") |> sweep(MARGIN=2,STATS=cent,FUN="+")
      means<- colMeans(raw.X)
      n <- nrow(raw.X)
      p <- ncol(raw.X)
      center<- TRUE
      scale <- TRUE
      means<-colMeans(raw.X)
      if(all(cent==0)){
        center <- FALSE
        means<- rep(0,p)
      }
      sd<- apply(raw.X,2,sd)
      if(all(norm==1)){
        scale <- FALSE
        sd<-rep(1,p)
      }
      X<- scale(raw.X)
      SVD<- svd(X)
      Vr <- SVD$v[,1:2]
      Lmat <- SVD$v
      e.vects <- 1:2
      ax.one.unit <- 1/(diag(Vr %*% t(Vr))) * Vr
      if (is.null(rownames(X))) rownames(X) <- paste(1:nrow(X))
      Z <- X %*% Vr
      if (is.null(rownames(Z))) rownames(Z) <- paste(1:nrow(Z))
    }
    na.vec.df <- NULL

    if(is.null(group.aes)) { if (!is.null(classes)) group.aes <- factor(classes) else group.aes <- factor(rep(1,n)) }
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
       p <- NULL
       n <- NULL
    }
    else
    {
      means <- apply(X, 2, mean)
      sd <- apply(X, 2, stats::sd)
      n <- nrow(X)
      p <- ncol(X)
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
      if (is.null(n)) n <- nrow(Xcat)
      p2 <- ncol(Xcat)
      if (is.null(rownames(Xcat))) rownames(Xcat) <- paste(1:nrow(Xcat))
      if (is.null(colnames(Xcat))) colnames(Xcat) <- paste("F", 1:ncol(Xcat), sep = "")
    }
    else p2 <- NULL

    if(!is.null(classes))
      classes <- factor(classes)

    if(is.null(group.aes)) { if (!is.null(classes)) group.aes <- classes else group.aes <- factor(rep(1,nrow(data))) }
    else group.aes <- factor(group.aes)
    
    g.names <-levels(group.aes)
    g <- length(g.names)

    object <- list(X = X, Xcat = Xcat, raw.X = data, classes=classes, na.action=na.vec.df, center=center, scaled=scaled,
                   means = means, sd = sd, n=n, p=p, p2=p2, group.aes = group.aes,g.names = g.names,g = g,
                   Title = Title)
    class(object) <- "biplot"
  }

  object
}

#' Compute measures of fit for the biplot.
#' 
#' @description
#' This function computes the measures of fit for the biplot. The biplot object is augmented with additional items, which can differ depending on the type of biplot. The measures provide information on the overall quality of fit and the adequacy of representation of variables.  
#'
#' @param bp an object of class \code{biplot}.
#'
#' @return An object of class \code{biplot}. The object is augmented with
#'         additional items, depending on the type of biplot object.
#' \item{quality}{the overall quality of fit.}
#' \item{adequacy}{the adequacy of representation of variables.}
#'
#' For an object of class \code{PCA}:
#' \item{axis.predictivity}{the fit measure of each individual axis.}
#' \item{sample.predictivity}{the fit measure for each individual sample.}

#' For an object of class \code{CVA}:
#' \item{axis.predictivity}{the fit measure of each individual axis.}
#' \item{class.predictivity}{the fit measure for each class mean.}
#' \item{within.class.axis.predictivity}{the fit measure for each axis based on values
#'                                       expressed as deviations from their class means.}
#' \item{within.class.sample.predictivity}{the fit measure for each sample expressed as
#'                                         deviation from its class mean.}
#'
#' For an object of class \code{CA}:
#' \item{row.predictivity}{the fit measure for each row of the input matrix individual sample.}
#' \item{col.predictivity}{the fit measure for each column of the input matrix individual sample.}
#' \item{Xhat}{predicted matrix per row profile}
#' 
#' @export
#'
#' @examples
#' out <- biplot (iris[,1:4]) |> PCA() |> fit.measures()
#' summary(out)
#' 
fit.measures <- function (bp)
{
  if (inherits(bp, "PCA"))
  {
    bp$quality <- sum(bp$eigenvalues[bp$e.vects])/sum(bp$eigenvalues)
    bp$adequacy <- apply(bp$Lmat[,bp$e.vects,drop=FALSE],1,function(x)sum(x^2))
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
  if (inherits(bp, "CA"))
  {
    nc <- bp$c
    nr <- bp$r
    nm <- min(nc, nr)
    
    Sigma2 <- diag(bp$SVD[[1]])^2
    quality <- sum((bp$SVD[[1]]^2)[bp$e.vects[1:bp$dim.biplot]])/sum((bp$SVD[[1]]^2))
    bp$quality <- quality
    
    tempMat <- diag(diag(bp$SVD[[3]] %*% Sigma2 %*% t(bp$SVD[[3]])))
    Weights <- diag(tempMat)/sum(Sigma2)
    out.cols <- vector("list", nc)
    out.Adeq <- vector("list", nc)
    out.Xhat <- vector("list", nm)
    
    for (i in 1:nm) {
      Jr <- matrix(0, nrow = nm, ncol = nm)
      Jr[1:i, 1:i] <- diag(i)
      out.cols[[i]] <- diag(diag(bp$SVD[[3]] %*% Sigma2 %*% Jr %*% t(bp$SVD[[3]]))) %*% solve(tempMat)
      out.Adeq[[i]] <- diag(diag(bp$SVD[[3]] %*% Jr %*% t(bp$SVD[[3]])))
      out.Xhat[[i]] <- bp$SVD[[2]] %*% diag(bp$SVD[[1]]) %*% Jr %*% t(bp$SVD[[3]])
      dimnames(out.Xhat[[i]]) <- dimnames(bp$X)
    }
    
    names(out.Xhat) <- c(paste("Dim", 1:nm, sep = " "))
    bp$Xhat <- out.Xhat
    
    col.predictivities <- round(diag(out.cols[[1]]), digits = 4)
    for (i in 2:nm) col.predictivities <- rbind(col.predictivities, round(diag(out.cols[[i]]), digits = 4))
    dimnames(col.predictivities) <- list(paste("Dim", 1:nm, sep = " "), dimnames(bp$X)[[2]])
    bp$col.predictivities <- col.predictivities
    
    adequacy <- round(diag(out.Adeq[[1]]), digits = 4)
    for (i in 2:nm) adequacy <- rbind(adequacy, round(diag(out.Adeq[[i]]), digits = 4))
    dimnames(adequacy) <- list(paste("Dim", 1:nm, sep = " "), dimnames(bp$X)[[2]])
    bp$adequacy <- adequacy
    
    out.rows <- vector("list", nc)
    for (i in 1:nm) {
      Jr <- matrix(0, nrow = nm, ncol = nm)
      Jr[1:i, 1:i] <- diag(i)
      out.rows[[i]] <- diag(diag(bp$SVD[[2]] %*% Sigma2 %*% Jr %*% t(bp$SVD[[2]]))) %*% 
        solve(diag(diag(bp$SVD[[2]] %*% Sigma2 %*% t(bp$SVD[[2]]))))
    }
    row.predictivities <- round(diag(out.rows[[1]]), digits = 4)
    for (i in 2:nm) row.predictivities <- rbind(row.predictivities, round(diag(out.rows[[i]]), digits = 4))
    dimnames(row.predictivities) <- list(paste("Dim", 1:nm, sep = " "), dimnames(bp$X)[[1]])
    bp$row.predictivities <- row.predictivities
  }
  
  bp
}
# ---------------------------------------------------------------------------------------------

ez.col <- c("blue","green","gold","cyan","magenta","black","red","grey","purple","salmon")

# ----------------------------------------------------------------------------------------------

#'Format the legend for the biplot
#'
#' @description
#' This function enables the user to format the legend and make a required selection to display.
#'
#' @usage legend.type(bp, samples = FALSE, means = FALSE, bags = FALSE,
#'                    ellipses=FALSE,regions=FALSE, new=FALSE, ...)
#' @aliases legend.type
#'
#' @param bp an object of class \code{biplot}.
#' @param samples a logical value indicating whether a legend should be printed for samples, with default \code{FALSE}.
#' @param means a logical value indicating whether a legend should be printed for means, with default \code{FALSE}.
#' @param bags a logical value indicating whether a legend should be printed for bags, with default \code{FALSE}.
#' @param ellipses a logical value indicating whether a legend should be printed for concentration ellipses, with default \code{FALSE}.
#' @param regions a logical value indicating whether a legend should be printed for classification regions, with default \code{FALSE}.
#' @param new a logical value indicating whether the legend should appear in a new window, with default \code{FALSE}.
#' @param ... additional arguments to be sent to \code{legend()}.
#'
#' @return A list with the following components is available:
#' \item{samples}{a logical value indicating whether a legend for samples are provided.}
#' \item{means}{a logical value indicating whether a legend for class means are provided.}
#' \item{bags}{a logical value indicating whether a legend for \eqn{\alpha}-bags are provided.}
#' \item{ellipses}{a logical value indicating whether a legend for \eqn{\kappa}-ellipses are provided.}
#' \item{regions}{a logical value indicating whether a legend for classification regions are provided.}
#' \item{new}{a logical value indicating whether the legend appears on new plot.}
#'
#' @export
#' @examples
#' biplot (iris[,1:4], Title="Test biplot") |> PCA(group.aes = iris[,5]) |>
#'     legend.type(samples=TRUE) |> plot()
#'     
legend.type <- function (bp, samples = FALSE, means = FALSE, bags = FALSE, 
                         ellipses=FALSE,regions=FALSE, new=FALSE, ...)
{
  bp$legend <- list(samples=samples, means=means, bags=bags,ellipses = ellipses,regions =regions, new=new)
  bp$legend.arglist <- list(...)
  bp
}

# ----------------------------------------------------------------------------------------------
#' Construct the legend for the biplot
#'
#' @param bp an object of class \code{biplot}.
#' @param ... more arguments to be sent to \code{legend}.
#'
#' @noRd
#' 
biplot.legend <- function(bp, ...)
{
  legend.type <- c(bp$legend$samples, bp$legend$means, bp$legend$bags, bp$legend$ellipses, bp$legend$regions)
  legend.addition <- c(bp$legend.arglist)

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
                     lwd = bp$alpha.bag.aes$lwd, legend = names(bp$alpha.bags),...)
  }

  if(bp$legend$means) do.call(graphics::legend,c(list("bottomright",col=bp$means.aes$col,pch=bp$means.aes$pch,
                                       legend=bp$g.names[bp$means.aes$which]), legend.addition))

  if(bp$legend$samples) do.call(graphics::legend,c(list("topright", col=bp$samples$col[1:length(bp$samples$which)],
                                                     pch=bp$samples$pch[1:length(bp$samples$which)],
                                                     legend=bp$g.names[bp$samples$which]), legend.addition))

  if(bp$legend$ellipses & !is.null(bp$conc.ellipses))
  { #formatting of legend names
    do.call(graphics::legend,c(list("bottomleft",col=bp$conc.ellipse.aes$col,lty=bp$conc.ellipse.aes$lty,lwd=bp$conc.ellipse.aes$lwd,
                     legend=names(bp$conc.ellipses)), legend.addition))
  }
  
  if(bp$legend$regions)
  {
    plot(x = c(0, 10), y = c(0, 10), type = "n", axes = FALSE, xlab = "", ylab = "", xaxs = "i", yaxs = "i")
    usr <- graphics::par("usr")
    x <- usr[1]
    y <- usr[4]
    do.call(graphics::legend,c(list("bottomleft",fill=bp$classify$aes$col, # ,bp$classify$aes$opacity),
                                    legend=bp$g.names,border="white"),legend.addition))
  }

  
  if(inherits(bp,"CA") & bp$legend$samples)
    do.call(graphics::legend,c(list("topright",col=bp$samples$col[1:bp$g], pch=bp$samples$pch[1:bp$g],
                                    legend=bp$g.names),legend.addition))
  
}

# ----------------------------------------------------------------------------------------------
#' Generic print function for objects of class biplot
#' 
#' @description
#' This function is used to print output when the biplot object is created.
#' 
#' @param x an object of class \code{biplot}.
#' @param ... additional arguments.
#'
#' @return This function will not produce a return value, it is called for side effects.
#'
#' @export
#' @examples
#' out <- biplot (iris[,1:4]) |> PCA()
#' out
#' 
print.biplot <- function (x, ...)
{
  if (!inherits(x, "CA"))
  {
  cat ("Object of class biplot, based on", x$n, "samples and", ncol(x$raw.X), "variables.\n")
  if (!is.null(x$X)) if (ncol(x$X) > 1) cat (ncol(x$X), "numeric variables.\n") else cat (ncol(x$X), "numeric variable.\n")
  if (!is.null(x$Xcat)) if (ncol(x$Xcat) > 1) cat (ncol(x$Xcat), "categorical variables.\n") else cat (ncol(x$Xcat), "categorical variable.\n")
  if (!is.null(x$classes))
    cat (nlevels(x$classes), "classes:", levels(x$classes), "\n")
  }
  if(inherits(x,"CA"))
  { #print statement is different for CA()
    cat ("Object of class CA, based on", nrow(x$raw.X), "row levels and", ncol(x$raw.X), "column levels.\n")
  }
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

  if(!is.null(x$classify$rate))
  {
    cat("\n")
    cat("Misclassification rate is ",1-x$classify$rate, "\n")
  }

  
  if(!is.null(x$classify$table))
  {
    cat("\n")
    cat("Confusion matrix \n")
    print(x$classify$table)
  }
  
  if (!is.null(x$predict$samples))
  {
    cat ("\n")
    cat (paste ("Sample predictions for samples", 
                paste(rownames(x$X)[x$predict$samples],collapse=", "), ".\n", sep=""))
  }
  
  if (!is.null(x$predict$means))
  {
    cat ("\n")
    cat (paste("Class mean predictions for classes ", 
               paste(x$g.names[x$predict$means],collapse=", "), ".\n", sep=""))
  }
}

# ----------------------------------------------------------------------------------------------
#' Generic summary function for objects of class biplot
#' 
#' @description
#' This function is used to print summary output of the biplot. These summary outputs are related to measures of fit. 
#'
#' @param object an object of class \code{biplot}.
#' @param adequacy a logical value indicating whether variable adequacies should be reported, with default \code{TRUE}.
#' @param axis.predictivity a logical value indicating whether axis predictivities should be reported, with default \code{TRUE}.
#' @param sample.predictivity a logical value indicating whether sample predictivities should be reported, with default \code{TRUE}.
#' @param class.predictivity a logical value indicating whether class predictivities should be reported, with default \code{TRUE}
#'                           (only applicable to objects of class \code{CVA}).
#' @param within.class.axis.predictivity a logical value indicating whether within class axis predictivity
#'                                       should be reported, with default \code{TRUE} (only applicable to objects
#'                                       of class \code{CVA}).
#' @param within.class.sample.predictivity a logical value indicating whether within class sample predictivity
#'                                       should be reported, with default \code{TRUE} (only applicable to objects
#'                                       of class \code{CVA}).
#' @param ... additional arguments.
#'
#' @return This function will not produce a return value, it is called for side effects.
#'
#' @export
#' @examples
#' out <- biplot (iris[,1:4]) |> PCA() |> fit.measures()
#' summary(out)
#' 
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
    cat ("\nThe following", length(object$na.action), "sample-rows were removed due to missing values\n", object$na.action, "\n")

  if (inherits(object, "CVA"))
    if (!is.null(object$crit.opt))
      cat ("The dimension of the canonical space is smaller than the dimension of the biplot. Additional dimensions where added with the", 
           object$low.dim, "method with standardised total sample sum of squared residual ", 
           paste0(format(object$crit.opt,digits=4), ".\n"))
  
  if (!is.null(object$quality))
    if (!is.list(object$quality))
      cat ("\nQuality of fit in", object$dim.biplot, "dimension(s) =", paste(round(object$quality*100,1),"%",sep=""), "\n")
  else
  {
    cat("\n")
    for (i in 1:length(object$quality))
      cat ("Quality of fit of", names(object$quality)[i], "in", object$dim.biplot, "dimension(s) =",
           paste(round(object$quality[[i]]*100,1),"%",sep=""), "\n")
  }
  if (!is.null(object$adequacy) & adequacy)
  {
    cat ("Adequacy of variables in", object$dim.biplot, "dimension(s):\n")
    print (object$adequacy)
  }
  if (!is.null(object$axis.predictivity) & axis.predictivity)
  {
    cat ("Axis predictivity in", object$dim.biplot, "dimension(s):\n")
    print (object$axis.predictivity)
  }
  if (!is.null(object$sample.predictivity) & sample.predictivity)
  {
    cat ("Sample predictivity in", object$dim.biplot, "dimension(s):\n")
    print (object$sample.predictivity)
  }
  if (!is.null(object$class.predictivity) & class.predictivity)
  {
    cat ("Class predictivity in", object$dim.biplot, "dimension(s):\n")
    print (object$class.predictivity)
  }
  if (!is.null(object$within.class.axis.predictivity) & within.class.axis.predictivity)
  {
    cat ("Within class axis predictivity in", object$dim.biplot, "dimension(s):\n")
    print (object$within.class.axis.predictivity)
  }
  if (!is.null(object$within.class.sample.predictivity) & within.class.sample.predictivity)
  {
    cat ("Within class sample predictivity in", object$dim.biplot, "dimension(s):\n")
    print (object$within.class.sample.predictivity)
  }
  
  if (!is.null(object$predict$samples))
  {
    cat ("\n")
    cat ("Sample predictions\n")
    mat <- object$predict$samples.mat[,object$predict$which]
    print (mat)
  }
  
  if (!is.null(object$predict$means))
  {
    cat ("\n")
    cat ("Class mean predictions\n")
    mat <- object$predict$means.mat[,object$predict$which]
    print (mat)
  }
  
}

# -----------------------------------------------------------------------------------------------------

#' Interpolate supplementary points and variables to add to the biplot
#' 
#' @description
#' This function adds supplementary points and variables to the plot from a new data set. 
#'
#' @param bp an object of class \code{biplot} obtained from preceding function \code{biplot()}.
#' @param newdata a new data set, similar in structure to the data set supplied to \code{biplot()}
#'                containing supplementary data points to be added onto the biplot.
#' @param newvariable a new data set, similar in structure to the data set supplied to \code{biplot()}
#'                containing supplementary variables to be added onto the biplot.
#'
#' @return The object of class \code{biplot} will be appended with the following elements:
#' \item{Xnew.raw}{the new data.}
#' \item{Xnew}{the matrix of the centered and scaled new numeric variables of new data.}
#' \item{Xnew.cat}{the matrix of the categorical variables of new data.}
#' \item{Znew}{the matrix of the coordinates of the new data in the biplot.}
#'
#' For an object of class \code{CA} the following additional elements will be appended:
#' \item{newrowcoor}{the matrix of row coordinates of the new data in the biplot.}
#' \item{newcolcoor}{the matrix of column coordinates of the new data in the biplot.}
#' 
#' @export
#' 
#' @examples
#' biplot(data = iris[1:145,]) |> PCA() |> interpolate(newdata = iris[146:150,]) |> plot()
#' biplot(HairEyeColor[,,2], center=F) |> CA(variant="Symmetric") |> interpolate(HairEyeColor[,,1]) |> plot()
#'
interpolate <- function (bp, newdata=NULL, newvariable=NULL)
{
  # New samples 
  if(!is.null(newdata))
  {
    if(!inherits(bp,"CA"))
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
      Xnew.cat <- NULL
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
    if (inherits(bp, "PCO"))
    {
      Ynew <- PCOinterpolate (bp$DDmat, bp$dist.func, bp$dist.func.cat, Xnew, Xnew.cat, bp$X, bp$Xcat, bp$Ymat, bp$Lambda, bp$n)
      Znew <- Ynew[,bp$e.vects]
    }
    
    
    bp$Xnew.raw <- Xnew.raw
    bp$Xnew <- Xnew
    bp$Xnew.cat <- Xnew.cat
    bp$Znew <- Znew
  }
    if(inherits(bp,"CA"))
    {
      if (!is.null(newdata)) 
        if (!(((is.vector(newdata)) | (is.matrix(newdata))) & is.numeric(newdata))) 
          stop("newdata must be a numeric vector or matrix. \n")
      
      if (!is.null(newdata)) {
        
        Xnew <- newdata/sum(newdata) # new correspondence matrix
        Xnew.cat <- NULL
        Xnew.raw <- Xnew
        
        if (is.vector(Xnew)) {
          newDr <- diag(apply(Xnew, 1, sum))
          newSmat <- sqrt(solve(newDr)) * (Xnew - newDr * bp$Dc) %*% bp$Dch
        }
        else {
          newDr <- diag(apply(Xnew, 1, sum))
          newDc <- diag(apply(Xnew, 2, sum))
          newDrh <- sqrt(solve(newDr))
          newDch <- sqrt(solve(newDc))
          newEmat <- (newDr %*%matrix(1, nrow = nrow(Xnew), ncol = ncol(Xnew)) %*%  newDc)
          newSmat <- newDrh%*%(Xnew-newEmat)%*%newDch
        }
      }
      svd.out <- svd(newSmat)
      
      newrowcoor <- (svd.out[[2]]%*%(diag(svd.out[[1]])^bp$gamma))*bp$lambda.val
      newcolcoor <- (svd.out[[3]]%*%(diag(svd.out[[1]])^(1-bp$gamma)))/bp$lambda.val
      
      rownames(newrowcoor) <- rownames(Xnew)
      rownames(newcolcoor) <- colnames(Xnew)

      #plotting: combine the samples for bp update
      Znew <- rbind(newrowcoor,newcolcoor)
      rownames(Znew) <- c(rownames(Xnew),colnames(Xnew))
      
      bp$Xnew.raw <- Xnew.raw
      bp$newrowcoor <- newrowcoor
      bp$newcolcoor <- newcolcoor
      bp$Znew <- Znew
    }
}
  
  # New variables 
  if(!inherits(bp,"CA"))
  {
  if(!is.null(newvariable))
  {
    newvariable <- as.matrix(newvariable)
    new.means <- apply(newvariable, 2, mean)
    if(bp$scaled) new.sd <- apply(newvariable, 2, stats::sd) 
    else new.sd <- rep(1, ncol(newvariable))  
    newvariable <- scale(newvariable,center=TRUE,scale = bp$scaled)
    num.vars <- ncol(newvariable)
    if (!is.null(dimnames(newvariable)[[2]])) 
      var.names <- dimnames(newvariable)[[2]]
    else 
      var.names <- paste("NV", 1:ncol(newvariable), sep = "")
    
    if (inherits(bp, "PCA"))
    {
      Sigma.mat <- diag(sqrt(bp$eigenvalues))
      U.mat <- svd(bp$X%*%t(bp$X))$u[,1:length(bp$eigenvalues)]
      SigmaMinOne <- ifelse(Sigma.mat < 1e-10, 0, 1/Sigma.mat)
      br <- t(SigmaMinOne %*% t(U.mat) %*% newvariable)
      br <- br[,bp$e.vects,drop=FALSE]
      new.ax.one.unit <- 1/(diag(br %*% t(br))) * br
      bp$new.ax.one.unit <- new.ax.one.unit
      bp$newvariable <- newvariable
      bp$num.vars <- num.vars 
      bp$var.names <- var.names 
      bp$new.means <- new.means 
      bp$new.sd <- new.sd
    }
    
    if (inherits(bp, "CVA"))
    {
      new.Xmeans <- solve(bp$Nmat) %*% t(bp$G) %*% newvariable
      LambdaMinOne <- ifelse(bp$lambda.mat < 1e-10, 0, 1/bp$lambda.mat)
      br <- t(LambdaMinOne %*% t(bp$Mr) %*% t(bp$Xbar) %*% bp$Cmat %*% new.Xmeans)
      br <- br[,bp$e.vects,drop=FALSE]
      new.ax.one.unit <- new.ax.one.unit <- 1/(diag(br %*% t(br))) * br
      bp$new.ax.one.unit <- new.ax.one.unit
      bp$newvariable <- newvariable
      bp$num.vars <- num.vars 
      bp$var.names <- var.names 
      bp$new.means <- new.means 
      bp$new.sd <- new.sd
    }
  }
  }
  
  bp
}

# -----------------------------------------------------------------------------------------------------
#' Predict samples to display on the biplot
#' 
#' @description
#' This function makes predictions of sample points, variables and means and displays them on the biplot.
#' 
#' @param bp an object of class \code{biplot} obtained from preceding function \code{biplot()}.
#' @param predict.samples a vector specifying which samples to predict.
#' @param predict.means a vector specifying which group means to predict.
#' @param which a vector specifying which variable to do the prediction.
#'
#' @return A list object called \code{predict} appended to the object of class \code{biplot} with the following elements:
#' \item{samples}{a vector of indices of samples which are being predicted.}
#' \item{predict.means}{a vector of group names of groups for which the means are being predicted.}
#' \item{which}{the vector of indices variables which are being predicted.}
#' \item{predict.mat}{the matrix of predicted samples.}
#' \item{predict.means.mat}{the matrix of predicted group means.}
#'
#'
#' @export
#'
#' @examples
#' biplot(data = iris[,1:4]) |> PCA(group.aes=iris[,5], show.class.means = TRUE) |> 
#' prediction(141:145,1:3) |> plot()
#'
prediction <- function (bp, predict.samples=NULL,predict.means=NULL,which=1:bp$p)
{
  if (!all(is.numeric(which))) which <- match(which, colnames(bp$X), nomatch = 0)
  p <- bp$p
  which <- which[which <= p]
  which <- which[which > 0]
  
  if (is.logical(predict.samples)) predict.samples <- 1:bp$n
  if (!all(is.numeric(predict.samples))) predict.samples <- match(predict.samples, rownames(bp$X), nomatch = 0)
  n <- bp$n
  predict.samples <- predict.samples[predict.samples <= n]
  predict.samples <- predict.samples[predict.samples > 0]

  if (is.logical(predict.means)) predict.means <- 1:bp$g
  if (!all(is.numeric(predict.means))) predict.means <- match(predict.means, bp$g.names, nomatch = 0)
  g <- bp$g
  predict.means <- predict.means[predict.means <= g]
  predict.means <- predict.means[predict.means > 0]
  
  if(length(predict.means)>0) { if(!bp$class.means) stop("Set show.class.means to TRUE in PCA()") }
  Zmeans <- bp$Zmeans

  if (!is.null(bp$Lmat)) Lrr <- solve(bp$Lmat)[bp$e.vects,,drop=F]
  else Lrr <- bp$Mrr
  
  if (length(predict.samples)>0) 
    predict.mat <- scale(bp$Z[predict.samples, , drop = F] %*% Lrr, center = F, scale = 1 / bp$sd)
  else predict.mat <- NULL
  if (!is.null(predict.mat))
    predict.mat <- scale(predict.mat, center = -bp$means, scale = F)

  if (length(predict.means)>0) 
    predict.means.mat <- scale(bp$Zmeans[predict.means, , drop = F] %*% Lrr, center = F, scale = 1 / bp$sd)
  else predict.means.mat <- NULL
  if (!is.null(predict.means.mat))
    predict.means.mat <- scale(predict.means.mat, center = -bp$means, scale = F)
    
  if (!is.null(predict.mat))
    dimnames(predict.mat) <- list(rownames(bp$X)[predict.samples], colnames(bp$X))
  if (!is.null(predict.means.mat))
    dimnames(predict.means.mat) <- list(bp$g.names[predict.means], colnames(bp$X))

  if (length(predict.samples)==0) predict.samples <- NULL
  if (length(predict.means)==0) predict.means <- NULL
  bp$predict <- list (samples = predict.samples, 
                      means = predict.means,
                      which = which,
                      samples.mat = predict.mat,
                      means.mat = predict.means.mat)
  bp
}

# -----------------------------------------------------------------------------------------------------
#' Classify samples into classes
#'
#' @param bp an object of class \code{biplot}
#' @param classify.regions a logical value indicating whether classifications regions should be
#'                         shown in the biplot, with default \code{TRUE}.
#' @param col the colours of the classification regions
#' @param opacity the opacity levels of the classification regions
#' @param borders the border colours of the classification regions
#'
#' @return A list object called \code{classify} appended to the object of class \code{biplot} with the following elements:
#' \item{table}{the confusion matrix resulting from the classification into classes.}
#' \item{rate}{the classification accuracy rate.}
#' \item{classify.regions}{a logical value indicating whether classification regions are shown in the biplot.}
#' \item{aes}{a list of chosen aesthetics for the colours, opacity levels and border colours of the classification regions.}
#' 
#' @export
#'
#' @examples
#' biplot(iris[,1:4],classes = iris[,5]) |> CVA() |> axes(col="black") |> 
#'   classify(col=c("red","blue","orange"),opacity=0.1) |> plot()
classify <- function(bp, classify.regions = TRUE, col=ez.col, opacity=0.4, borders = FALSE)
{
  if (inherits(bp, "CVA"))
  {
    while (length(col) < bp$g) col <- c(col, col)
    col <- as.vector(col[1:bp$g])
    col <- grDevices::adjustcolor(col,opacity[1])
    n <- bp$n
    p <- bp$p
    g <- bp$g
    Xmeans <- bp$Xmeans
    Bmat <- bp$Bmat
    Wmat <- bp$Wmat
    Spl <- Wmat / (n -g)
    
    LC <- t(cbind(diag(-0.5*Xmeans %*% solve(Spl) %*% t(Xmeans)), Xmeans %*% solve(Spl)) %*%
              t(cbind(1,bp$X)))
    
    predicted <- c()
    for(i in 1:n) predicted[i] <- which.max(LC[i,])
    
    actual_group <- as.factor(bp$group)
    predicted <- factor(predicted,labels=bp$g.names)
    table <- caret::confusionMatrix(actual_group,predicted)$table
    rate <- caret::confusionMatrix(actual_group,predicted)$overall[1]
    region.midpoints <- bp$Zmeans
    
    bp$classify <- list(table=table,rate=rate,classify.regions=classify.regions,region.midpoints=region.midpoints,
                        aes=list(col=col,opacity=opacity,borders=borders))
  }
  bp
}

# -----------------------------------------------------------------------------------------------------
#' Reflect the biplot about a chosen axis
#' 
#' @description
#' This function provides the user with an option to reflect the biplot horizontally, vertically or diagonally.
#' 
#' @param bp an object of class \code{biplot}
#' @param reflect.axis a character string indicating which axis about to reflect. One of \code{FALSE} (default), "\code{x}" for reflection about the x-axis, "\code{y}" for reflection about the y-axis and "\code{xy}" for reflection about both axes. 
#'
#' @return An object of class \code{biplot}
#' @export
#'
#' @examples
#' biplot(iris[,1:4],group.aes = iris[,5]) |> PCA() |> reflect("x") |> plot()
#' biplot(iris[,1:4],group.aes = iris[,5]) |> PCA() |> reflect("y") |> plot()
#' biplot(iris[,1:4],group.aes = iris[,5]) |> PCA() |> reflect("xy") |> plot()
#' 
reflect <- function(bp,reflect.axis = c("FALSE","x","y","xy"))
{
  relect.axis <- reflect.axis[1]
  reflect.mat <- diag(bp$dim.biplot)
  if (relect.axis == "x" & bp$dim.biplot < 3) reflect.mat[1, 1] <- -1
  if (relect.axis == "y" & bp$dim.biplot == 2) reflect.mat[2, 2] <- -1
  if (relect.axis == "xy" & bp$ dim.biplot == 2) reflect.mat[1:2, 1:2] <- diag(-1, 2)
  
  bp$Z <- bp$Z %*% reflect.mat
  bp$ax.one.unit <- bp$ax.one.unit %*% reflect.mat
  if(!is.null(bp$Zmeans)) bp$Zmeans <- bp$Zmeans %*% reflect.mat
  if(!is.null(bp$Znew)) bp$Znew <- bp$Znew %*% reflect.mat
  
  bp
}

# -----------------------------------------------------------------------------------------------------
#' Rotate the biplot a chosen amount of degrees
#' 
#' @description
#' This function provides the user with an option to rotate the biplot anti-clockwise or clockwise.
#' 
#' @param bp an object of class \code{biplot}
#' @param rotate.degrees a value specifying the degrees the biplot should be rotated, with default \code{0}. A positive value results in anti-clockwise rotation and a negative value in clockwise rotation.
#'
#' @return An object of class \code{biplot}.
#' @export
#'
#' @examples
#' biplot(iris[,1:4],group.aes = iris[,5]) |> PCA() |> rotate(200) |> plot()
#' 
rotate <- function(bp,rotate.degrees=0)
{
  rotate.mat <- diag(bp$dim.biplot)
  
  if (bp$dim.biplot == 2)
  {
    if (!is.null(rotate.degrees))
    {
      if (is.numeric(rotate.degrees))
      {
        radns <- pi * rotate.degrees / 180
        rotate.mat <- matrix(c(cos(radns),-sin(radns), sin(radns), cos(radns)), ncol = 2)
      }
      else
      {
        if (rotate.degrees == "maxpred") 
          {
            if(is.null(bp$fit.measures)) bp <- fit.measures(bp)
            rotate.degrees <- (names(bp$axis.predictivity))[bp$axis.predictivity == max(bp$axis.predictivity)]
            rotate.degrees <- match(rotate.degrees, dimnames(bp$X)[[2]])
          } else rotate.degrees <- match(rotate.degrees, dimnames(bp$X)[[2]])
        radns <- -atan2(bp$V.mat[rotate.degrees, bp$e.vects[2]], bp$V.mat[rotate.degrees, bp$e.vects[1]])
        rotate.mat <- matrix(c(cos(radns),-sin(radns), sin(radns), cos(radns)), ncol = 2)
      }
    }
  }
  bp$Z <- bp$Z %*% rotate.mat
  bp$ax.one.unit <- bp$ax.one.unit %*% rotate.mat
  if(!is.null(bp$Zmeans)) bp$Zmeans <- bp$Zmeans %*% rotate.mat
  if(!is.null(bp$Znew)) bp$Znew <- bp$Znew %*% rotate.mat
  bp
}


