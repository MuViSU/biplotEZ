# -------------------------------------------------------------------------------------------
#' Perform Categorical Principal Components Analysis (CatPCA)
#'
#' @description
#' This function appends the \code{biplot} object with elements resulting from performing CatPCA.
#'
#' @param bp an object of class \code{biplot} obtained from preceding function \code{biplot()}.
#' @param cat.type a vector indicating whether each categorical variables is \code{nominal} or \code{ordinal}.
#' @param dim.biplot the dimension of the biplot. Only values \code{1}, \code{2} and \code{3} are accepted, with default \code{2}.
#' @param e.vects the vector indicating which eigenvectors (principal components) should be plotted in the biplot, with default \code{1:dim.biplot}.
#' @param group.aes a vector of the same length as the number of rows in the data matrix
#'                  for differentiated aesthetics for samples.
#' @param show.class.means a logical value indicating whether group means should be plotted in the biplot.
#' @param epsilon convergence criteria for iterative algorithm with defaults 1e-6.
#'
#' @return An object of class CatPCA with the following elements:
#' \item{X}{the matrix of the centered and scaled numeric variables.}
#' \item{Xcat}{the data frame of the categorical variables.}
#' \item{raw.X}{the original data.}
#' \item{classes}{the vector of category levels for the class variable. This is to be used for \code{colour}, \code{pch} and \code{cex} specifications.}
#' \item{na.action}{the vector of observations that have been removed.}
#' \item{center}{a logical value indicating whether \eqn{\mathbf{X}} is centered.}
#' \item{scaled}{a logical value indicating whether \eqn{\mathbf{X}} is scaled.}
#' \item{means}{the vector of means for each numerical variable.}
#' \item{sd}{the vector of standard deviations for each numerical variable.}
#' \item{n}{the number of observations.}
#' \item{p}{the number of variables.}
#' \item{group.aes}{the vector of category levels for the grouping variable. This is to be used for \code{colour}, \code{pch} and \code{cex} specification.}
#' \item{g.names}{the descriptive names to be used for group labels.}
#' \item{g}{the number of groups.}
#' \item{Title}{the title of the biplot rendered.}
#' \item{Z}{the matrix with each row containing the details of the points that are plotted (i.e. coordinates).}
#' \item{Lmat}{the matrix for transformation to the principal components.}
#' \item{Linv}{the inverse of \eqn{\mathbf{L}}.}
#' \item{eigenvalues}{the vector of eigenvalues of the covariance matrix of \eqn{\mathbf{X}}.}
#' \item{all.ax.one.unit}{one unit in the positive direction of each biplot axis.}
#' \item{ax.type}{one of numeric, ordinal or nominal.}
#' \item{ax.one.unit}{one unit in the positive direction of each numeric biplot axis.}
#' \item{nom.levels}{a list with the levels of each nominal variable}
#' \item{ord.levels}{a list with the levels of each ordinal variable}
#' \item{e.vects}{the vector indicating which principal components are plotted in the biplot.}
#' \item{dim.biplot}{the dimension of the biplot.}
#' \item{class.means}{a logical value indicating whether group means are plotted in the biplot.}
#' \item{Zmeans}{the matrix of class mean coordinates that are plotted in the biplot.}
#'
#' @usage CatPCA(bp, cat.type = NULL, dim.biplot = c(2, 1, 3), e.vects = 1:ncol(bp$X),
#' group.aes = NULL, show.class.means = FALSE, epsilon = 1e-6)
#' @aliases CatPCA
#'
#' @export
#'
#' @examples
#' biplot(iris) |> CatPCA()
#' # create a CatPCA biplot
#' biplot(data = iris) |> CatPCA() |> plot()
#' 
CatPCA <- function (bp, cat.type = NULL, dim.biplot = c(2, 1, 3), e.vects = 1:ncol(bp$X), group.aes=NULL, 
                    show.class.means = FALSE, epsilon = 1e-6)
{
  UseMethod("CatPCA")
}

#' Calculate elements for the CatPCA biplot
#'
#' @description This function performs calculations for the construction of a CatPCA biplot.
#'
#' @inheritParams CatPCA
#'
#' @return an object of class \code{CatPCA}, inherits from classes \code{PCA} and \code{biplot}.
#' @export
#'
#' @examples
#' biplot(iris) |> CatPCA()
#' # create a CatPCA biplot
#' biplot(data = iris) |> CatPCA() |> plot()
#'
CatPCA.biplot <- function (bp, cat.type = NULL, dim.biplot = c(2, 1, 3), e.vects = 1:ncol(bp$X), group.aes=NULL,
                        show.class.means = FALSE, epsilon = 1e-6)
{
  dim.biplot <- dim.biplot[1]
  if (dim.biplot != 1 & dim.biplot != 2 & dim.biplot != 3) stop("Only 1D, 2D and 3D biplots")
  e.vects <- e.vects[1:dim.biplot]
  if (!is.null(group.aes)) 
  { bp$group.aes <- factor(group.aes)
    bp$g.names <-levels(factor(group.aes))
    bp$g <- length(bp$g.names)
  }
  
  if (!bp$center)
  {  warning("Any form of PCA requires a centred datamatrix. Your numeric data was centred before computation. Use center = TRUE in the call to biplot()")
     bp <- biplot (bp$X, center = TRUE, scaled=bp$scaled, bp$classes, bp$group.aes)
  }
  X <- bp$X
  n <- bp$n
  p <- bp$p
  p2 <- bp$p2
  Xcat <- bp$Xcat
  if (is.null(cat.type)) cat.type <- rep("nom", p2)
  if (length(cat.type) != p2) stop ("The length of cat.type needs to match the number of columns in Xcat")
  cat.type <- c("nominal","ordinal")[pmatch (cat.type, c("nominal", "ordinal"), duplicates.ok = TRUE)]
  if (any(is.na(cat.type))) stop ("Only 'nominal' or 'ordinal' allowed for cat.type.")  
  
  Gk <- vector("list", p2)
  Lk.mat <- vector("list", p2)
  Lk <- rep(NA, p2)
  zlist <- vector("list", p2)
  Hmat <- matrix(nrow = n, ncol = p2)
  CatLabels <- NULL
  
  for (k in 1:p2) 
  { Gk[[k]] <- indmat(Xcat[, k])
    CatLabels <- append(CatLabels, dimnames(Gk[[k]])[[2]])
    Lk[k] <- ncol(Gk[[k]])
    Lk.mat[[k]] <- t(Gk[[k]]) %*% Gk[[k]]
  }
  
  Gmat <- matrix(unlist(Gk), nrow = n)
  L.mat <- diag(diag(t(Gmat) %*% Gmat))
  L.mat.min.half <- diag(diag(L.mat)^-0.5)
  N.mat <- diag(n) - matrix(1, nrow = n, ncol = n)/n
  svd.mca <- svd(L.mat.min.half %*% t(Gmat) %*% N.mat %*% Gmat %*% L.mat.min.half/p)
  z.ini <- svd.mca$v[, 1]
  names(z.ini) <- CatLabels
  count <- 0
  for (k in 1:p2) 
  {  zlist[[k]] <- as.vector(z.ini[(count + 1):(count + ncol(Gk[[k]]))])
     names(zlist[[k]]) <- CatLabels[(count + 1):(count + ncol(Gk[[k]]))]
     count <- count + ncol(Gk[[k]])
     if (identical(cat.type[k], "ordinal")) 
       { 
          gradient <- sign(stats::lm(zlist[[k]] ~ I(1:length(zlist[[k]])))$coef[2])
          if (gradient>0) zlist[[k]] <- ties (d = 1:length(zlist[[k]]), delta=zlist[[k]])
          if (gradient<0) zlist[[k]] <- rev(ties (d = 1:length(zlist[[k]]), delta=rev(zlist[[k]])))
          if (gradient==0) zlist[[k]] <- mean(zlist[[k]])         
       }
  }
  for (k in 1:p2) 
  { labs <- names(zlist[[k]])
    scaling.factor <- as.numeric(sqrt(t(zlist[[k]]) %*% t(Gk[[k]]) %*% N.mat %*% Gk[[k]] %*% zlist[[k]]))
    if (is.na(1/scaling.factor)) scaling.factor <- 1
    scaling.vec <- rep(scaling.factor, length(zlist[[k]]))
    if (identical(cat.type[k], "nominal")) zlist[[k]] <- as.vector(zlist[[k]]/scaling.vec)
    if (identical(cat.type[k], "ordinal")) 
    {  u.temp <- sum(diag(Lk.mat[[k]]))
       v.temp <- sum(zlist[[k]] * diag(Lk.mat[[k]]))                       
       w.temp <- c(t(zlist[[k]]) %*% Lk.mat[[k]] %*% zlist[[k]])
       b.temp <- sqrt(1/(w.temp - v.temp^2/u.temp))
       zlist[[k]] <- b.temp * zlist[[k]] - b.temp * v.temp/u.temp
    }
    names(zlist[[k]]) <- labs
    Hmat[, k] <- Gk[[k]] %*% zlist[[k]]
  }
  Hmat <- as.matrix(Hmat)
  dimnames(Hmat) <- dimnames(Xcat)
  again <- TRUE
  RSS.old <- NULL
  Hmat.cent <- cbind(N.mat %*% Hmat, X)
  iter <- 0
  while (again) 
  {  iter <- iter + 1
     svd.out <- svd(Hmat.cent)
     if (dim.biplot>1)
       Yr <- scale(svd.out$u[, e.vects, drop=F] %*% diag(svd.out$d[e.vects]) %*% t(svd.out$v[, e.vects, drop=F]), scale = FALSE, center = TRUE)
     else
       Yr <- scale((svd.out$u[, e.vects, drop=F] * svd.out$d[e.vects]) %*% t(svd.out$v[, e.vects, drop=F]), scale = FALSE, center = TRUE)
     RSS <- sum((Hmat.cent - Yr)^2)
     if (!is.null(RSS.old)) again <- ((RSS.old - RSS) > epsilon)
     RSS.old <- RSS
     if (again) {
      for (k in 1:p2) {
        labs <- names(zlist[[k]])
        if (identical(cat.type[k], "nominal")) {
          scale.factor <- as.numeric(sqrt(t(Yr[, k]) %*% Gk[[k]] %*% solve(Lk.mat[[k]]) %*% t(Gk[[k]]) %*% Yr[, k]))
          zlist[[k]] <- as.vector((solve(Lk.mat[[k]]) %*% t(Gk[[k]]) %*% Yr[, k])/scale.factor)
        }
        if (identical(cat.type[k], "ordinal")) {
          zlist[[k]] <- as.vector((solve(Lk.mat[[k]]) %*% t(Gk[[k]]) %*% Yr[, k]))
          gradient <- sign(stats::lm(zlist[[k]] ~ I(1:length(zlist[[k]])))$coef[2])
          if (gradient>0) zlist[[k]] <- ties (d = 1:length(zlist[[k]]), delta=zlist[[k]])
          if (gradient<0) zlist[[k]] <- rev(ties (d = 1:length(zlist[[k]]), delta=rev(zlist[[k]])))
          if (gradient==0) zlist[[k]] <- mean(zlist[[k]])         
          u.temp <- sum(diag(Lk.mat[[k]]))
          v.temp <- sum(zlist[[k]] * diag(Lk.mat[[k]]))
          w.temp <- c(t(zlist[[k]]) %*% Lk.mat[[k]] %*% zlist[[k]])
          b.temp <- sqrt(1/(w.temp - v.temp^2/u.temp))
          zlist[[k]] <- b.temp * zlist[[k]] - b.temp * v.temp/u.temp
        }
        names(zlist[[k]]) <- labs
        Hmat[, k] <- Gk[[k]] %*% zlist[[k]]
      }
    }
    Hmat.cent <- cbind(N.mat %*% Hmat, X)
  }
  Lz <- rep(NA, p2)
  for (i in 1:p2) Lz[i] <- sum(Lk.mat[[i]] %*% zlist[[i]])
  zLz <- rep(NA, p2)
  for (i in 1:p2) zLz[i] <- sum(t(zlist[[i]]) %*% Lk.mat[[i]] %*% zlist[[i]])
  Hmat.cent.svd <- svd(Hmat.cent)
  
  V.mat <- Hmat.cent.svd$v
  U.mat <- Hmat.cent.svd$u
  Sigma.mat <- diag(Hmat.cent.svd$d)
  Lmat <- Hmat.cent.svd$v
  Vr <- Hmat.cent.svd$v[, e.vects, drop = FALSE]
  Z <- Hmat.cent %*% Hmat.cent.svd$v[, e.vects, drop=F]
  rownames(Z) <- rownames(X)
  
  ax.one.unit <- 1/(diag(Vr %*% t(Vr))) * Vr
  nom.levels <- ord.levels <- NULL
  nom.count <- ord.count <- 0
  for (k in 1:p2)
  {
    if (cat.type[k] == "nominal")
    {
      nom.count <- nom.count + 1
      nom.levels[[nom.count]] <- zlist[[k]]
    }
    if (cat.type[k] == "ordinal")
    {
      ord.count <- ord.count + 1
      ord.levels[[ord.count]] <- zlist[[k]]
    }
  }
  
  ax.type <- c(cat.type, rep("numeric",p))
  num.levels <- apply(Xcat, 2, function(x) nlevels(factor(x)))
  num.levels <- c(num.levels, rep(1,p))
  ax.type <- data.frame (ax.type, num.levels)
  
  bp$Z <- Z
  bp$Lmat <- Lmat
  bp$eigenvalues <- svd.out$d^2
  bp$all.ax.one.unit <- ax.one.unit
  bp$ax.type <- ax.type
  bp$ax.one.unit <- ax.one.unit[ax.type$ax.type=="numeric",]
  bp$nom.levels <- nom.levels
  bp$ord.levels <- ord.levels
  bp$e.vects <- e.vects
  bp$Vr <- Vr
  bp$dim.biplot <- dim.biplot
  if (bp$g == 1) bp$class.means <- FALSE else bp$class.means <- show.class.means
  if (bp$class.means)
  {
    G <- indmat(bp$group.aes)
    Xmeans <- solve(t(G)%*%G) %*% t(G) %*% X
    Zmeans <- Xmeans %*% Lmat[,e.vects]
    bp$Zmeans <- Zmeans
  }
  
  class(bp) <- append(class(bp),"PCA")
  class(bp) <- append(class(bp),"catPCA")
  bp
}
