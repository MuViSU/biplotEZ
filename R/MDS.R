
# -------------------------------------------------------------------------------------------
#' Regression biplot method
#'
#' @param bp an object of class \code{biplot} obtained from preceding function \code{biplot()}. 
#' @param Z the matrix of coordinates of the samples
#' @param group.aes vector of the same length as the number of rows in the data matrix
#'                  for differentiated aesthetics for samples.
#' @param show.group.means logical, indicating whether group means should be plotted in the biplot.
#' @param axes the type of axes to be fitted to the biplot. Options are 'regression' for linear
#'             regression axes (default) and 'splines' for B-spline axes.
#'
#' @return  Object of class biplot
#'
#' @usage regress(bp, Z, group.aes=NULL, show.group.means = TRUE, 
#'                axes = c("regression", "splines"))
#' @aliases regress
#'
#' @export
#'
#' @examples
#' biplot(iris[,1:4]) |> regress(Z=cmdscale(dist(iris[,1:4]))) |> plot()

regress <- function (bp, Z, group.aes=NULL, show.group.means = TRUE, 
                     axes = c("regression", "splines"))
{
  UseMethod("regress")
}

#' Regression biplot
#'
#' @description Computes regression biplot axes
#'
#' @inheritParams regress
#'
#' @return an object of class biplot.
#' @export
#'
#' @examples
#' biplot(iris) |> regress(Z = cmdscale(dist(iris[,1:4]))) |> plot()
#'
regress.biplot <- function (bp, Z, group.aes=NULL, show.group.means = TRUE,
                            axes = c("regression", "splines"))
{
  axes <- axes[1]
  dim.biplot <- ncol(Z)
  if (dim.biplot != 1 & dim.biplot != 2 & dim.biplot != 3) stop("Only 1D, 2D and 3D biplots")
  e.vects <- 1:dim.biplot
  if (!is.null(group.aes)) { bp$group.aes <- factor(group.aes)
  bp$g.names <-levels(factor(group.aes))
  bp$g <- length(bp$g.names)
  }
  
  if (any(apply(Z, 2, mean) != 0)) Z <- scale(Z, scale=F)
  
  X <- bp$X
  n <- bp$n
  p <- bp$p
  
  if (axes == "regression")
  {
    Lmat <- solve(t(Z)%*%Z) %*% t(Z) %*% X
    Vr <- t(Lmat)
    bp$ax.one.unit <- 1/(diag(Vr %*% t(Vr))) * Vr
    bp$Vr <- Vr
  }
  if(axes == "splines") bp$spline.control <- biplot.spline.axis.control()
  bp$PCOaxes <- axes

  if (is.null(rownames(Z))) rownames(Z) <- rownames(X)
  
  bp$Z <- Z
  bp$Lmat <- NULL
  bp$eigenvalues <- NULL
  bp$e.vects <- e.vects
  bp$dim.biplot <- dim.biplot
  if (bp$g == 1) bp$class.means <- FALSE else bp$class.means <- show.group.means
  if (bp$class.means)
  {
    G <- indmat(bp$group.aes)
    Xmeans <- solve(t(G)%*%G) %*% t(G) %*% X
    Zmeans <- solve(t(G)%*%G) %*% t(G) %*% Z
    bp$Zmeans <- Zmeans
  }
  
  bp
}

# -------------------------------------------------------------------------------------------
#' Principal Coordinate Analysis (PCO) biplot method
#'
#' @param bp an object of class \code{biplot} obtained from preceding function \code{biplot()}. 
#' @param Dmat nxn matrix of Euclidean embeddable distances between samples
#' @param dist.func function to compute Euclidean embeddable distances between samples. The
#'                  default NULL computes Euclidean distance.
#' @param dist.func.cat function to compute Euclidean embeddable distance between categorical 
#'                      variables for the samples. The default NULL computes the extended
#'                      matching coefficient.
#' @param dim.biplot dimension of the biplot. Only values 1, 2 and 3 are accepted, with default \code{2}.
#' @param e.vects e.vects which eigenvectors (canonical variates) to extract, with default \code{1:dim.biplot}.
#' @param group.aes vector of the same length as the number of rows in the data matrix
#'                  for differentiated aesthetics for samples.
#' @param show.class.means logical, indicating whether to plot the class means on the biplot.
#' @param axes type of biplot axes, currently only regression axes are implemented
#' @param ... more arguments to \code{dist.func}
#'
#' @return  Object of class biplot
#'
#' @usage PCO(bp, Dmat=NULL, dist.func=NULL, dist.func.cat=NULL,
#'            dim.biplot = c(2,1,3), e.vects = NULL, group.aes=NULL,
#'            show.class.means = FALSE, axes = c("regression","splines"), ...)
#' @aliases PCO
#'
#' @export
#'
#' @examples
#' biplot(iris[,1:4]) |> PCO(dist.func = sqrtManhattan)
#' # create a CVA biplot
#' biplot(iris[,1:4]) |> PCO(dist.func = sqrtManhattan) |> plot()

PCO <- function (bp, Dmat=NULL, dist.func=NULL, dist.func.cat=NULL,
                 dim.biplot = c(2,1,3), e.vects = NULL, group.aes=NULL,
                 show.class.means = FALSE, axes = c("regression","splines"), ...)
{
  UseMethod("PCO")
}

#' PCO biplot
#'
#' @description Computes Principal Coordinate Analysis biplot
#'
#' @inheritParams PCO
#'
#' @return an object of class biplot.
#' @export
#'
#' @examples
#' biplot(iris) |> PCO(dist.func=sqrtManhattan) |> plot()
#'
PCO.biplot <- function (bp, Dmat=NULL, dist.func=NULL, dist.func.cat=NULL,
                        dim.biplot = c(2,1,3), e.vects = NULL, group.aes=NULL,
                        show.class.means = FALSE, axes = c("regression","splines"), ...)
{
  axes <- axes[1]
  X <- bp$X
  Xcat <- bp$Xcat
  n <- bp$n
  p <- bp$p
  p2 <- ncol(bp$Xcat)
  pp <- ifelse(is.null(p),0,p) + ifelse(is.null(p2),0,p2)
  if (is.null(e.vects)) e.vects <- 1:pp

  if (is.null(dist.func) & !is.null(X)) dist.func <- stats::dist
  if (is.null(dist.func.cat) & !is.null(Xcat)) dist.func.cat <- extended.matching.coefficient
  if (is.null(Dmat)) 
    {  D1 <- D2 <- NULL
       if (!is.null(dist.func)) D1 <- dist.func(X, ...)
       if (!is.null(dist.func.cat)) D2 <- dist.func.cat(Xcat, ...)
       if (!is.null(D1)) D.sq <- D1^2 else D.sq <- NULL
       if (!is.null(D2)) if (is.null(D.sq)) D.sq <- D2^2 else D.sq <- D.sq + D2^2
       Dmat <- sqrt(D.sq)
    }
  Dmat <- as.matrix(Dmat)
  dim.biplot <- dim.biplot[1]
  if (dim.biplot != 1 & dim.biplot != 2 & dim.biplot != 3) stop("Only 1D, 2D and 3D biplots")
  e.vects <- e.vects[1:dim.biplot]
  if (!is.null(group.aes)) { bp$group.aes <- factor(group.aes)
  bp$g.names <-levels(factor(group.aes))
  bp$g <- length(bp$g.names)
  }

  DDmat <- -0.5 * Dmat^2
  s.vec <- matrix(rep(1,n)/n, ncol=1)
  one <- matrix (1, nrow=1, ncol=n)
  
  I.min.N <- diag(n) - s.vec %*% one
  B <- I.min.N %*% DDmat %*% I.min.N
  if (any(zapsmall(eigen(B)$values)<0)) 
    warning ("Your distances are not Euclidean embeddable.")
  
  svd.out <- svd(B)
  Ymat <- svd.out$v %*% diag(sqrt(svd.out$d))
  Lambda <- diag(svd.out$d[svd.out$d>sqrt(.Machine$double.eps)])
  Ymat <- Ymat[,svd.out$d>sqrt(.Machine$double.eps)]
  Z <- Ymat[,e.vects]
  if (!is.null(X)) rownames(Z) <- rownames(X) else rownames(Z) <- rownames(Xcat)

  if (axes == "regression")
  {
    bp$Lmat <- NULL
    bp$eigenvalues <- svd.out$d
    if (!is.null(X))
    {
      Mr <- solve(t(Z) %*% Z) %*% t(Z) %*% X
      bp$ax.one.unit <- 1/(diag(t(Mr) %*% Mr)) * t(Mr)
    }
    else bp$ax.one.unit <- NULL
  }
  if(axes == "splines") bp$spline.control <- biplot.spline.axis.control()
  bp$PCOaxes <- axes
  
  if (!is.null(Xcat))
  {
    CLPs <- vector("list", p2)
    for (j in 1:p2)
    {
      CLPj <- NULL
      for (tau in levels(factor(Xcat[,j])))
      {
        X.tau <- Xcat
        X.tau[,j] <- tau
        Y.tau <- PCOinterpolate(DDmat, dist.func, dist.func.cat,
                                X, X.tau, X, Xcat, Ymat, Lambda, n)
        CLPj <- rbind (CLPj, apply(Y.tau,2,mean))
      }
      rownames(CLPj) <- levels(factor(Xcat[,j]))
      CLPs[[j]] <- CLPj
    }
    names(CLPs) <- colnames(Xcat)
  }
  else CLPs <- NULL
  
  bp$Z <- Z
  bp$Lmat <- NULL
  bp$eigenvalues <- svd.out$d
  bp$e.vects <- e.vects
  bp$dim.biplot <- dim.biplot
  bp$DDmat <- DDmat
  bp$dist.func <- dist.func
  bp$dist.func.cat <- dist.func.cat
  bp$Ymat <- Ymat
  bp$Lambda <- Lambda
  bp$CLPs <- CLPs
  
  if (bp$g == 1) bp$class.means <- FALSE else bp$class.means <- show.class.means
  if (bp$class.means)
  {
    G <- indmat(bp$group.aes)
    Xmeans <- solve(t(G)%*%G) %*% t(G) %*% X
    Zmeans <- solve(t(G)%*%G) %*% t(G) %*% Z
    bp$Zmeans <- Zmeans
  }

  class(bp)<-append(class(bp),"PCO")
  bp
}
  
PCOinterpolate <- function (DDmat, dist.func, dist.func.cat, Xnew, Xnew.cat, X, Xcat, Ymat, Lambda, n)
{
  if (!is.null(Xnew)) if (is.null(dist.func)) stop ("You must specify dist.func")
  if (!is.null(Xnew.cat)) if (is.null(dist.func.cat)) stop ("You must specify dist.func.cat")
  dnew1 <- dnew2 <- NULL
  if (!is.null(Xnew)) 
  {
    big.mat <- rbind (X, Xnew)
    big.D <- as.matrix(dist.func (big.mat))
    dnew1 <- big.D[1:nrow(X),-(1:nrow(X))]
  }
  if (!is.null(Xnew.cat))
  {
    big.mat <- rbind (Xcat, Xnew.cat)
    big.D <- as.matrix (dist.func.cat (big.mat))
    dnew2 <- big.D[1:nrow(Xcat),-(1:nrow(Xcat))]
  }
  if (!is.null(dnew1)) d.n.plus1 <- dnew1^2 else d.n.plus1 <- NULL
  if (!is.null(dnew2)) 
  {
    if (is.null(d.n.plus1)) 
      d.n.plus1 <- dnew2^2 
    else d.n.plus1 <- d.n.plus1 + dnew2^2
  }
  d.n.plus1 <- -0.5*d.n.plus1
  D.one <- matrix(apply(DDmat,1,sum),ncol=1) %*% matrix(1,nrow=1,ncol=ncol(d.n.plus1))
  Ynew.t <- solve(Lambda) %*% t(Ymat) %*% (d.n.plus1 - 1/n*D.one)
  t(Ynew.t)
}