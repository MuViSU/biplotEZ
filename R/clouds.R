#' Create a density in 2-dimensions 
#'
#' @param bp object of class `biplot`
#' @param which which group to create a density; limited to only a single group at a time. 
#'              If NULL, density drawn over all data points. 
#' @param contours logical indicating whether contours are added to the density plot
#' @param h vector of bandwidths for x and y directions, see \code{\link[MASS]{kde2d}}.
#' @param n number of grid points in each direction. Can be scalar or a length-2 integer 
#'          vector.
#' @param col vector of colours to use to form a 'continuous' sequence of colours. 
#' @param contour.col colour of the contours.
#' @param cuts number of colours in `col`.
#' @param cex character expansion.
#' @param tcl The length of tick marks as a fraction of the height of a line of text. 
#' @param mgp The margin line.
#' @param layout.heights A vector of values for the heights of rows.
#' @param legend.mar The margin line of the legend.
#'
#' @return An object of class \code{biplot}.
#' @export
#'
#' @examples
#' biplot(iris[,1:4],group.aes = iris[,5]) |> PCA() |> 
#'   density2D(which=3,col=c("white","purple","cyan","blue")) |> plot()
#' biplot(iris[,1:4],group.aes = iris[,5]) |> PCA() |> 
#'   density2D(which=3,col=c("white","purple","cyan","blue"),contours = TRUE,
#'   contour.col = "grey") |> plot()

density2D <- function(bp,which = NULL,contours = F, h = NULL, n = 100, 
                      col = c("green", "yellow", "red"), contour.col = "black", cuts = 50, cex = 0.6, 
                      tcl = -0.2, mgp = c(0, -0.25, 0), layout.heights = c(100, 10), legend.mar = c(2, 5, 0, 5))
{
  
  g <- bp$g
  g.names <- bp$g.names
  
  # If which = NULL, draw density over all groups 
  if (is.null(which))
  {
    which <- 1
    G <- indmat(rep(1,bp$n)) 
    print("Density plotted over all samples")
  } else G <- indmat(bp$group.aes)
  
  if(length(which) > 1)
  {
    which <- which[1] 
    print("Density drawn for the first group")
  }  
    
  
  density.style <- biplot.density.2D.control(g=g, g.names=g.names, which=which,
                                             contours=contours, h=h,
                                             n=n,col=col,contour.col=grDevices::adjustcolor(contour.col,0.6),
                                             cuts=cuts, cex=cex,tcl=tcl,mgp=mgp,
                                             layout.heights = layout.heights,
                                             legend.mar=legend.mar)

  mat <- bp$Z[G[, which] == 1, ]
    
  x.range <- range(bp$Z[, 1])
  y.range <- range(bp$Z[, 2])
  width <- max(x.range[2] - x.range[1], y.range[2] - y.range[1])
  xlim <- mean(bp$Z[, 1]) + c(-1, 1) * 0.75 * width
  ylim <- mean(bp$Z[, 1]) + c(-1, 1) * 0.75 * width
  if (is.null(density.style$h)) z.density <- MASS::kde2d(mat[, 1], mat[, 2], n = density.style$n, lims = c(xlim, ylim))
    else z.density <- MASS::kde2d(mat[, 1], mat[, 2], h = density.style$h, n = density.style$n, lims = c(xlim, ylim))
    
  if(length(density.style$col)==1) density.style$col <- c("white",density.style$col)
   
  bp$z.density <- z.density
  bp$density.style <- density.style
  bp
}
#' Creates a kernel density in 1-dimension 
#'
#' @param bp object of class `biplot`
#' @param which which group.
#' @param h bandwidth.
#' @param kernel character string giving the smoothing kernel to be used.
#' @param col colours to be used for each of the density curves.
#' @param lwd linewidth of density curve.
#' @param legend.mar The margin line of the legend.
#'
#' @return An object of class \code{biplot}.
#' @export
#'
#' @examples biplot (iris,classes=iris[,5]) |> CVA(dim=1) |> density1D() |> plot()
density1D <- function(bp,which = NULL, h = "nrd0", kernel="gaussian", 
                      col = ez.col, lwd=1.5, 
                      legend.mar = c(2, 5, 0, 5))
{
  g <- bp$g
  g.names <- bp$g.names
  
  if (is.null(which)) which <- 1:g
  G <- indmat(bp$group.aes)
  tmp.g <- length(which)
  if (length(lwd)==tmp.g){lwd.vec <- lwd} else {lwd.vec <- rep(lwd,bp$g)}
  
  density.style <- list(g=g, g.names=g.names[which], which=which,
                        h=h,col=col[which],lwd=lwd.vec[which],
                        legend.mar=legend.mar)
  
  z.density <- vector("list", bp$g)
  
  z.density.max <- numeric(bp$g)
  
  
  for (i in 1:tmp.g) {
    j <- which[i]
    mat <- bp$Z[G[, j] == 1]
    if(is.numeric(h)){
      z.density[[i]] <- stats::density(mat, width=h, kernel=kernel)
    } else {z.density[[i]] <- stats::density(mat, bw=h, kernel=kernel)}
    z.density.max[i] <- max(z.density[[i]]$y)
  }
  maxy <- max(z.density.max)
  for (j in 1:tmp.g) {
    y.dens <- z.density[[j]]$y
    z.density[[j]]$y <- (y.dens)/(maxy)*2.5
  }
  
  bp$z.density <- z.density
  bp$density.style <- density.style
  bp
}


# ----------------------------------------------------------------------------------------------
#' Create alpha bags
#'
#' @description
#' This function produces \eqn{\alpha}-bags, which is a useful graphical summary of the
#' scatter plot. The alpha-bag refers to a contour which contains \eqn{\alpha}% of the observations.
#'
#' @param bp an object of class \code{biplot}.
#' @param alpha numeric vector between 0 and 1 to determine coverage of the bag (\eqn{\alpha}), with default \code{0.95}.
#' @param which numeric vector indicating the selection of groups or classes to be fitted with \eqn{\alpha}-bags.
#' @param col vector of colours for the \eqn{\alpha}-bags. Multiple \eqn{\alpha} bags for one group will be displayed in the same colour.
#' @param lty vector of line types for the \eqn{\alpha}-bags. The same line type will be used per value of \eqn{\alpha}.
#' @param lwd vector of line widths for the \eqn{\alpha}-bags. The same line width will be used per value of \eqn{\alpha}.
#' @param max maximum number of samples to include in \eqn{\alpha}-bag calculations, with default 2500. If
#'              more samples are in the group, a random sample of size max is taken for the computations.
#' @param trace logical, indicating progress of computation.
#' @param opacity level of opacity, with default \code{0.5}.
#' @param outlying logical indicating whether only outlying points should be plotted. Note the \code{which} argument may be overwritten when \code{TRUE}
#'
#' @return  A list with the following components is available:
#' \item{alpha.bags}{list of coordinates for the \eqn{\alpha}-bags for each group.}
#' \item{col}{vector of colours for the \eqn{\alpha}-bags.}
#' \item{lty}{vector of line types for the \eqn{\alpha}-bags.}
#' \item{lwd}{vector of line widths for the \eqn{\alpha}-bags.}
#'
#' @references
#' Gower, J., Gardner-Lubbe, S. & Le Roux, N. (2011, ISBN: 978-0-470-01255-0) \emph{Understanding Biplots.} Chichester, England: John Wiley & Sons Ltd.
#'
#' @export
#' @usage alpha.bags(bp, alpha = 0.95, which = NULL, col = ez.col[which], lty = 1,
#' lwd = 1, max = 2500, trace = TRUE, opacity = 0.25, outlying=FALSE)
#' @aliases alpha.bags
#'
#' @examples
#' biplot (iris[,1:4]) |> PCA(group.aes=iris[,5]) |> alpha.bags(alpha=0.95) |> plot()
#' biplot (iris[,1:4],group.aes=iris[,5]) |> PCA() |> alpha.bags(alpha=0.95) |> plot()
#'
alpha.bags <- function(bp, alpha=0.95, which = NULL, col = ez.col[which], lty = 1, 
                       lwd = 1, max = 2500, trace = TRUE, opacity = 0.25,
                       outlying=FALSE)
{ 
  if (!requireNamespace("geometry", quietly = TRUE)) {
     stop("Package 'geometry' is required for this function. Please install it.", call. = FALSE)
   }
  g <- bp$g
  g.names <- bp$g.names
  if (is.null(which)) which <- 1:g
  
  #This piece of code is just to ensure which arguments in samples() and alpha.bag() lines up
  # to plot only the specified alpha bags and points
  if(!is.null(bp$samples$which) & outlying){
    if(length(which) != length(bp$samples$which)){
      message("NOTE in alpha.bags(): 'which' argument overwritten in samples()")
      which<-bp$samples$which
    }
    else if(all(sort(which) != sort(bp$alpha.bag.aes$which))){
      message("NOTE in alpha.bags(): 'which' argument overwritten in samples()")
      which<-bp$samples$which
    }
  }
    
    control.output <- control.alpha.bags(g=g, g.names=g.names, alpha=alpha, which=which,
                                        col=col, lty=lty, lwd=lwd, max=max, opacity=opacity)
    
    all.alpha.bags <- list()
    samples_outside<-list()
    
    for(a in 1:length(control.output$which))
    {
      if (trace) 
        cat(paste("Computing", control.output$alpha[a], "-bag for",g.names[control.output$which[a]], "\n"))
      Zgroup <- bp$Z[bp$group.aes==g.names[control.output$which[a]],]
      if(ncol(bp$Z)==2){
        calc <- calc.alpha.bags(Zgroup, aa=control.output$alpha[a], approx.limit=control.output$max)$xy[,1:2]
        new_hull<-geometry::convhulln(calc)
        samples_outside[[a]]<-!geometry::inhulln(new_hull,Zgroup)
      } else if(ncol(bp$Z)==1){
        calc <- stats::quantile(Zgroup, c((1 - control.output$alpha[a])/2, 1 - (1 - control.output$alpha[a])/2))
      } else {
        warning("Alpha Bags only available for biplots of fewer than three dimensions.");calc <- NA
      }
      all.alpha.bags[[a]] <- calc
      
    }
  names(all.alpha.bags) <- paste (g.names[control.output$which], control.output$alpha, sep="-")
  

  if (is.null(bp$alpha.bags))  bp$alpha.bags <- all.alpha.bags
  else bp$alpha.bags <- append(bp$alpha.bags, all.alpha.bags)
  if (is.null(bp$alpha.bag.aes))  bp$alpha.bag.aes <- list(col=control.output$col,
                                                           lty=control.output$lty,
                                                           lwd=control.output$lwd,
                                                           opacity=control.output$opacity)
  else { bp$alpha.bag.aes$col <- c(bp$alpha.bag.aes$col, control.output$col)
  bp$alpha.bag.aes$lty <- c(bp$alpha.bag.aes$lty, control.output$lty)
  bp$alpha.bag.aes$lwd <- c(bp$alpha.bag.aes$lwd, control.output$lwd)
  bp$alpha.bag.aes$opacity <- c(bp$alpha.bag.aes$opacity, control.output$opacity)
  
  
  }
  if(outlying)
    bp$alpha.bag.outside<-samples_outside
  bp$alpha.bag.aes$which<-which
  bp
}

# ----------------------------------------------------------------------------------------------
#' Concentration ellipses (\eqn{\kappa}-ellipses)
#'
#' @description
#' This function produces \eqn{\kappa}-ellipses, which is a useful geometrical description of the
#' data points about the sample mean.
#'
#' @param bp an object of class \code{biplot}.
#' @param df degrees of freedom, with default \code{2}.
#' @param kappa value to construct \eqn{\kappa}-ellipse (the value of \eqn{\kappa}).
#' @param which the selection of the group for ellipse construction.
#' @param alpha size of \eqn{\alpha}-bag, with default \code{0.95}.
#' @param col colour of ellipse. Multiple \eqn{\kappa}-ellipse for one group will be displayed in the same colour.
#' @param lty line type of ellipse. The same line type will be used per value of \eqn{\kappa}.
#' @param lwd line width of ellipse. The same line width will be used per value of \eqn{\kappa}.
#' @param opacity level of opacity, with default \code{0.25}.
#' @param trace logical, indicating progress of computation.
#'
#' @return A list with the following components is available:
#' \item{conc.ellipses}{list of coordinates for the \eqn{\kappa}-ellipses for each group.}
#' \item{col}{vector of colours for the \eqn{\kappa}-ellipses.}
#' \item{lty}{vector of line types for the \eqn{\kappa}-ellipses.}
#' \item{lwd}{vector of line widths for the \eqn{\kappa}-ellipses.}
#' \item{alpha}{vector of \eqn{\alpha} values.}
#'
#' @references
#' Gower, J., Gardner-Lubbe, S. & Le Roux, N. (2011, ISBN: 978-0-470-01255-0) \emph{Understanding Biplots.} Chichester, England: John Wiley & Sons Ltd.<br><br>
#' @export
#' @usage ellipses(bp, df=2, kappa = NULL, which = NULL,
#' alpha = 0.95, col = bp$sample$col[which], lty = 1, lwd = 1,
#' opacity = 0.25, trace = TRUE)
#' @aliases ellipses
#'
#' @examples
#' biplot (iris[,1:4]) |> PCA(group.aes=iris[,5]) |> ellipses(kappa=2) |> plot()
#'
ellipses <- function(bp, df=2, kappa = NULL, which = NULL, alpha = 0.95,
                     col = bp$sample$col[which], lty = 1, lwd = 1, opacity = 0.25, trace = TRUE)
{

  g <- bp$g
  g.names <- bp$g.names
  if (ncol(bp$Z)!=2) df <- ncol(bp$Z)
  if (is.null(which)) which <- 1:g
  if (any(alpha < 0 | alpha > 0.99)) stop(message = "alpha not to be negative or larger than 0.99")
  if (is.null(kappa)) kappa <- sqrt(stats::qchisq(alpha,df))
  control.output <- control.concentration.ellipse (g=g, g.names=g.names, df=df, kappa = kappa, which = which,
                                                   col = col, lty = lty, lwd = lwd,
                                                   opacity = opacity)

  all.ellipses <- list()
  for(a in 1:length(control.output$which))
  {
    if (trace) cat (paste("Computing", round(control.output$kappa[a],2), "-ellipse for",g.names[control.output$which[a]], "\n"))
    Zgroup <- bp$Z[bp$group.aes==g.names[control.output$which[a]],]
    
    if(ncol(bp$Z)==2){calc <- calc.concentration.ellipse(Zgroup, kappa=control.output$kappa[a])
    } else if(ncol(bp$Z)==1){
      calc <- c(-control.output$kappa[a], control.output$kappa[a])*stats::sd(Zgroup)+mean(Zgroup)
    } else if(ncol(bp$Z)==3) {
      calc <- rgl::ellipse3d(x = stats::var(Zgroup), centre = apply(Zgroup, 2, mean), t = control.output$kappa[a])  
      # warning("Ellipses currently only available for biplots of fewer than three dimensions.");calc <- NA
    }
    all.ellipses[[a]] <- calc
  }
  names(all.ellipses) <- paste (g.names[control.output$which], round(control.output$kappa,2), sep="-")

  if (is.null(bp$conc.ellipses))  bp$conc.ellipses <- all.ellipses
  else bp$conc.ellipses <- append(bp$conc.ellipses, all.ellipses)
  if (is.null(bp$conc.ellipse.aes))  bp$conc.ellipse.aes <- list(col=control.output$col,
                                                                 lty=control.output$lty,
                                                                 lwd=control.output$lwd,
                                                                 opacity=control.output$opacity)
  else { bp$conc.ellipse.aes$col <- c(bp$conc.ellipse.aes$col, control.output$col)
  bp$conc.ellipse.aes$lty <- c(bp$conc.ellipse.aes$lty, control.output$lty)
  bp$conc.ellipse.aes$lwd <- c(bp$conc.ellipse.aes$lwd, control.output$lwd)
  bp$conc.ellipse.aes$opacity <- c(bp$conc.ellipse.aes$opacity, control.output$opacity)
  }
  bp
}


# ----------------------------------------------------------------------------------------------
#' Calculate concentration ellipses
#'
#' @param X matrix of coordinates.
#' @param kappa quantile of the chi-squared distribution determining the size of the ellipse.
#' @param covmat a covariance matrix to use in place of computing a covariance matrix from X.
#'
#' @return matrix of coordinates.
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
  a <- (0:628)/100   # 2pi100 = 628.3185
  Y <- cbind(cos(a), sin(a))
  Y <- Y %*% diag(sqrt(svd.covmat$d)) %*% t(svd.covmat$v) * kappa
  Y + matrix(rep(1, 629), ncol = 1) %*% t(means)
}

# ----------------------------------------------------------------------------------------------
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
  if (any(apply(xydata,2,stats::sd)<1e-15))
  {
    xy <- apply(xydata, 2, mean)
    xy <- jitter(rbind (xy, xy, xy))
    return (list(xy=xy))
  }
    
  if (is.data.frame(xydata)) xydata <- as.matrix(xydata)
  if (any(is.na(xydata))) {
    if (na.rm) {
      xydata <- xydata[!apply(is.na(xydata), 1, any), , drop = FALSE]
      warning("NA elements have been removed!!")
    }
    else {
      xy.medians <- apply(xydata, 2, function(x) stats::median(x, na.rm = TRUE))
      for (j in 1:ncol(xydata)) xydata[is.na(xydata[, j]), j] <- xy.medians[j]
      warning("NA elements have been exchanged by median values!!")
    }
  }
  if (length(xydata) < 4) {
    stop("not enough data points")
    return()
  }
  if ((length(xydata)%%2) == 1) {
    stop("number of values isn't even")
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



