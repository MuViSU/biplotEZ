# Utility functions in BiplotFunctions

# just copy

#' Indmat
#'
#' @param groep.vec Grouping
#'
#' @return
#' @export
#'
#' @examples
indmat  <- function (groep.vec)
{
  elements <- levels(factor(groep.vec))
  Y <- matrix(0, nrow = length(groep.vec), ncol = length(elements))
  dimnames(Y) <- list(NULL, paste(elements))
  for (i in 1:length(elements)) Y[groep.vec == elements[i], i] <- 1
  return(Y)
}


# create a function to produce a legend for the samples, classmeans and bags
# position default, but gives the user the option to plot the legend on a new empty plot

# samples, means and alpha bags <--- bp$legend.type - this will feed into the legend function
# samples <--- bp$samples
# groups (pch + col) <--- bp$group.aes and bp$g.names
# classmeans (pch + col) <--- bp$Z.means from CVAbiplot (provision is classmeans is T but no class means computed)
# alphabags (lty,lwd + col) <--- alpha.bags (new function Raeesa to write)

#' Legend
#'
#' @param legend.type Three logical values (samples, means and bags) from legend.type function
#' @param new Logical to create a new plot with legend(s)
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
biplot.legend <- function (legend.type, new=TRUE,...)
{
  if (all(legend.type == FALSE)) return(cat("Change legend.type to obtain a legend\n"))

  if(new)
  {
    plot(x = c(0, 10), y = c(0, 10), type = "n", axes = FALSE, xlab = "", ylab = "", xaxs = "i", yaxs = "i")
    usr <- par("usr")
    x <- usr[1]
    y <- usr[4]
  }

  if(legend.type$bag) legend("topleft",col=bp$samples$col,lty=1,lwd=1,legend=bp$alpha.bags)

  if(legend.type$means) legend("bottomright",col=bp$samples$col,pch=16,legend=bp$g.names)

  if(legend.type$samples) legend("topright",col=bp$samples$col,pch=3,legend=bp$g.names)
}

# write a new function alpha.bags <- function(group=NULL)
# Null means all Z values
# group.aes has n components agreeing with n rows of bp$Z
# should be a list with names
# output of calc.alpha.bags is a matrix of 2 columns.

#' Create alpha bags
#'
#' @param bp Object bp
#'
#' @return
#' @export
#'
#' @examples
alpha.bags <- function(bp)
{
  # confused here
  alpha.bags <- do.call("biplot.alpha.bag.control", c(bp$g, bp$g.names, bp$alpha.bags))

  all.alpha.bags <- list()
  for(a in 1:length(bp$alpha.bags))
  {
    per.alpha.bags <- list()
    for(i in 1:bp$g)
    {
      Zgroup <- Z[which((bp$group.aes)==bp$g.names[i]),]
      calc <- calc.alpha.bags(Zgroup,bp$alpha.bags[[a]])$xy[,1:2]
      per.alpha.bags[[bp$g.names[i]]] <- list(calc)
    }
    all.alpha.bags[[a]] <- per.alpha.bags
  }
}

# input the bp$Z matrix into calc.alpha.bags
# returns a 2 column matrix

#' Calculate alpha bags
#'
#' @param x
#' @param y
#' @param aa
#' @param na.rm
#' @param approx.limit
#' @param precision
#'
#' @return
#' @export
#'
#' @examples
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
    print(result)
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
  if (is.data.frame(xydata)) xydata <- as.matrix(xydata)
  if (any(is.na(xydata))) {
    if (na.rm) {
      xydata <- xydata[!apply(is.na(xydata), 1, any), , drop = FALSE]
      print("Warning: NA elements have been removed!!")
    }
    else {
      xy.medians <- apply(xydata, 2, function(x) median(x, na.rm = TRUE))
      for (j in 1:ncol(xydata)) xydata[is.na(xydata[, j]), j] <- xy.medians[j]
      print("Warning: NA elements have been exchanged by median values!!")
    }
  }
  if (length(xydata) < 4) {
    print("not enough data points")
    return()
  }
  if ((length(xydata)%%2) == 1) {
    print("number of values isn't even")
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
  prdata <- prcomp(xydata)
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
  xysd <- apply(xy, 2, sd)
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
  h[chull(h[, 1], h[, 2]), ]
}


# write a new function concentration.ellipse <- function(group=NULL)
# same function as alpha.bags but change the call to calc.alpha.bags

#' Concentration ellipses
#'
#' @param group
#'
#' @return
#' @export
#'
#' @examples
concentration.ellipse <- function(group=NULL)
{
  kappa.ellipse <- do.call("biplot.kappa.ellipse.control", c(bp$g, bp$g.names, ncol(bp$Z), bp$kappa.ellipse))

  all.ellipses <- list()
  for(k in 1:length(bp$kappa.ellipse))
  {
    per.ellipse <- list()
    for(i in 1:bp$g)
    {
      Zgroup <- Z[which((bp$group.aes)==bp$g.names[i]),]
      calc <- calc.concentration.ellipse(Zgroup,bp$kappa.ellipse[[k]])
      per.ellipse[[bp$g.names[i]]] <- list(calc)
    }
    all.ellipses[[k]] <- per.ellipse
  }
}


#' Calculate concentration ellipses
#'
#' @param X
#' @param kappa
#' @param covmat
#'
#' @return
#' @export
#'
#' @examples
calc.concentration.ellipse <- function (X, kappa=2, covmat = NULL)
{
  means <- matrix(apply(Z, 2, mean), nrow = 2)
  if (is.null(covmat)) covmat <- cov(Z)
  range.vec <- apply(Z, 2, range)
  mid.vec <- apply(range.vec, 2, function(x) (x[2] + x[1])/2)
  dif <- max(range.vec[2, ] - range.vec[1, ])/2
  xlim <- c(mid.vec[1] - dif, mid.vec[1] + dif)
  ylim <- c(mid.vec[2] - dif, mid.vec[2] + dif)
  svd.covmat <- svd(covmat)
  a <- (0:6283)/1000
  Y <- cbind(cos(a), sin(a))
  Y <- Y %*% diag(sqrt(svd.covmat$d)) %*% t(svd.covmat$v) * kappa
  Y + matrix(rep(1, 6284), ncol = 1) %*% t(means)
}


