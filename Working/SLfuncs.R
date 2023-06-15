#' Performs calculations for a PCA biplot
#'
#' @param bp object of class biplot
#' @param dim.biplot dimension of the biplot. Only values 1, 2 and 3 are accepted, with default 2
#' @param e.vects which eigevectors (principal components) to extract, defaults to `1:dim.biplot`
#' @param group.aes optional vector of the same length as the number of rows in the data matrix
#'                  for differentiated aesthetics for samples
#' @param correlation.biplot defaults to FALSE. If FALSE, the distances between sample points are
#'                           optimally approximated in the biplot. If TRUE, the correlations between
#'                           variables are optimally approximated by the cosine of the angles between
#'                           axes. See Gabriel (1971) The biplot graphic display of matrices with application
#'                           to principal component analysis. Biometrika, 58(3), pp.453-467.
#'
#' @return an object of class bp
#' @export
#'
#' @examples
#' biplot(iris[,1:4]) |> PCA()
#'
PCA.biplot <- function (bp, dim.biplot = c(2, 1, 3), e.vects = 1:ncol(bp$X), group.aes=NULL,
                        correlation.biplot=FALSE)
{
  dim.biplot <- dim.biplot[1]
  if (dim.biplot != 1 & dim.biplot != 2 & dim.biplot != 3) stop("Only 1D, 2D and 3D biplots")
  e.vects <- e.vects[1:dim.biplot]
  if (!is.null(group.aes)) bp$group.aes <- factor(group.aes)
  group.aes <- bp$group.aes

  if (!bp$center)
    {  warning("PCA requires a centred datamatrix. Your data was centred before computation. Use center = TRUE in the call to biplot()")
       bp <- biplot (bp$X, center = TRUE, scaled=bp$scaled)
    }
  X <- bp$X
  n <- bp$n
  p <- bp$p
  J <- nlevels(group.aes)

  svd.out <- svd(X)
  V.mat <- svd.out$v
  U.mat <- svd.out$u
  Sigma.mat <- diag(svd.out$d)
  Vr <- svd.out$v[, e.vects, drop = F]

  if (correlation.biplot)
    {

       if (dim.biplot>1) lambda.r <- diag(svd(t(X) %*% X)$d[1:dim.biplot])
       else lambda.r <- matrix(svd(t(X) %*% X)$d, nrow=1, ncol=1)
       Z <- sqrt(n - 1) * X %*% Vr %*% sqrt(solve(lambda.r))
  }
  else { Z <- X %*% Vr }
  rownames(Z) <- rownames(X)

  if (correlation.biplot)
    axes.direction <- (sqrt(n - 1)/(diag(Vr %*% lambda.r %*% t(Vr)))) * Vr %*% sqrt(lambda.r)
  else
    axes.direction <- 1/(diag(Vr %*% t(Vr))) * Vr

  bp$Z <- Z
  bp$Vr <- Vr
  bp$Xhat <- Z %*% t(bp$Vr)
  bp$axes.direction <- axes.direction
  if (bp$scaled) bp$Xhat <- scale(bp$Xhat, center=F, scale=1/bp$sd)
  if (bp$center) bp$Xhat <- scale(bp$Xhat, center=-1*bp$means, scale=F)
  bp
}


indmat  <- function (groep.vec)
{  elements <- levels(factor(groep.vec))
Y <- matrix(0, nrow = length(groep.vec), ncol = length(elements))
dimnames(Y) <- list(NULL, paste(elements))
for (i in 1:length(elements)) Y[groep.vec == elements[i], i] <- 1
return(Y)
}




draw.biplot <- function (bp, mean.style = NULL, ax.style = NULL, ax.nominal.style = NULL, ax.ordinal.style = NULL,
                         bag.style = NULL, ellipse.style = NULL, new.sample.style = NULL, density.style = NULL, density.legend=T,
                         region.style = NULL, predict.samples = NULL, predict.means = NULL, Title = NULL, exp.factor = 1.2, plot.coords = NULL, ...)
{

  .connected.samples <- function(Z, G, classes, sample.style) {
    mid.mat <- matrix (NA, nrow=length(classes), ncol=2)
    for (j in 1:length(classes))
    {  x.class <- Z[G[,classes[j]]==1,1]
    y.class <- Z[G[,classes[j]]==1,2]
    mid.mat[j,] <- c(mean(x.class),mean(y.class))
    }
    x.vals <- Z[, 1]
    y.vals <- Z[, 2]
    invals <- x.vals < usr[2] & x.vals > usr[1] & y.vals < usr[4] & y.vals > usr[3]
    Z <- Z[invals, ]
    G <- G[invals, ,drop=F]
    for (j in 1:length(classes))
    { class.num <- classes[j]
    Z.class <- Z[G[, class.num] == 1, , drop = FALSE]
    text.pos <- match(sample.style$label.side[j], c("bottom", "left", "top", "right"))
    if (sample.style$label[j]) text(Z.class[, 1], Z.class[, 2], labels = dimnames(Z.class)[[1]],
                                    cex = sample.style$label.cex[j], col = sample.style$col[j], pos = text.pos)
    points (x=mid.mat[j,1], y=mid.mat[j,2], pch=sample.style$pch[j], col=sample.style$col[j], cex=sample.style$cex[j])
    if(nrow(Z.class)>0) for (i in 1:nrow(Z.class)) lines(x = c(mid.mat[j,1],Z.class[i, 1]), y = c(mid.mat[j,2],Z.class[i, 2]),
                                                         col = sample.style$col[j])
    }
  }
  .predict.func <- function(p.point, coef, col, lty, lwd) {
    if (is.na(coef[2])) lines(c(p.point[1], coef[1]), rep(p.point[2], 2), col = col, lwd = lwd, lty = lty)
    else if (coef[2] == 0) lines(rep(p.point[1], 2), p.point[2:3], col = col, lwd = lwd, lty = lty)
    else {  intercept.projection <- p.point[2] + p.point[1]/coef[2]
    project.on.x <- (intercept.projection - coef[1])/(coef[2] + 1/coef[2])
    project.on.y <- coef[1] + coef[2] * project.on.x
    lines(c(p.point[1], project.on.x), c(p.point[2], project.on.y), col = col, lwd = lwd, lty = lty)
    }
  }
  .marker.label.cm <- function(x, y, grad, marker.val, expand = 1, col, label.on.off, side, pos, offset, label.col, cex) {
    uin <- par("pin")/c(usr[2] - usr[1], usr[4] - usr[3])
    mm <- 1/(uin[1] * 25.4)
    d <- expand * mm
    if (grad == "v")
    {  lines(rep(x, 2), c(y - d, y + d), col = col)
      if (label.on.off == 1) text(x, y - d, label = marker.val, pos = pos, offset = offset, col = label.col, cex = cex)
    }
    if (grad == "h")
    {  lines(c(x - d, x + d), rep(y, 2), col = col)
      if (label.on.off == 1) if (side == "right") text(x + d, y, label = marker.val, pos = pos, offset = offset, col = label.col, cex = cex)
      else text(x - d, y, label = marker.val, pos = pos, offset = offset, col = label.col, cex = cex)
    }
    if (is.numeric(grad))
    {  b <- d * sqrt(1/(1 + grad * grad))
    a <- b * grad
    lines(c(x - b, x + b), c(y - a, y + a), col = col)
    if (label.on.off == 1) if (side == "right") text(x + b, y + a, label = marker.val, pos = pos, offset = offset, col = label.col, cex = cex)
    else text(x - b, y - a, label = marker.val, pos = pos, offset = offset, col = label.col, cex = cex)
    }
  }
  .marker.func <- function(vec, coef, col, tick.size, side, pos, offset, label.col, cex) {
    x <- as.numeric(vec[1])
    y <- as.numeric(vec[2])
    marker.val <- vec[3]
    label.on.off <- as.numeric(vec[4])
    if (is.na(coef[2]))
      .marker.label.cm(x, y, grad = "h", marker.val, expand = tick.size, col = col, label.on.off = label.on.off, side = side, pos = pos, offset = offset, label.col = label.col, cex = cex)
    else if (coef[2] == 0)
      .marker.label.cm(x, y, grad = "v", marker.val, expand = tick.size, col = col, label.on.off = label.on.off, side = side, pos = pos, offset = offset, label.col = label.col, cex = cex)
    else
      .marker.label.cm(x, y, grad = -1/coef[2], marker.val, expand = tick.size, col = col, label.on.off = label.on.off, side = side, pos = pos, offset = offset, label.col = label.col, cex = cex)
  }

.lin.axes.plot <- function(z.axes, ax.style, predict.mat)
{
    for (i in 1:length(ax.style$which))
      {  ax.num <- ax.style$which[i]
         this.axis<-z.axes[[i]]
         marker.mat <- this.axis$coords
         marker.mat <- marker.mat[rev(order(marker.mat[, 3])), ]
         x.vals <- marker.mat[, 1]
         y.vals <- marker.mat[, 2]

         lin.coef<-c(a=this.axis$a,b=this.axis$b)
         if (is.null(this.axis$b))
           abline(v = this.axis$v, col = ax.style$col[i], lwd = ax.style$lwd[i], lty = ax.style$lty[i])
         else
            abline(coef=lin.coef, col = ax.style$col[i], lwd = ax.style$lwd[i], lty = ax.style$lty[i])

         if (ax.style$label == "Hor") {  par(las = 1)
                                         adjust <- c(0.5, 1, 0.5, 0)       }
         if (ax.style$label == "Orthog") { par(las = 2)
                                           adjust <- c(1, 1, 0, 0)         }
         if (ax.style$label == "Paral") {  par(las = 0)
                                           adjust <- c(0.5, 0.5, 0.5, 0.5) }

         h <- nrow(marker.mat)
         if (is.null(this.axis$b))
           { if (y.vals[1] < y.vals[h])
                mtext(text = ax.style$names[i], side = 1, line = ax.style$label.dist[i], adj = adjust[1], at = x.vals[1], col = ax.style$label.col[i], cex = ax.style$label.cex[i])
             else
                mtext(text = ax.style$names[i], side = 3, line = ax.style$label.dist[i], adj = adjust[3], at = y.vals[1], col = ax.style$label.col[i], cex = ax.style$label.cex[i])
           }
        else
           { y1.ster <- lin.coef[2] * usr[1] + lin.coef[1]
             y2.ster <- lin.coef[2] * usr[2] + lin.coef[1]
             x1.ster <- (usr[3] - lin.coef[1])/lin.coef[2]
             x2.ster <- (usr[4] - lin.coef[1])/lin.coef[2]
             if (lin.coef[2] == 0)
               { if (x.vals[1] < x.vals[h])
                   mtext(text = ax.style$names[i], side = 2, line = ax.style$label.dist[i], adj = adjust[2], at = y.vals[1], col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                 else
                   mtext(text = ax.style$names[i], side = 4, line = ax.style$label.dist[i], adj = adjust[4], at = y.vals[1], col = ax.style$label.col[i], cex = ax.style$label.cex[i])
               }
             if (lin.coef[2] > 0)
               {  if (x.vals[1] < x.vals[h])
                    if (y1.ster <= usr[4] & y1.ster >= usr[3])
                      mtext(text = ax.style$names[i], side = 2, line = ax.style$label.dist[i], adj = adjust[2], at = y1.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                    else
                      mtext(text = ax.style$names[i], side = 1, line = ax.style$label.dist[i], adj = adjust[1], at = x1.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                  else if (y2.ster <= usr[4] & y2.ster >= usr[3])
                          mtext(text = ax.style$names[i], side = 4, line = ax.style$label.dist[i], adj = adjust[4], at = y2.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                       else
                         mtext(text = ax.style$names[i], side = 3, line = ax.style$label.dist[i], adj = adjust[3], at = x2.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
               }
            if (lin.coef[2] < 0)
              {  if (x.vals[1] < x.vals[h])
                   if (y1.ster <= usr[4] & y1.ster >= usr[3])
                     mtext(text = ax.style$names[i], side = 2, line = ax.style$label.dist[i], adj = adjust[2], at = y1.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                   else
                     mtext(text = ax.style$names[i], side = 3, line = ax.style$label.dist[i], adj = adjust[3], at = x2.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                 else if (y2.ster <= usr[4] & y2.ster >= usr[3])
                        mtext(text = ax.style$names[i], side = 4, line = ax.style$label.dist[i], adj = adjust[4], at = y2.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                      else
                        mtext(text = ax.style$names[i], side = 1, line = ax.style$label.dist[i], adj = adjust[1], at = x1.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
              }
           }

        invals <- x.vals < usr[2] & x.vals > usr[1] & y.vals < usr[4] & y.vals > usr[3]
        std.markers <- marker.mat[invals, 3]
        if (is.numeric(std.markers)) std.markers <- zapsmall(std.markers)
        x.vals <- x.vals[invals]
        y.vals <- y.vals[invals]
        if (ax.style$tick.label[i]) label.on.off <- rep(1, sum(invals)) else rep(0, sum(invals))
        if (!ax.style$tick.label[i]) label.on.off[c(1, length(label.on.off))] <- 1
        if(sum(invals)>0) apply(data.frame(x.vals, y.vals, std.markers, label.on.off), 1, .marker.func,
                            coef = lin.coef, col = ax.style$tick.col[i], tick.size = ax.style$tick.size[i],
                            side = ax.style$tick.label.side[i], pos = ax.style$tick.label.pos[i],
                            offset = ax.style$tick.label.offset[i], label.col = ax.style$tick.label.col[i],
                            cex = ax.style$tick.label.cex[i])
        if (!is.null(predict.mat)) apply(cbind(predict.mat, y.vals[1]), 1, .predict.func, coef = lin.coef, col = ax.style$predict.col[i], lty = ax.style$predict.lty[i], lwd = ax.style$predict.lwd[i])
   }
}

.nominal.axes.plot <- function(z.axes, ax.style, predict.mat) {
    for (i in 1:length(ax.style$which))
    {  ax.num <- ax.style$which[i]
    marker.mat <- z.axes[[i]][z.axes[[i]][, 4] == 1, 1:3]
    marker.mat <- marker.mat[order(marker.mat[, 1]), ]
    ends.mat <- c(usr[1],NA)
    k <- nrow(marker.mat)
    for (j in 2:nrow(marker.mat))
      ends.mat <- rbind (ends.mat, (marker.mat[j,1:2]+marker.mat[j-1,1:2])/2)
    ends.mat <- rbind (ends.mat, c(usr[2],NA))
    x.vals <- marker.mat[, 1]
    y.vals <- marker.mat[, 2]
    lin.coef <- coefficients(lm(y.vals ~ x.vals))
    if (is.na(lin.coef[2])) {  ends.mat[1,1] <- ends.mat[k+1,1] <- 0
    ends.mat[1,2] <- usr[3]
    ends.mat[k+1,2] <- usr[4]
    }
    else {  ends.mat[1,2] <- lin.coef[1] + ends.mat[1,1]*lin.coef[2]
    ends.mat[k+1,2] <- lin.coef[1] + ends.mat[k+1,1]*lin.coef[2]
    }
    if (ax.style$label == "Hor") {  par(las = 1)
      adjust <- c(0.5, 1, 0.5, 0)       }
    if (ax.style$label == "Orthog") { par(las = 2)
      adjust <- c(1, 1, 0, 0)         }
    if (ax.style$label == "Paral") {  par(las = 0)
      adjust <- c(0.5, 0.5, 0.5, 0.5) }
    if (is.na(lin.coef[2]))
    { if (y.vals[1] < y.vals[k])
      mtext(text = ax.style$names[i], side = 1, line = ax.style$label.dist[i], adj = adjust[1], at = x.vals[1], col = ax.style$label.col[i], cex = ax.style$label.cex[i])
      else
        mtext(text = ax.style$names[i], side = 3, line = ax.style$label.dist[i], adj = adjust[3], at = y.vals[1], col = ax.style$label.col[i], cex = ax.style$label.cex[i])
    }
    else
    { y1.ster <- lin.coef[2] * usr[1] + lin.coef[1]
    y2.ster <- lin.coef[2] * usr[2] + lin.coef[1]
    x1.ster <- (usr[3] - lin.coef[1])/lin.coef[2]
    x2.ster <- (usr[4] - lin.coef[1])/lin.coef[2]
    if (lin.coef[2] == 0)
    { if (x.vals[1] < x.vals[k])
      mtext(text = ax.style$names[i], side = 2, line = ax.style$label.dist[i], adj = adjust[2], at = y.vals[1], col = ax.style$label.col[i], cex = ax.style$label.cex[i])
      else
        mtext(text = ax.style$names[i], side = 4, line = ax.style$label.dist[i], adj = adjust[4], at = y.vals[1], col = ax.style$label.col[i], cex = ax.style$label.cex[i])
    }
    if (lin.coef[2] > 0)
    {  if (x.vals[1] < x.vals[k])
      if (y1.ster <= usr[4] & y1.ster >= usr[3])
        mtext(text = ax.style$names[i], side = 2, line = ax.style$label.dist[i], adj = adjust[2], at = y1.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
      else
        mtext(text = ax.style$names[i], side = 1, line = ax.style$label.dist[i], adj = adjust[1], at = x1.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
      else if (y2.ster <= usr[4] & y2.ster >= usr[3])
        mtext(text = ax.style$names[i], side = 4, line = ax.style$label.dist[i], adj = adjust[4], at = y2.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
      else
        mtext(text = ax.style$names[i], side = 3, line = ax.style$label.dist[i], adj = adjust[3], at = x2.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
    }
    if (lin.coef[2] < 0)
    {  if (x.vals[1] < x.vals[k])
      if (y1.ster <= usr[4] & y1.ster >= usr[3])
        mtext(text = ax.style$names[i], side = 2, line = ax.style$label.dist[i], adj = adjust[2], at = y1.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
      else
        mtext(text = ax.style$names[i], side = 3, line = ax.style$label.dist[i], adj = adjust[3], at = x2.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
      else if (y2.ster <= usr[4] & y2.ster >= usr[3])
        mtext(text = ax.style$names[i], side = 4, line = ax.style$label.dist[i], adj = adjust[4], at = y2.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
      else
        mtext(text = ax.style$names[i], side = 1, line = ax.style$label.dist[i], adj = adjust[1], at = x1.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
    }
    }
    for (j in 1:k)
    {  lines (x=ends.mat[(0:1)+j,1], y=ends.mat[(0:1)+j,2], col=ax.style$col[[i]][j], lwd = 2*ax.style$lwd[i], lty = ax.style$lty[i])
      .marker.func(c(marker.mat[j,],as.numeric(ax.style$tick.label[i])), coef = lin.coef, col = ax.style$tick.col[i],
                   tick.size = ax.style$tick.size[i], side = ax.style$tick.label.side[i], pos = ax.style$tick.label.pos[i],
                   offset = ax.style$tick.label.offset[i], label.col = ax.style$tick.label.col[i],
                   cex = ax.style$tick.label.cex[i])
    }
    if (!is.null(predict.mat)) apply(cbind(predict.mat, y.vals[1]), 1, .predict.func, coef = lin.coef, col = ax.style$predict.col[i], lty = ax.style$predict.lty[i], lwd = ax.style$predict.lwd[i])
    }
  }
  .ordinal.axes.plot <- function(z.axes, ax.style, predict.mat) {
    for (i in 1:length(ax.style$which))
    {  ax.num <- ax.style$which[i]
    marker.mat <- z.axes[[i]][z.axes[[i]][, 4] == 1, 1:3]
    marker.mat <- marker.mat[order(marker.mat[, 1]), ]
    ends.mat <- c(usr[1],NA)
    k <- nrow(marker.mat)
    for (j in 2:nrow(marker.mat))
      ends.mat <- rbind (ends.mat, (marker.mat[j,1:2]+marker.mat[j-1,1:2])/2)
    ends.mat <- rbind (ends.mat, c(usr[2],NA))
    x.vals <- marker.mat[, 1]
    y.vals <- marker.mat[, 2]
    lin.coef <- coefficients(lm(y.vals ~ x.vals))
    if (is.na(lin.coef[2])) {  ends.mat[1,1] <- ends.mat[k+1,1] <- 0
    ends.mat[1,2] <- usr[3]
    ends.mat[k+1,2] <- usr[4]
    }
    else {  ends.mat[1,2] <- lin.coef[1] + ends.mat[1,1]*lin.coef[2]
    ends.mat[k+1,2] <- lin.coef[1] + ends.mat[k+1,1]*lin.coef[2]
    }
    if (ax.style$label == "Hor") {  par(las = 1)
      adjust <- c(0.5, 1, 0.5, 0)       }
    if (ax.style$label == "Orthog") { par(las = 2)
      adjust <- c(1, 1, 0, 0)         }
    if (ax.style$label == "Paral") {  par(las = 0)
      adjust <- c(0.5, 0.5, 0.5, 0.5) }
    if (is.na(lin.coef[2]))
    { if (y.vals[1] < y.vals[k])
      mtext(text = ax.style$names[i], side = 1, line = ax.style$label.dist[i], adj = adjust[1], at = x.vals[1], col = ax.style$label.col[i], cex = ax.style$label.cex[i])
      else
        mtext(text = ax.style$names[i], side = 3, line = ax.style$label.dist[i], adj = adjust[3], at = y.vals[1], col = ax.style$label.col[i], cex = ax.style$label.cex[i])
    }
    else
    { y1.ster <- lin.coef[2] * usr[1] + this.axis$a
    y2.ster <- lin.coef[2] * usr[2] + this.axis$a
    x1.ster <- (usr[3] - this.axis$a)/lin.coef[2]
    x2.ster <- (usr[4] - this.axis$a)/lin.coef[2]
    if (lin.coef[2] == 0)
    { if (x.vals[1] < x.vals[k])
      mtext(text = ax.style$names[i], side = 2, line = ax.style$label.dist[i], adj = adjust[2], at = y.vals[1], col = ax.style$label.col[i], cex = ax.style$label.cex[i])
      else
        mtext(text = ax.style$names[i], side = 4, line = ax.style$label.dist[i], adj = adjust[4], at = y.vals[1], col = ax.style$label.col[i], cex = ax.style$label.cex[i])
    }
    if (lin.coef[2] > 0)
    {  if (x.vals[1] < x.vals[k])
      if (y1.ster <= usr[4] & y1.ster >= usr[3])
        mtext(text = ax.style$names[i], side = 2, line = ax.style$label.dist[i], adj = adjust[2], at = y1.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
      else
        mtext(text = ax.style$names[i], side = 1, line = ax.style$label.dist[i], adj = adjust[1], at = x1.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
      else if (y2.ster <= usr[4] & y2.ster >= usr[3])
        mtext(text = ax.style$names[i], side = 4, line = ax.style$label.dist[i], adj = adjust[4], at = y2.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
      else
        mtext(text = ax.style$names[i], side = 3, line = ax.style$label.dist[i], adj = adjust[3], at = x2.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
    }
    if (lin.coef[2] < 0)
    {  if (x.vals[1] < x.vals[k])
      if (y1.ster <= usr[4] & y1.ster >= usr[3])
        mtext(text = ax.style$names[i], side = 2, line = ax.style$label.dist[i], adj = adjust[2], at = y1.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
      else
        mtext(text = ax.style$names[i], side = 3, line = ax.style$label.dist[i], adj = adjust[3], at = x2.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
      else if (y2.ster <= usr[4] & y2.ster >= usr[3])
        mtext(text = ax.style$names[i], side = 4, line = ax.style$label.dist[i], adj = adjust[4], at = y2.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
      else
        mtext(text = ax.style$names[i], side = 1, line = ax.style$label.dist[i], adj = adjust[1], at = x1.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
    }
    }
    if (ax.style$reverse[i]) lwd.vec <- k:1 else lwd.vec <- 1:k
    lwd.vec <- lwd.vec*ax.style$lwd.factor[i] - (ax.style$lwd.factor[i]-1)
    for (j in 1:k)
    {
      lines (x=ends.mat[(0:1)+j,1], y=ends.mat[(0:1)+j,2], col=ax.style$col[i], lwd = lwd.vec[j], lty = ax.style$lty[i])
      .marker.func(c(marker.mat[j,],as.numeric(ax.style$tick.label[i])), coef = lin.coef, col = ax.style$tick.col[i],
                   tick.size = ax.style$tick.size[i], side = ax.style$tick.label.side[i], pos = ax.style$tick.label.pos[i],
                   offset = ax.style$tick.label.offset[i], label.col = ax.style$tick.label.col[i],
                   cex = ax.style$tick.label.cex[i])
    }
    if (!is.null(predict.mat)) apply(cbind(predict.mat, y.vals[1]), 1, .predict.func, coef = lin.coef, col = ax.style$predict.col[i], lty = ax.style$predict.lty[i], lwd = ax.style$predict.lwd[i])
    }
  }
  .nonlin.axes.plot <- function(z.axes, ax.style, predict.mat) {
    .predict.func <- function(p.point, coef, col, lty, lwd) {
      if (is.na(coef[2])) lines(c(p.point[1], coef[1]), rep(p.point[2], 2), col = col, lwd = lwd, lty = lty)
      else if (coef[2] == 0) lines(rep(p.point[1], 2), p.point[2:3], col = col, lwd = lwd, lty = lty)
      else { intercept.projection <- p.point[2] + p.point[1]/coef[2]
      project.on.x <- (intercept.projection - coef[1])/(coef[2] + 1/coef[2])
      project.on.y <- coef[1] + coef[2] * project.on.x
      lines(c(p.point[1], project.on.x), c(p.point[2], project.on.y), col = col, lwd = lwd, lty = lty)     }
    }
    for (i in 1:length(ax.style$which))
    {  ax.num <- ax.style$which[i]
    axis.mat <- z.axes[[i]]
    axis.mat <- axis.mat[rev(order(axis.mat[, 3])), ]
    x.vals <- axis.mat[, 1]
    y.vals <- axis.mat[, 2]
    xy.before <- rbind(axis.mat[-1,1:2],axis.mat[nrow(axis.mat),1:2])
    xy.after <- rbind(axis.mat[1,1:2],axis.mat[-nrow(axis.mat),1:2])
    coef.mat <- matrix(NA,nrow=nrow(axis.mat),ncol=2)
    for (j in 1:nrow(axis.mat)) coef.mat[j,] <- coefficients(lm(c(xy.after[j,2],xy.before[j,2]) ~ c(xy.after[j,1],xy.before[j,1])))
    invals <- x.vals < usr[2] & x.vals > usr[1] & y.vals < usr[4] & y.vals > usr[3]
    axis.mat <- axis.mat[invals,,drop=F]
    coef.mat <- coef.mat[invals,,drop=F]
    lines(axis.mat[,1], axis.mat[,2], col=ax.style$col[i], lwd=ax.style$lwd[i], lty=ax.style$lty[i])
    if (ax.style$label.side[i]=="below") label.pos <- 1
    if (ax.style$label.side[i]=="left") label.pos <- 2
    if (ax.style$label.side[i]=="above") label.pos <- 3
    if (ax.style$label.side[i]=="right") label.pos <- 4
    if (nrow(axis.mat)>0) text(x=axis.mat[1,1], y=axis.mat[1,2], label = ax.style$names[i], offset = ax.style$label.dist[i], pos = label.pos, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
    marker.mat <- axis.mat[axis.mat[,4]==1,1:3, drop=F]
    if (nrow(marker.mat)>0) { marker.mat[,3] <- zapsmall(marker.mat[,3])
    coef.mat <- coef.mat[axis.mat[,4]==1,,drop=F]
    if (ax.style$tick.label[i]) label.on.off <- rep(1, nrow(marker.mat)) else rep(0, nrow(marker.mat))
    if (!ax.style$tick.label[i]) label.on.off[c(1, length(label.on.off))] <- 1
    for (j in 1:nrow(marker.mat))
      .marker.func (c(marker.mat[j,],label.on.off[j]), coef=coef.mat[j,], col = ax.style$tick.col[i], tick.size = ax.style$tick.size[i], side = ax.style$tick.label.side[i],
                    pos = ax.style$tick.label.pos[i], offset = ax.style$tick.label.offset[i], label.col = ax.style$tick.label.col[i], cex = ax.style$tick.label.cex[i])
    }
    }
  }

  .bags.plot <- function(z.bags, bag.style) {
    for (i in 1:length(z.bags))
      polygon (z.bags[[i]], border=bag.style$col[i], lty=bag.style$lty[i], lwd=bag.style$lwd[i])
  }

  .ellipse.plot <- function(z.ellipse, ellipse.style) {
    for (i in 1:length(z.ellipse))
      polygon(z.ellipse[[i]], border=ellipse.style$col[i], lty=ellipse.style$lty[i], lwd = ellipse.style$lwd[i])
  }

  .new.samples.plot <- function(Z.new, new.sample.style) {
    points(Z.new[, 1], Z.new[, 2], pch = new.sample.style$pch, col = new.sample.style$col, cex = new.sample.style$cex)
    pos.vec <- rep(0, nrow(Z.new))
    pos.vec <- match(new.sample.style$label.side, c("bottom", "left", "top", "right"))
    if (any(new.sample.style$label))
      text(Z.new[new.sample.style$label, 1], Z.new[new.sample.style$label, 2],
           labels = dimnames(Z.new)[[1]][new.sample.style$label],
           cex = new.sample.style$label.cex[new.sample.style$label], pos = pos.vec[new.sample.style$label])
  }

  .class.means.plot <- function(Z.means, mean.style) {
    points(Z.means[, 1], Z.means[, 2], pch = mean.style$pch, col = mean.style$col, cex = mean.style$cex)
    pos.vec <- rep(0, nrow(Z.means))
    pos.vec <- match(mean.style$label.side, c("bottom", "left", "top", "right"))
    if (any(mean.style$label))
      text(Z.means[mean.style$label, 1], Z.means[mean.style$label, 2],
           labels = dimnames(Z.means)[[1]][mean.style$label], cex = mean.style$label.cex[mean.style$label],
           pos = pos.vec[mean.style$label])
  }

  .density.plot <- function(Z.density, density.style) {
    levels.rect <- pretty(range(Z.density$z), n = density.style$cuts)
    col.use <- colorRampPalette(density.style$col)
    col.use <- col.use(length(levels.rect) - 1)
    image(Z.density, breaks = levels.rect, col = col.use, add = TRUE)
    if (density.style$contours)
      contour(Z.density, levels = levels.rect, col = density.style$contour.col, add = TRUE)
    list(levels.rect, col.use)
  }

  .density.legend <- function(levels.rect, col.use) {
    par(pty = "m", mar = density.style$legend.mar)
    plot(range(levels.rect), y = 1:2, ylim = c(10, 100), xaxs = "i", yaxs = "i", xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n", frame.plot = FALSE)
    rect(xleft = levels.rect[-length(levels.rect)], ybottom = 10, xright = levels.rect[-1], ytop = 50, col = col.use, border = FALSE)
    axis(side = 1, at = pretty(levels.rect, n = 8), labels = pretty(levels.rect, n = 8), line = 0, cex.axis = density.style$cex, mgp = density.style$mgp, tcl = density.style$tcl, las = 0)
  }

  .regions.plot  <- function(Z.regions, region.style) {
    for (j in 1:length(Z.regions))
      polygon (Z.regions[[j]], col=region.style$col[j], border=NA)
  }

  Z <- bp$Z

  par(pty = "s", ...)
  # if (!is.null(Z.density)) layout(mat = matrix(1:2, ncol = 1), heights = density.style$layout.heights)
  if (is.null(bp$plot.coords))
     plot(Z[, 1] * exp.factor, Z[, 2] * exp.factor, xlim = range(Z[, 1] * exp.factor), ylim = range(Z[, 2] * exp.factor), xaxt = "n", yaxt = "n", xlab = "", ylab = "", type = "n", xaxs = "i", yaxs = "i", asp = 1)
  else
     plot(plot.coords$x, plot.coords$y, xlim = range(plot.coords$x), ylim = range(plot.coords$y), xaxt = "n", yaxt = "n", xlab = "", ylab = "", type = "n", xaxs = "i", yaxs = "i", asp = 1)

  usr <- par("usr")
  # if (!is.null(predict.samples)) predict.mat <- Z[predict.samples, , drop = F] else predict.mat <- NULL
  # if (!is.null(predict.means)) predict.mat <- rbind(predict.mat, Z.means[predict.means, , drop = F])
  # if (!is.null(Z.density)) density.out <- .density.plot(Z.density, density.style) else density.out <- NULL
  # if (!is.null(Z.regions)) .regions.plot (Z.regions, region.style)
  # if (!is.null(z.axes.nominal)) .nominal.axes.plot(z.axes.nominal, ax.nominal.style, predict.mat)
  # if (!is.null(z.axes.ordinal)) .ordinal.axes.plot(z.axes.ordinal, ax.ordinal.style, predict.mat)
  # if (!is.null(z.trajectories)) .nonlin.axes.plot(z.trajectories, ax.style, predict.mat)
  # if (length(classes) > 0) if(sample.style$connected) .connected.samples(Z, G, classes, sample.style)
  #                           else .samples.plot(Z, G, classes, sample.style)
  # if (length(mean.style$which) > 0) .class.means.plot(Z.means, mean.style)
  # if (!is.null(Z.new)) .new.samples.plot(Z.new, new.sample.style)
  # if (!is.null(density.out) & density.legend) .density.legend(density.out[[1]], density.out[[2]])
}

  biplot(iris[,1:4]) |> PCA() |> samples(col="red") |> draw.biplot()
  biplot(iris[,1:4]) |> PCA(group.aes=iris[,5]) |> draw.biplot()
  biplot(iris[,1:4], group.aes=iris[,5]) |> PCA() |> draw.biplot()
  biplot(iris[,1:4]) |> PCA() |> draw.biplot()
