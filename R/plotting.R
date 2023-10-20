# ----------------------------------------------------------------------------------------------
#' Generic Plotting function of objects of class biplot
#'
#' @param x An object of class \code{biplot}.
#' @param exp.factor factor to expand plotting area beyond samples.
#' @param ... additional arguments.
#'
#' @return An object of class \code{biplot}.
#'
#' @export
#' @examples
#' biplot (iris[,1:4]) |> PCA() |> plot()
plot.biplot <- function(x, exp.factor=1.2, ...)
{
  .samples.plot <- function(Z, group.aes, sample.aes)
  {
    x.vals <- Z[, 1]
    y.vals <- Z[, 2]
    invals <- x.vals < usr[2] & x.vals > usr[1] & y.vals < usr[4] & y.vals > usr[3]
    Z <- Z[invals, ]
    groups <- levels(group.aes)
    group.aes <- group.aes[invals]
    for (j in 1:length(sample.aes$which))
     {  group.num <- levels(group.aes)[sample.aes$which[j]]
        Z.class <- Z[group.aes==group.num, , drop = FALSE]
        text.pos <- match(sample.aes$label.side[j], c("bottom", "left", "top", "right"))
        if (sample.aes$label[j]) graphics::text(Z.class[, 1], Z.class[, 2], labels = dimnames(Z.class)[[1]],
                                            cex = sample.aes$label.cex[j], col = sample.aes$col[j], pos = text.pos)
        graphics::points(x = Z.class[, 1], y = Z.class[, 2], pch = sample.aes$pch[j], col = sample.aes$col[j],
                     cex = sample.aes$cex[j])
    }
  }
  .calibrate.axis <- function (j, Xhat, means, sd, axes.rows, ax.which, ax.tickvec,
                               ax.orthogxvec, ax.orthogyvec)
  {

    ax.num <- ax.which[j]
    tick <- ax.tickvec[j]
    ax.direction <- axes.rows[ax.num,]
    r <- ncol(axes.rows)
    ax.orthog <- rbind(ax.orthogxvec, ax.orthogyvec)
    if (nrow(ax.orthog) < r)    ax.orthog <- rbind(ax.orthog, 0)
    if (nrow(axes.rows) > 1)    phi.vec <- diag(1 / diag(axes.rows %*% t(axes.rows))) %*% axes.rows %*% ax.orthog[, ax.num]
    else  phi.vec <- (1 / (axes.rows %*% t(axes.rows))) %*% axes.rows %*% ax.orthog[, ax.num]

    std.ax.tick.label <- pretty(range(Xhat[, ax.num]), n = tick)
    std.range <- range(std.ax.tick.label)
    std.ax.tick.label.min <-  std.ax.tick.label - (std.range[2] - std.range[1])
    std.ax.tick.label.max <-  std.ax.tick.label + (std.range[2] - std.range[1])
    std.ax.tick.label <-  c(std.ax.tick.label,  std.ax.tick.label.min, std.ax.tick.label.max)
    interval <- (std.ax.tick.label - means[ax.num]) / sd[ax.num]
    axis.vals <- sort(unique(interval))

    number.points <- length(axis.vals)
    axis.points <- matrix(0, nrow = number.points, ncol = r)
    for (i in 1:r)
      axis.points[, i] <-  ax.orthog[i, ax.num] + (axis.vals - phi.vec[ax.num]) * ax.direction[i]
    axis.points <- cbind(axis.points, axis.vals * sd[ax.num] + means[ax.num])

    #slope = delta y / delta x of two datapoints
    slope <- (axis.points[1, 2] - axis.points[2, 2]) / (axis.points[1, 1] - axis.points[2, 1])
    #if slope is infinite then all x-values are same
    v <- NULL
    if (is.na(slope))
      {  v <- axis.points[1, 1]
         slope = NULL
      }
    else if (abs(slope) == Inf)
           {  v <- axis.points[1, 1]
              slope = NULL
           }

    #y=mx+c... c=y-mx
    intercept <- axis.points[1, 2] - slope * axis.points[1, 1]

    details <- list(a = intercept, b = slope, v = v)
    retvals <- list(coords = axis.points, a = intercept, b = slope, v = v)
    return(retvals)
  }
  .marker.label.cm <- function(x, y, grad, marker.val, expand = 1, col, label.on.off, side, pos, offset, label.col, cex)
  {
    uin <- graphics::par("pin")/c(usr[2] - usr[1], usr[4] - usr[3])
    mm <- 1/(uin[1] * 25.4)
    d <- expand * mm
    if (grad == "v")
    {  graphics::lines(rep(x, 2), c(y - d, y + d), col = col)
       if (label.on.off == 1) graphics::text(x, y - d, label = marker.val, pos = pos, offset = offset, col = label.col, cex = cex)
    }
    if (grad == "h")
    {  graphics::lines(c(x - d, x + d), rep(y, 2), col = col)
       if (label.on.off == 1) if (side == "right") graphics::text(x + d, y, label = marker.val, pos = pos, offset = offset, col = label.col, cex = cex)
       else graphics::text(x - d, y, label = marker.val, pos = pos, offset = offset, col = label.col, cex = cex)
    }
    if (is.numeric(grad))
    {  b <- d * sqrt(1/(1 + grad * grad))
       a <- b * grad
       graphics::lines(c(x - b, x + b), c(y - a, y + a), col = col)
       if (label.on.off == 1) if (side == "right") graphics::text(x + b, y + a, label = marker.val, pos = pos, offset = offset, col = label.col, cex = cex)
       else graphics::text(x - b, y - a, label = marker.val, pos = pos, offset = offset, col = label.col, cex = cex)
    }
  }
  .marker.func <- function(vec, coef, col, tick.size, side, pos, offset, label.col, cex)
  {
    x <- vec[1]
    y <- vec[2]
    marker.val <- vec[3]
    label.on.off <- vec[4]
    if (is.na(coef[2]))
      .marker.label.cm(x, y, grad = "h", marker.val, expand = tick.size, col = col, label.on.off = label.on.off, side = side, pos = pos, offset = offset, label.col = label.col, cex = cex)
    else if (coef[2] == 0)
           .marker.label.cm(x, y, grad = "v", marker.val, expand = tick.size, col = col, label.on.off = label.on.off, side = side, pos = pos, offset = offset, label.col = label.col, cex = cex)
         else
           .marker.label.cm(x, y, grad = -1/coef[2], marker.val, expand = tick.size, col = col, label.on.off = label.on.off, side = side, pos = pos, offset = offset, label.col = label.col, cex = cex)
  }

  .lin.axes.plot <- function(z.axes, ax.aes, predict.mat)
  {
    for (i in 1:length(ax.aes$which))
    {  ax.num <- ax.aes$which[i]
       this.axis<-z.axes[[i]]
       marker.mat <- this.axis$coords
       marker.mat <- marker.mat[rev(order(marker.mat[, 3])), ]
       x.vals <- marker.mat[, 1]
       y.vals <- marker.mat[, 2]

       lin.coef<-c(a=this.axis$a,b=this.axis$b)
       if (is.null(this.axis$b))
         graphics::abline(v = this.axis$v, col = ax.aes$col[i], lwd = ax.aes$lwd[i], lty = ax.aes$lty[i])
       else
         graphics::abline(coef=lin.coef, col = ax.aes$col[i], lwd = ax.aes$lwd[i], lty = ax.aes$lty[i])

       if (ax.aes$label.dir == "Hor") {  graphics::par(las = 1)
                                         adjust <- c(0.5, 1, 0.5, 0)       }
       if (ax.aes$label.dir == "Orthog") { graphics::par(las = 2)
                                           adjust <- c(1, 1, 0, 0)         }
       if (ax.aes$label.dir == "Paral") {  graphics::par(las = 0)
                                           adjust <- c(0.5, 0.5, 0.5, 0.5) }

       h <- nrow(marker.mat)
       if (is.null(this.axis$b))
         { if (y.vals[1] < y.vals[h])
             graphics::mtext(text = ax.aes$names[i], side = 1, line = ax.aes$label.dist[i], adj = adjust[1], at = x.vals[1], col = ax.aes$label.col[i], cex = ax.aes$label.cex[i])
           else
             graphics::mtext(text = ax.aes$names[i], side = 3, line = ax.aes$label.dist[i], adj = adjust[3], at = y.vals[1], col = ax.aes$label.col[i], cex = ax.aes$label.cex[i])
         }
       else
         { y1.ster <- lin.coef[2] * usr[1] + lin.coef[1]
           y2.ster <- lin.coef[2] * usr[2] + lin.coef[1]
           x1.ster <- (usr[3] - lin.coef[1])/lin.coef[2]
           x2.ster <- (usr[4] - lin.coef[1])/lin.coef[2]
           if (lin.coef[2] == 0)
             { if (x.vals[1] < x.vals[h])
                 graphics::mtext(text = ax.aes$names[i], side = 2, line = ax.aes$label.dist[i], adj = adjust[2], at = y.vals[1], col = ax.aes$label.col[i], cex = ax.aes$label.cex[i])
               else
                 graphics::mtext(text = ax.aes$names[i], side = 4, line = ax.aes$label.dist[i], adj = adjust[4], at = y.vals[1], col = ax.aes$label.col[i], cex = ax.aes$label.cex[i])
             }
           if (lin.coef[2] > 0)
             {  if (x.vals[1] < x.vals[h])
                  if (y1.ster <= usr[4] & y1.ster >= usr[3])
                    graphics::mtext(text = ax.aes$names[i], side = 2, line = ax.aes$label.dist[i], adj = adjust[2], at = y1.ster, col = ax.aes$label.col[i], cex = ax.aes$label.cex[i])
                  else
                    graphics::mtext(text = ax.aes$names[i], side = 1, line = ax.aes$label.dist[i], adj = adjust[1], at = x1.ster, col = ax.aes$label.col[i], cex = ax.aes$label.cex[i])
                else if (y2.ster <= usr[4] & y2.ster >= usr[3])
                       graphics::mtext(text = ax.aes$names[i], side = 4, line = ax.aes$label.dist[i], adj = adjust[4], at = y2.ster, col = ax.aes$label.col[i], cex = ax.aes$label.cex[i])
                     else
                       graphics::mtext(text = ax.aes$names[i], side = 3, line = ax.aes$label.dist[i], adj = adjust[3], at = x2.ster, col = ax.aes$label.col[i], cex = ax.aes$label.cex[i])
             }
           if (lin.coef[2] < 0)
             {  if (x.vals[1] < x.vals[h])
                   if (y1.ster <= usr[4] & y1.ster >= usr[3])
                      graphics::mtext(text = ax.aes$names[i], side = 2, line = ax.aes$label.dist[i], adj = adjust[2], at = y1.ster, col = ax.aes$label.col[i], cex = ax.aes$label.cex[i])
                   else
                      graphics::mtext(text = ax.aes$names[i], side = 3, line = ax.aes$label.dist[i], adj = adjust[3], at = x2.ster, col = ax.aes$label.col[i], cex = ax.aes$label.cex[i])
                else if (y2.ster <= usr[4] & y2.ster >= usr[3])
                       graphics::mtext(text = ax.aes$names[i], side = 4, line = ax.aes$label.dist[i], adj = adjust[4], at = y2.ster, col = ax.aes$label.col[i], cex = ax.aes$label.cex[i])
                     else
                       graphics::mtext(text = ax.aes$names[i], side = 1, line = ax.aes$label.dist[i], adj = adjust[1], at = x1.ster, col = ax.aes$label.col[i], cex = ax.aes$label.cex[i])
             }
        }

      invals <- x.vals < usr[2] & x.vals > usr[1] & y.vals < usr[4] & y.vals > usr[3]
      std.markers <- marker.mat[invals, 3]
      if (is.numeric(std.markers)) std.markers <- zapsmall(std.markers)
      x.vals <- x.vals[invals]
      y.vals <- y.vals[invals]
      if (ax.aes$tick.label[i]) label.on.off <- rep(1, sum(invals))  else label.on.off <- rep(0, sum(invals))
      if (!ax.aes$tick.label[i]) label.on.off[c(1, length(label.on.off))] <- 1
      if(sum(invals)>0) apply(data.frame(x.vals, y.vals, std.markers, label.on.off), 1, .marker.func,
                            coef = lin.coef, col = ax.aes$tick.col[i], tick.size = ax.aes$tick.size[i],
                            side = ax.aes$tick.label.side[i], pos = ax.aes$tick.label.pos[i],
                            offset = ax.aes$tick.label.offset[i], label.col = ax.aes$tick.label.col[i],
                            cex = ax.aes$tick.label.cex[i])
    #  if (!is.null(predict.mat)) apply(cbind(predict.mat, y.vals[1]), 1, .predict.func, coef = lin.coef, col = ax.aes$predict.col[i], lty = ax.aes$predict.lty[i], lwd = ax.aes$predict.lwd[i])
    }
  }


  .bags.plot <- function(z.bags, bag.aes)
  {
    for (i in 1:length(z.bags))
      graphics::polygon (z.bags[[i]], border=bag.aes$col[i], lty=bag.aes$lty[i], lwd=bag.aes$lwd[i])
  }

  .conc.ellipse.plot <- function(z.ellipse, ellipse.aes)
  {
    for (i in 1:length(z.ellipse))
      graphics::polygon(z.ellipse[[i]], border=ellipse.aes$col[i], lty=ellipse.aes$lty[i], lwd = ellipse.aes$lwd[i])
  }

  if (is.null(x$Z)) stop ("Add a biplot method before generating a plot")
  else Z <- x$Z

  old.par <- graphics::par(pty = "s", ...)
  withr::defer(graphics::par(old.par))

  plot(Z[, 1] * exp.factor, Z[, 2] * exp.factor, xlim = range(Z[, 1] * exp.factor), ylim = range(Z[, 2] * exp.factor),
       xaxt = "n", yaxt = "n", xlab = "", ylab = "", type = "n", xaxs = "i", yaxs = "i", asp = 1)
  usr <- graphics::par("usr")

  if (is.null(x$axes)) x <- axes(x)
  ax.aes <- x$axes

  if (length(ax.aes$which) > 0)
  {
    z.axes <- lapply(1:length(ax.aes$which), .calibrate.axis, x$Xhat, x$means, x$sd, x$ax.one.unit, ax.aes$which,
                     ax.aes$ticks, ax.aes$orthogx, ax.aes$orthogy)
    .lin.axes.plot(z.axes, ax.aes, predict.mat=NULL)

  }

  if (is.null(x$samples)) x <- samples(x)
  .samples.plot(Z, x$group.aes, x$samples)

  if (!is.null(x$alpha.bags)) .bags.plot (x$alpha.bags, x$alpha.bag.aes)

  if (!is.null(x$conc.ellipses)) .conc.ellipse.plot (x$conc.ellipses, x$conc.ellipse.aes)

  if (!is.null(x$Title)) graphics::title(main=x$Title)

  if (!is.null(x$legend)) do.call(biplot.legend, list(bp=x, x$legend$arglist))

  invisible(x)
}

