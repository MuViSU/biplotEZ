# ---------------------------------------------------------------------------------------------
#' Plotly function ala bipl5

# ----------------------------------------------------------------------------------------------
#' Generic Plotting function of objects of class biplot
#'
#' @param x An object of class \code{biplot}.
#' @param exp.factor factor to expand plotting area beyond samples.
#' @param axis.predictivity either logical or a numeric value between 0 and 1.
#'          If it is a numeric value, this value is used as threshold so that
#'          only axes with axis predictivity larger than the threshold is displayed.
#'          If \code{axis.predictivity = TRUE}, the axis color is 'diluted' in
#'          proportion with the axis predictivity.
#' @param sample.predictivity either a logical or a numeric value between 0 and 1.
#'          If it is a numeric value, this value is used as threshold so that
#'          only samples with sample predictivity larger than the threshold is displayed.
#'          if \code{sample.predictivity = TRUE}, the sample size is shrinked in
#'          proportion with the sample predictivity.
#' @param ... additional arguments.
#'
#' @return An object of class \code{biplot}.
#'
#' @export
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 ggplot_build
#' @importFrom ggrepel geom_text_repel
#' @importFrom grid grid.force
#' @importFrom grid childNames
#' @importFrom grid grid.get
#' @importFrom grid convertX
#' @importFrom grid convertY
#' @importFrom grDevices colorRampPalette
#'
#' @examples
#' biplot (iris[,1:4]) |> PCA() |> plot()
plot.biplot <- function(x, exp.factor=1.2, axis.predictivity=NULL, sample.predictivity=NULL, ...)
{
  #----------
  .samples.plot <- function(Z, group.aes, sample.aes, n, g.names,
                            ggrepel.labs, too.small, cex.vec)
  {
    if (sample.aes$connected)
      graphics::lines (Z[,1], Z[,2], col=sample.aes$connect.col, lty=sample.aes$connect.lty, lwd=sample.aes$connect.lwd)
    x.vals <- Z[, 1]
    y.vals <- Z[, 2]
    invals <- x.vals < usr[2] & x.vals > usr[1] & y.vals < usr[4] & y.vals > usr[3]
    which.samples <- rep(FALSE, n)
    for (j in 1:length(sample.aes$which))
      which.samples[group.aes == g.names[sample.aes$which[j]]] <- TRUE
    groups <- levels(group.aes)

    if (sample.aes$label[1]=="ggrepel")
    {
      ZZ <- data.frame (no=1:n, group.aes = group.aes, pch = rep(NA,n),
                        col = rep(NA,n), cex = rep(NA,n),
                        cex.vec, Z)

      for(j in 1:length(sample.aes$which))
      {
        ZZ$pch[group.aes==g.names[sample.aes$which[j]]] = sample.aes$pch[j]
        ZZ$col[group.aes==g.names[sample.aes$which[j]]] = sample.aes$col[j]
        ZZ$cex[group.aes==g.names[sample.aes$which[j]]] = sample.aes$cex[j]
      }
      ZZ <- ZZ[which.samples,]
      ZZ <- ZZ[invals[which.samples],]
      if (!is.null(too.small))
        ZZ <- ZZ[-stats::na.omit(match(too.small,ZZ[,1])),]
      ZZ <- ZZ[,-1]
      ZZ.points <- ZZ[,2:5]
      ZZ <- ZZ[,-(1:5)]
      for (j in 1:nrow(ggrepel.labs$coords))
         graphics::text(ggrepel.labs$coords[j, 1], ggrepel.labs$coords[j, 2], labels = ggrepel.labs$coords[j,3],
                        cex = sample.aes$label.cex[ggrepel.labs$visible[j]],
                        col = sample.aes$label.col[ggrepel.labs$visible[j]])
      for (j in ggrepel.labs$textlines)
      {
        label.val <- rownames(Z)[j]
        label.xy <- ggrepel.labs$coords[match(label.val, ggrepel.labs$coords[,3]),1:2]
        graphics::lines (x=c(label.xy[1],Z[j,1]), y=c(label.xy[2],Z[j,2]), col=sample.aes$label.col[j])
      }
    }
    else
    {
      ZZ <- data.frame (no=1:n, names=rownames(Z), label=sample.aes$label,
                        label.side = sample.aes$label.side, label.cex = sample.aes$label.cex,
                        label.col = sample.aes$label.col, label.offset = sample.aes$label.offset,
                        group.aes = group.aes, pch = rep(NA,n),
                        col = rep(NA,n), cex = rep(NA,n),
                        cex.vec, Z)

      for(j in 1:length(sample.aes$which))
      {
        ZZ$pch[group.aes==g.names[sample.aes$which[j]]] = sample.aes$pch[j]
        ZZ$col[group.aes==g.names[sample.aes$which[j]]] = sample.aes$col[j]
        ZZ$cex[group.aes==g.names[sample.aes$which[j]]] = sample.aes$cex[j]
      }
      ZZ <- ZZ[which.samples,]
      ZZ <- ZZ[invals[which.samples],]
      if (!is.null(too.small))
        ZZ <- ZZ[-stats::na.omit(match(too.small,ZZ[,1])),]
      ZZ <- ZZ[,-1]
      ZZ.labels <- ZZ[,1:6]
      ZZ <- ZZ[,-(1:6)]
      ZZ.points <- ZZ[,2:5]
      ZZ <- ZZ[,-(1:5)]
      for (j in 1:nrow(ZZ.labels))
      {  text.pos <- match(ZZ.labels$label.side[j], c("bottom", "left", "top", "right"))
         if (ZZ.labels$label[j])
           graphics::text(ZZ[j, 1], ZZ[j, 2], labels = ZZ.labels$names[j],
                          cex = ZZ.labels$label.cex[j], col = ZZ.labels$label.col[j],
                          pos = text.pos, offset = ZZ.labels$label.offset[j])
      }
    }
    for (i in 1:nrow(ZZ.points))
      graphics::points (x=ZZ[i,1], y=ZZ[i,2], pch=ZZ.points$pch[i],
                           col=ZZ.points$col[i],
                           cex=ZZ.points$cex.vec[i]*ZZ.points$cex[i])
  }
  #----------
  .means.plot <- function(Z, sample.aes, g.names, ggrepel.labs)
  {
    x.vals <- Z[, 1]
    y.vals <- Z[, 2]
    invals <- x.vals < usr[2] & x.vals > usr[1] & y.vals < usr[4] & y.vals > usr[3]
    Z <- Z[invals, ]
    ZZ <- Z[sample.aes$which,]
    if (sample.aes$label[1]=="ggrepel")
    {
      for (j in 1:nrow(ggrepel.labs$coords))
        graphics::text(ggrepel.labs$coords[j, 1], ggrepel.labs$coords[j, 2], labels = ggrepel.labs$coords[j,3],
                       cex = sample.aes$label.cex[ggrepel.labs$visible[j]],
                       col = sample.aes$label.col[ggrepel.labs$visible[j]])
      for (j in ggrepel.labs$textlines)
      {
        label.val <- rownames(ZZ)[j]
        label.xy <- ggrepel.labs$coords[match(label.val, ggrepel.labs$coords[,3]),1:2]
        graphics::lines (x=c(label.xy[1],ZZ[j,1]), y=c(label.xy[2],ZZ[j,2]), col=sample.aes$label.col[j])
      }
    }
    else
    {
      Z.labels <- rownames(Z)[sample.aes$which]
      for (j in 1:length(sample.aes$label.side))
      {  text.pos <- match(sample.aes$label.side[j], c("bottom", "left", "top", "right"))
         if (sample.aes$label[j]) graphics::text(ZZ[j, 1], ZZ[j, 2], labels = Z.labels[j],
                                              cex = sample.aes$label.cex[j], col = sample.aes$label.col[j],
                                              pos = text.pos, offset = sample.aes$label.offset[j])
      }
    }
    for (j in 1:length(sample.aes$which))
      graphics::points(x = Z[sample.aes$which[j], 1], y = Z[sample.aes$which[j], 2],
                       pch = sample.aes$pch[j], col = sample.aes$col[j], cex = sample.aes$cex[j])
  }
  #----------
  .newsamples.plot <- function(Z, sample.aes, ggrepel.labs)
  {
    if (sample.aes$connected)
      graphics::lines (Z[,1], Z[,2], col=sample.aes$connect.col, lty=sample.aes$connect.lty, lwd=sample.aes$connect.lwd)
    x.vals <- Z[, 1]
    y.vals <- Z[, 2]
    invals <- x.vals < usr[2] & x.vals > usr[1] & y.vals < usr[4] & y.vals > usr[3]
    Z <- Z[invals, , drop = FALSE]
    if (sample.aes$label[1]=="ggrepel")
    {
      ggrepel.labs$coords <- ggrepel.labs$coords[invals, ,drop=F]
      ggrepel.labs$visible <- ggrepel.labs$visible[invals]
      ggrepel.labs$textlines <- ggrepel.labs$textlines[invals]
      for (j in 1:nrow(ggrepel.labs$coords))
        graphics::text(ggrepel.labs$coords[j, 1], ggrepel.labs$coords[j, 2], labels = ggrepel.labs$coords[j,3],
                       cex = sample.aes$label.cex[ggrepel.labs$visible[j]],
                       col = sample.aes$label.col[ggrepel.labs$visible[j]])
      for (j in ggrepel.labs$textlines)
      {
        label.val <- rownames(Z)[j]
        label.xy <- ggrepel.labs$coords[match(label.val, ggrepel.labs$coords[,3]),1:2]
        graphics::lines (x=c(label.xy[1],Z[j,1]), y=c(label.xy[2],Z[j,2]), col=sample.aes$label.col[j])
      }
    }
    else
    {
      for (j in 1:nrow(Z))
      {  text.pos <- match(sample.aes$label.side[j], c("bottom", "left", "top", "right"))
         if (sample.aes$label[j]) graphics::text(Z[j, 1], Z[j, 2], labels = rownames(Z)[j],
                                              cex = sample.aes$label.cex[j], col = sample.aes$label.col[j],
                                              pos = text.pos, offset = sample.aes$label.offset[j])
      }
    }
    graphics::points(x = Z[, 1], y = Z[, 2], pch = sample.aes$pch, col = sample.aes$col,
                     cex = sample.aes$cex)
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
  
  
  #----------
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
  #----------
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
  #----------
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
  #----------
  .lin.axes.plot <- function(z.axes, ax.aes, predict.mat, too.small)
  {
    for (i in 1:length(ax.aes$which))
    {  ax.num <- ax.aes$which[i]
       if (!is.null(too.small)) if (ax.num %in% too.small) next
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
           graphics::mtext(text = ax.aes$names[i], side = 1, line = ax.aes$label.line[i], adj = adjust[1], at = x.vals[1], col = ax.aes$label.col[i], cex = ax.aes$label.cex[i])
         else
           graphics::mtext(text = ax.aes$names[i], side = 3, line = ax.aes$label.line[i], adj = adjust[3], at = y.vals[1], col = ax.aes$label.col[i], cex = ax.aes$label.cex[i])
       }
       else
       { y1.ster <- lin.coef[2] * usr[1] + lin.coef[1]
         y2.ster <- lin.coef[2] * usr[2] + lin.coef[1]
         x1.ster <- (usr[3] - lin.coef[1])/lin.coef[2]
         x2.ster <- (usr[4] - lin.coef[1])/lin.coef[2]
         if (lin.coef[2] == 0)
           { if (x.vals[1] < x.vals[h])
               graphics::mtext(text = ax.aes$names[i], side = 2, line = ax.aes$label.line[i], adj = adjust[2], at = y.vals[1], col = ax.aes$label.col[i], cex = ax.aes$label.cex[i])
             else
               graphics::mtext(text = ax.aes$names[i], side = 4, line = ax.aes$label.line[i], adj = adjust[4], at = y.vals[1], col = ax.aes$label.col[i], cex = ax.aes$label.cex[i])
           }
         if (lin.coef[2] > 0)
           {  if (x.vals[1] < x.vals[h])
                if (y1.ster <= usr[4] & y1.ster >= usr[3])
                  graphics::mtext(text = ax.aes$names[i], side = 2, line = ax.aes$label.line[i], adj = adjust[2], at = y1.ster, col = ax.aes$label.col[i], cex = ax.aes$label.cex[i])
                else
                  graphics::mtext(text = ax.aes$names[i], side = 1, line = ax.aes$label.line[i], adj = adjust[1], at = x1.ster, col = ax.aes$label.col[i], cex = ax.aes$label.cex[i])
              else if (y2.ster <= usr[4] & y2.ster >= usr[3])
                     graphics::mtext(text = ax.aes$names[i], side = 4, line = ax.aes$label.line[i], adj = adjust[4], at = y2.ster, col = ax.aes$label.col[i], cex = ax.aes$label.cex[i])
                   else
                     graphics::mtext(text = ax.aes$names[i], side = 3, line = ax.aes$label.line[i], adj = adjust[3], at = x2.ster, col = ax.aes$label.col[i], cex = ax.aes$label.cex[i])
           }
         if (lin.coef[2] < 0)
           {  if (x.vals[1] < x.vals[h])
                if (y1.ster <= usr[4] & y1.ster >= usr[3])
                  graphics::mtext(text = ax.aes$names[i], side = 2, line = ax.aes$label.line[i], adj = adjust[2], at = y1.ster, col = ax.aes$label.col[i], cex = ax.aes$label.cex[i])
                else
                  graphics::mtext(text = ax.aes$names[i], side = 3, line = ax.aes$label.line[i], adj = adjust[3], at = x2.ster, col = ax.aes$label.col[i], cex = ax.aes$label.cex[i])
              else if (y2.ster <= usr[4] & y2.ster >= usr[3])
                     graphics::mtext(text = ax.aes$names[i], side = 4, line = ax.aes$label.line[i], adj = adjust[4], at = y2.ster, col = ax.aes$label.col[i], cex = ax.aes$label.cex[i])
                   else
                     graphics::mtext(text = ax.aes$names[i], side = 1, line = ax.aes$label.line[i], adj = adjust[1], at = x1.ster, col = ax.aes$label.col[i], cex = ax.aes$label.cex[i])
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
    if (!is.null(predict.mat)) apply(cbind(predict.mat, y.vals[1]), 1, .predict.func, coef = lin.coef,col=ax.aes$predict.col,lwd=ax.aes$predict.lwd,lty=ax.aes$predict.lty)
    }
  }
  #----------
  .bags.plot <- function(z.bags, bag.aes)
  {
    for (i in 1:length(z.bags))
      graphics::polygon (z.bags[[i]], border=bag.aes$col[i], lty=bag.aes$lty[i], lwd=bag.aes$lwd[i])
  }
  #----------
  .conc.ellipse.plot <- function(z.ellipse, ellipse.aes)
  {
    for (i in 1:length(z.ellipse))
      graphics::polygon(z.ellipse[[i]], border=ellipse.aes$col[i], lty=ellipse.aes$lty[i], lwd = ellipse.aes$lwd[i])
  }
  #----------
  .get.ggrepel.coords <- function(df)
  {
    pp <- ggplot2::ggplot (df, aes(df$x,df$y,label=df$z)) + ggplot2::geom_point() + ggrepel::geom_text_repel()
    print (pp)
    xrg <- ggplot2::ggplot_build(pp)$layout$panel_params[[1]]$x.range
    yrg <- ggplot2::ggplot_build(pp)$layout$panel_params[[1]]$y.range
    grid::grid.force()
    kids <- grid::childNames(grid::grid.get("textrepeltree", grep=TRUE))
    textrepels <- grep("textrepelgrob", kids)
    textlines <- kids[-textrepels]
    textlines <- as.numeric(substring(textlines,17,19))
    textvisible <- kids[textrepels]
    textvisible <- as.numeric(substring(textvisible,14,16))
    kids <- kids[textrepels]
    get.xy.pos.labs <- function(n)
    {
      grb <- grid.get(n)
      data.frame(x = xrg[1]+diff(xrg)*grid::convertX(grb$x, "native", valueOnly = TRUE),
                 y = yrg[1]+diff(yrg)*grid::convertY(grb$y, "native", valueOnly = TRUE))
    }
    ggrepel.labs <- do.call (rbind, lapply(kids, get.xy.pos.labs))
    ggrepel.labs$lab <- df$z[textvisible]
    list (coords = ggrepel.labs, visible=textvisible, textlines=textlines)
  }

  #==========
  if (is.null(x$Z)) stop ("Add a biplot method before generating a plot")
  else Z <- x$Z
  
  if (is.null(x$samples)) x <- samples(x)
  if (!is.null(x$class.means)) if(x$class.means) if (is.null(x$means.aes)) x <- means(x)
  if (!is.null(x$Znew)) if (is.null(x$newsamples)) x <- newsamples(x)
  if (!is.null(x$predict.samples)) predict.mat <- Z[x$predict.samples, , drop = F] else predict.mat <- NULL
  if (!is.null(x$predict.means)) predict.mat <- rbind(predict.mat, x$Zmeans[x$predict.means, , drop = F])
  
  if (is.null(x$samples$which)) samples.ggrepel <- FALSE
  else samples.ggrepel <- x$samples$label[1]=="ggrepel"
  newsamples.ggrepel <- FALSE
  if (!is.null(x$Znew)) newsamples.ggrepel <- x$newsamples$label[1]=="ggrepel"
  means.ggrepel <- FALSE
  if (!is.null(x$class.means)) if (x$class.means) means.ggrepel <- x$means.aes$label[1]=="ggrepel"

  do.ggrepel <- samples.ggrepel | means.ggrepel | newsamples.ggrepel
  if (samples.ggrepel)
    {  df <- data.frame (x=Z[,1], y=Z[,2], z=rownames(Z))
       for (i in 1:x$g)
         if (is.na(match(i, x$samples$which))) df$x[x$g.names[i]==x$group.aes] <- NA
       df <- stats::na.omit(df)
  }
  else df <- data.frame (x=NULL, y=NULL, z=NULL)
  n.samples <- nrow(df)
  if (means.ggrepel) df <- rbind (df, data.frame(x=x$Zmeans[x$means.aes$which,1], y=x$Zmeans[x$means.aes$which,2],
                                           z=rownames(x$Zmeans)[x$means.aes$which]))
  n.means <- nrow(df)
  if (newsamples.ggrepel) df <- rbind (df, data.frame(x=x$Znew[,1], y=x$Znew[,2],z=rownames(x$Znew)))
  n.newsamples <- nrow(df)
  ggrepel.new <- ggrepel.means <- ggrepel.samples <- NULL
  if (do.ggrepel)
  {
    out <- .get.ggrepel.coords(df)
    if (n.newsamples>n.means) ggrepel.new <-list(coords = out$coords[out$visible>n.newsamples & out$visible<n.means+1,,drop=F],
                                                 visible = out$visible[out$visible>n.newsamples & out$visible<n.means+1]-n.means,
                                                 textlines = out$textlines[out$textlines>n.newsamples & out$textlines<n.means+1]-n.means)
    if (n.means>n.samples) ggrepel.means <- list(coords = out$coords[out$visible>n.samples & out$visible<n.newsamples+1,,drop=F],
                                                 visible = out$visible[out$visible>n.samples & out$visible<n.newsamples+1]-n.samples,
                                                 textlines = out$textlines[out$textlines>n.samples & out$textlines<n.newsamples+1]-n.samples)
    if (samples.ggrepel) ggrepel.samples <- list(coords = out$coords[out$visible<n.samples+1,,drop=F],
                                                 visible = out$visible[out$visible<n.samples+1],
                                                 textlines = out$textlines[out$textlines<n.samples+1])
  }

  old.par <- graphics::par(pty = "s", ...)
  withr::defer(graphics::par(old.par))

  plot(Z[, 1] * exp.factor, Z[, 2] * exp.factor, xlim = range(Z[, 1] * exp.factor), ylim = range(Z[, 2] * exp.factor),
       xaxt = "n", yaxt = "n", xlab = "", ylab = "", type = "n", xaxs = "i", yaxs = "i", asp = 1)
  usr <- graphics::par("usr")

  if (is.null(x$axes)) x <- axes(x)
  ax.aes <- x$axes
  too.small <- NULL
  if (!is.null(axis.predictivity))
  {
    if(is.null(x$axis.predictivity)) x <- fit.measures(x)
    if(is.numeric(axis.predictivity))
    {
      too.small <- (1:x$p)[x$axis.predictivity<axis.predictivity]
    }
    if(axis.predictivity)
    {
      for (j in 1:length(ax.aes$which))
      {
        ax.num <- ax.aes$which[j]
        ax.col <- ax.aes$col[j]
        ax.aes$col[j] <- colorRampPalette(c("white",ax.col))(101)[round(100*x$axis.predictivity[ax.num])+1]
        ax.col <- ax.aes$label.col[j]
        ax.aes$label.col[j] <- grDevices::colorRampPalette(c("white",ax.col))(101)[round(100*x$axis.predictivity[ax.num])+1]
        ax.col <- ax.aes$tick.col[j]
        ax.aes$tick.col[j] <- grDevices::colorRampPalette(c("white",ax.col))(101)[round(100*x$axis.predictivity[ax.num])+1]
        ax.col <- ax.aes$tick.label.col[j]
        ax.aes$tick.label.col[j] <- grDevices::colorRampPalette(c("white",ax.col))(101)[round(100*x$axis.predictivity[ax.num])+1]
      }
    }
  }


  if (length(ax.aes$which) > 0)
  {
    Xhat <- x$Z %*% solve(x$Lmat)[x$e.vects,]
    if (x$scaled) Xhat <- scale(Xhat, center=FALSE, scale=1/x$sd)
    if (x$center) Xhat <- scale(Xhat, center=-1*x$means, scale=FALSE)

    z.axes <- lapply(1:length(ax.aes$which), .calibrate.axis, Xhat, x$means, x$sd, x$ax.one.unit, ax.aes$which,
                     ax.aes$ticks, ax.aes$orthogx, ax.aes$orthogy)
    .lin.axes.plot(z.axes, ax.aes, predict.mat, too.small)

  }

  too.small <- NULL
  cex.vec <- rep(1, x$n)
  if (!is.null(sample.predictivity) & !inherits(x, "CVA"))
  {
    if(is.null(x$sample.predictivity)) x <- fit.measures(x)
    if(is.numeric(sample.predictivity))
      too.small <- (1:x$n)[x$sample.predictivity<sample.predictivity]
    if(sample.predictivity)
      cex.vec <- x$sample.predictivity
  }

  if (!is.null(x$samples$which)) .samples.plot(Z, x$group.aes, x$samples, x$n, x$g.names, ggrepel.samples,
                                               too.small, cex.vec)

    if (!is.null(x$Znew)) .newsamples.plot (x$Znew, x$newsamples, ggrepel.new)
  if (!is.null(x$class.means)) if (x$class.means)
    {
       if (is.null(x$means.aes)) x <- means(x)
       .means.plot (x$Zmeans, x$means.aes, x$g.names, ggrepel.means)
    }

  if (!is.null(x$alpha.bags)) .bags.plot (x$alpha.bags, x$alpha.bag.aes)

  if (!is.null(x$conc.ellipses)) .conc.ellipse.plot (x$conc.ellipses, x$conc.ellipse.aes)

  if (!is.null(x$Title)) graphics::title(main=x$Title)

  if (!is.null(x$legend)) do.call(biplot.legend, list(bp=x, x$legend.arglist))

  invisible(x)
}

