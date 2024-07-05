
.samples.plot1D.old<- function(bp) 
{
  Z <- bp$Z
  #  which.indices <- 1:bp$n %in% a$samples$which
  for (j in 1:length(bp$g.names)) 
  {  indices <- rep(FALSE, bp$n)
  tmp.indices <- bp$group.aes == bp$g.names[j]
  indices <- tmp.indices #& which.indices
  
  graphics::points(x = Z[indices], y = rep(0, length(Z[indices])),
                   col = bp$samples$col[j], pch = bp$samples$pch[j], cex = bp$samples$cex[j])
  } 
}


.samples.plot1D <- function(Z, samples.aes, group.aes, g.names,  usr)#ggrepel.labs,
{
  group.member <- sapply((group.aes),function(x)which(x==g.names))
  which.indices <- group.member%in%samples.aes$which
  which.indices <- which(which.indices)

  x.vals <- Z[, 1, drop = FALSE]
  y.vals <- rep(0, length(Z))
 # invals <- x.vals < usr[2] & x.vals > usr[1]
  #Z <- Z[invals, , drop = FALSE]
  ZZ <- Z[which.indices,1,drop=FALSE]
  
  {
    Z.labels <- rownames(Z)
    #for (j in 1:length(samples.aes$label.side))
    for (j in (which.indices))
    {
      text.pos <-
        match(samples.aes$label.side[j],
              c("bottom", "left", "top", "right"))
      if (samples.aes$label[j])
        graphics::text(
          Z[j, 1],
          0,
          labels = Z.labels[j],
          cex = samples.aes$label.cex[j],
          col = samples.aes$label.col[j],
          pos = text.pos,
          offset = samples.aes$label.offset[j]
        )
    }
  }

  for (j in 1:length(samples.aes$which)){
    tmp.which <- samples.aes$which[j]
    tmp.ind <- which(group.member==tmp.which)
    graphics::points(
      x = Z[tmp.ind, 1],
      y = rep(0,length(tmp.ind)),
      pch = samples.aes$pch[j],
      col = samples.aes$col[j],
      cex = samples.aes$cex[j]
    )
  }
}

.samples.plot1D.CA <- function(Z, samples.aes, group.aes, g.names,  usr)#ggrepel.labs,
{
  print(group.aes)
  group.member <- sapply((group.aes),function(x)which(x==g.names))
  which.indices <- group.member%in%samples.aes$which
  which.indices <- which(which.indices)
  
  x.vals <- Z[, 1, drop = FALSE]
  y.vals <- rep(0, length(Z))
  ZZ <- Z[which.indices,1,drop=FALSE]
  
  {
    Z.labels <- rownames(Z)
    #for (j in 1:length(samples.aes$label.side))
    for (j in (which.indices))
    {
      text.pos <-
        match(samples.aes$label.side[j],
              c("bottom", "left", "top", "right"))
      if (samples.aes$label[j])
        graphics::text(
          Z[j, 1],
          0,
          labels = Z.labels[j],
          cex = samples.aes$label.cex[j],
          col = samples.aes$label.col[j],
          pos = text.pos,
          offset = samples.aes$label.offset[j]
        )
    }
  }
  
  for (j in 1:length(samples.aes$which)){
    tmp.which <- samples.aes$which[j]
    tmp.ind <- which(group.member==tmp.which)
    graphics::points(
      x = Z[tmp.ind, 1],
      y = rep(0,length(tmp.ind)),
      pch = samples.aes$pch[j],
      col = samples.aes$col[j],
      cex = samples.aes$cex[j]
    )
  }
}

#----------
.density.plot1D <- function(Z.density, density.style) 
{
  for (i in 1:length(Z.density))
    graphics::lines(Z.density[[i]]$x, Z.density[[i]]$y+0.5, col = density.style$col[i], lwd = density.style$lwd[i])
}

#----------
.prediction.regions.plot1D <- function(borders, col,border,bounds=graphics::par('usr'))
{
  graphics::rect(borders[1], 0.5, borders[2], bounds[4], col = col, border = border) 
}
#----------
.means.plot1D <- function(Z, means.aes, g.names,  usr)#ggrepel.labs,
{
  x.vals <- Z[, 1, drop = FALSE]
  y.vals <- rep(0, length(Z))
  invals <- x.vals < usr[2] & x.vals > usr[1]
  Z <- Z[invals, , drop = FALSE]
  ZZ <- Z[means.aes$which,,drop=FALSE]
  
  {
    Z.labels <- rownames(Z)[means.aes$which]
    for (j in 1:length(means.aes$label.side))
    {
      text.pos <-
        match(means.aes$label.side[j],
              c("bottom", "left", "top", "right"))
      if (means.aes$label[j])
        graphics::text(
          ZZ[j, 1],
          0,
          labels = Z.labels[j],
          cex = means.aes$label.cex[j],
          col = means.aes$label.col[j],
          pos = text.pos,
          offset = means.aes$label.offset[j]
        )
    }
  }
  for (j in 1:length(means.aes$which))
    graphics::points(
      x = Z[means.aes$which[j], 1],
      y = 0,
      pch = means.aes$pch[j],
      col = means.aes$col[j],
      cex = means.aes$cex[j]
    )
}

#----------
.bags.plot1D <- function(z.bags, bag.style) 
{
  for (i in 1:length(z.bags))
    graphics::rect(z.bags[[i]][1], -0.25, z.bags[[i]][2], 0.25,
                   col = grDevices::adjustcolor(bag.style$col[i],alpha.f = bag.style$opacity[i]),
                   border = bag.style$col[i],
                   cex = bag.style$lwd[i] * 1.2)
  
}

#----------
.conc.ellipse.plot1D <- function(z.interval, ellipse.style)
{
  for (i in 1:length(z.interval))
    graphics::rect(z.interval[[i]][1], -0.25, z.interval[[i]][2], 0.25,
                   col = grDevices::adjustcolor(ellipse.style$col[i],alpha.f = ellipse.style$opacity[i]),
                   border = ellipse.style$col[i],
                   cex = ellipse.style$lwd[i] * 1.2)
}

#----------
.predict1D <- function(p.point, num.vars, col, lty, lwd) 
{
  for (i in 1:nrow(p.point)) 
    graphics::lines(rep(p.point[i], 2), c(0, -1 * num.vars),
                    col = col, lwd = lwd, lty = lty)
  
}

#----------
.newsamples.plot1D <- function(Z, sample.aes)
{
  graphics::points(x = Z[, 1], y = rep(0, length(Z)),
                   pch = sample.aes$pch, col = sample.aes$col, cex = sample.aes$cex)
}

#----------
.marker.label.cm1D <- function(x, y, grad, marker.val, expand = 1, col, label.on.off,
                             side, pos, offset, label.col, cex, usr)
{
  uin <- graphics::par("pin") / c(usr[2] - usr[1], usr[4] - usr[3])
  mm <- 1 / (uin[2] * 25.4)
  d <- expand * mm
  if (grad == "v")
  {
    graphics::lines(rep(x, 2.5), c(y - d, y + d), col = col)
    if (label.on.off == 1)
      graphics::text(x, y - d*2, label = marker.val, pos = pos, offset = offset,
                     col = label.col, cex = cex)
  }
}

#----------
.marker.func1D <- function(vec, coef, col, tick.size, side, pos, offset, label.col, cex, usr)
{
  x <- vec[1]
  y <- vec[2]
  marker.val <- vec[3]
  label.on.off <- vec[4]
  .marker.label.cm1D(x, y, grad = "v", marker.val, expand = tick.size, col = col,
                   label.on.off = label.on.off, side = side, pos = pos,
                   offset = offset, label.col = label.col, cex = cex, usr = usr)
}

#----------
.lin.axes.plot1D <- function(z.axes, ax.aes, too.small, usr)
{
  for (i in 1:length(ax.aes$which))
  {
    ax.num <- ax.aes$which[i]
    if (!is.null(too.small))
      if (ax.num %in% too.small)
        next
    this.axis <- z.axes[[i]]
    marker.mat <- this.axis$coords
    marker.mat <- marker.mat[rev(order(marker.mat[, 3])),]
    x.vals <- marker.mat[, 1]
    y.vals <- marker.mat[, 2]
    lin.coef <- c(a = this.axis$a, b = this.axis$b)
    #  if (is.null(this.axis$b))
    #    graphics::abline(v = this.axis$v, col = ax.aes$col[i], lwd = ax.aes$lwd[i], lty = ax.aes$lty[i])
    #  else
    graphics::abline(coef = lin.coef, col = ax.aes$col[i], lwd = ax.aes$lwd[i], lty = ax.aes$lty[i])
    if (ax.aes$label.dir == "Hor") {
      graphics::par(las = 1)
      adjust <- c(0.5, 1, 0.5, 0)
    }
    if (ax.aes$label.dir == "Orthog") {
      graphics::par(las = 2)
      adjust <- c(1, 1, 0, 0)
    }
    if (ax.aes$label.dir == "Paral") {
      graphics::par(las = 0)
      adjust <- c(0.5, 0.5, 0.5, 0.5)
    }
    
    h <- nrow(marker.mat)
    if (is.null(this.axis$b))
    {
      if (y.vals[1] < y.vals[h])
        graphics::mtext(text = ax.aes$names[i], side = 1, line = ax.aes$label.line[i],
                        adj = adjust[1], at = x.vals[1], col = ax.aes$label.col[i],
                        cex = ax.aes$label.cex[i])
      else
        graphics::mtext(text = ax.aes$names[i], side = 3, line = ax.aes$label.line[i],
                        adj = adjust[3], at = y.vals[1], col = ax.aes$label.col[i],
                        cex = ax.aes$label.cex[i])
    }
    else
    {
      y1.ster <- lin.coef[2] * usr[1] + lin.coef[1]
      y2.ster <- lin.coef[2] * usr[2] + lin.coef[1]
      x1.ster <- (usr[3] - lin.coef[1]) / lin.coef[2]
      x2.ster <- (usr[4] - lin.coef[1]) / lin.coef[2]
      
      if (lin.coef[2] == 0)
      {
        if (x.vals[1] < x.vals[h])
          graphics::mtext(text = ax.aes$names[i], side = 2, line = ax.aes$label.line[i],
                          adj = adjust[2], at = y.vals[1], col = ax.aes$label.col[i],
                          cex = ax.aes$label.cex[i])
        else
          graphics::mtext(text = ax.aes$names[i], side = 4, line = ax.aes$label.line[i],
                          adj = adjust[4], at = y.vals[1], col = ax.aes$label.col[i],
                          cex = ax.aes$label.cex[i])
      }
      if (lin.coef[2] > 0)
      {
        if (x.vals[1] < x.vals[h])
          if (y1.ster <= usr[4] & y1.ster >= usr[3])
            graphics::mtext(text = ax.aes$names[i], side = 2, line = ax.aes$label.line[i],
                            adj = adjust[2], at = y1.ster, col = ax.aes$label.col[i],
                            cex = ax.aes$label.cex[i])
        else
          graphics::mtext(text = ax.aes$names[i], side = 1, line = ax.aes$label.line[i],
                          adj = adjust[1], at = x1.ster, col = ax.aes$label.col[i],
                          cex = ax.aes$label.cex[i])
        else if (y2.ster <= usr[4] & y2.ster >= usr[3])
          graphics::mtext(text = ax.aes$names[i], side = 4, line = ax.aes$label.line[i],
                          adj = adjust[4], at = y2.ster, col = ax.aes$label.col[i],
                          cex = ax.aes$label.cex[i])
        else
          graphics::mtext(text = ax.aes$names[i], side = 3, line = ax.aes$label.line[i],
                          adj = adjust[3], at = x2.ster, col = ax.aes$label.col[i],
                          cex = ax.aes$label.cex[i])
        
      }
      
      invals <-x.vals < usr[2] & x.vals > usr[1] & y.vals < usr[4] & y.vals > usr[3]
      std.markers <- marker.mat[invals, 3]
      if (is.numeric(std.markers))
        std.markers <- zapsmall(std.markers)
      x.vals <- x.vals[invals]
      y.vals <- y.vals[invals]
      if (ax.aes$tick.label[i])
        label.on.off <- rep(1, sum(invals))
      else
        label.on.off <- rep(0, sum(invals))
      if (!ax.aes$tick.label[i])
        label.on.off[c(1, length(label.on.off))] <- 1
      if (sum(invals) > 0)
        apply(data.frame(x.vals, y.vals, std.markers, label.on.off),
              1, .marker.func1D, coef = lin.coef, col = ax.aes$tick.col[i],
              tick.size = ax.aes$tick.size[i], side = ax.aes$tick.label.side[i],
              pos = ax.aes$tick.label.pos[i], offset = ax.aes$tick.label.offset[i],
              label.col = ax.aes$tick.label.col[i], cex = ax.aes$tick.label.cex[i], usr=usr
        )
 #     if(ax.num %in% x$predict$which)
#      {
#        if (!is.null(predict.mat)) apply(cbind(predict.mat,y.vals[1]), 1, .predict.fun1D, coef = lin.coef,col=ax.aes$predict.col,lwd=ax.aes$predict.lwd,lty=ax.aes$predict.lty)
#      }
      
    }
  }
  
}

#----------
.calibrate.axis1D <- function (j, Xhat, means, sd, axes.rows, ax.which, ax.tickvec,
                             ax.orthogxvec, ax.orthogyvec)
{
  ax.num <- ax.which[j]
  tick <- ax.tickvec[j]
  ax.direction <- axes.rows[ax.num, ]
  r <- ncol(axes.rows)
  ax.orthog <- rbind(ax.orthogxvec, ax.orthogyvec)
  if (nrow(ax.orthog) < r) ax.orthog <- rbind(ax.orthog, 0)
  if (nrow(axes.rows) > 1)phi.vec <- diag(1 / diag(axes.rows %*% t(axes.rows))) %*% axes.rows %*% ax.orthog[, ax.num]
  else phi.vec <-(1 / (axes.rows %*% t(axes.rows))) %*% axes.rows %*% ax.orthog[, ax.num]
  std.ax.tick.label <- pretty(range(Xhat[, ax.num]), n = tick)
  std.range <- range(std.ax.tick.label)
  std.ax.tick.label.min <- std.ax.tick.label - (std.range[2] - std.range[1])
  std.ax.tick.label.max <- std.ax.tick.label + (std.range[2] - std.range[1])
  std.ax.tick.label <- c(std.ax.tick.label, std.ax.tick.label.min, std.ax.tick.label.max)
  interval <- (std.ax.tick.label - means[ax.num]) / sd[ax.num]
  axis.vals <- sort(unique(interval))
  
  number.points <- length(axis.vals)
  axis.points <- matrix(0, nrow = number.points, ncol = r)
  for (i in 1:r) axis.points[, i] <- ax.orthog[i, ax.num] + (axis.vals - phi.vec[ax.num]) * ax.direction[i]
  axis.points <- cbind(axis.points, axis.vals * sd[ax.num] + means[ax.num])
  retvals <- list(coords = axis.points, a = NULL, b = 0, v = NULL)
  return(retvals)
}


plot_CA_1D <- function(bp, exp.factor=1.2, axis.predictivity=NULL, sample.predictivity=NULL,
                       zoom=FALSE, xlim = NULL, ylim = NULL, ...){
  if (is.null(bp$Z)) stop ("Add a biplot method before generating a plot")
  else Z <- bp$Z    
  
  graphics::plot(NA,xlim=range(bp$rowcoor,bp$colcoor)*exp.factor,ylim=0.5*c(-1,1), 
                 axes=FALSE, ann=FALSE)
  graphics::abline(h = 0)
  #.samples.plot1D(bp)
  .samples.plot1D.CA (Z = bp$Z, samples.aes = bp$samples,
                   group.aes =  bp$group.aes, 
                   g.names = bp$g.names, c=bp$c, r=bp$r)
  
}
  
.samples.plot1D.CA <- function(Z, samples.aes, group.aes, g.names, c,r)#ggrepel.labs,
{
  nn <- nrow(Z)
  
  #text.pos <-match(samples.aes$label.side,
  #        c("bottom", "left", "top", "right"))
  #text.pos.len <- length(text.pos)
  #text.pos <- text.pos[ifelse(1:nn%%text.pos.len==0,text.pos.len,1:nn%%text.pos.len)]
  text.pos <- c(rep(1,r),rep(3,c))
  
  label.offset <- samples.aes$label.offset
  label.offset.len <- length(label.offset)
  label.offset <- label.offset[ifelse(1:nn%%label.offset.len==0,label.offset.len,1:nn%%label.offset.len)]
  
  label.cex <- samples.aes$label.cex
  label.cex.len <- length(label.cex)
  label.cex <- label.cex[ifelse(1:nn%%label.cex.len==0,label.cex.len,1:nn%%label.cex.len)]
  
  {
    Z.labels <- rownames(Z)
    for (j in 1:nn)
    {
        graphics::text(
          Z[j, 1],
          0,
          labels = Z.labels[j],
          cex = label.cex[j],
          col = samples.aes$label.col[j],
          pos = text.pos[j],
          offset = label.offset[j]
        )
    }
  }
  
  group.member <- sapply((group.aes),function(x)which(x==g.names))
  for (j in 1:2){
    tmp.ind <- which(group.member==j)
    graphics::points(
      x = Z[tmp.ind, 1],
      y = rep(0,length(tmp.ind)),
      pch = samples.aes$pch[j],
      col = samples.aes$col[j],
      cex = samples.aes$cex[j]
    )
  }
}
  
  