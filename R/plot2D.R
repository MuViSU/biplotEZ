#' Title
#'
#' @param Z Coordinates of points (x,y,label)
#' @param group.aes 
#' @param sample.aes 
#' @param n 
#' @param g.names 
#' @param ggrepel.labs 
#' @param too.small 
#' @param cex.vec 
#'
#' @noRd
.samples.plot <- function(Z, group.aes, sample.aes, n, g.names,
                          ggrepel.labs, too.small, cex.vec, usr=usr,
                          alpha.bag.outside, alpha.bag.aes)
{
  if (sample.aes$connected)
    graphics::lines (Z[,1], Z[,2], col=sample.aes$connect.col, lty=sample.aes$connect.lty, lwd=sample.aes$connect.lwd)
  x.vals <- Z[, 1]
  y.vals <- Z[, 2]
  invals <- x.vals < usr[2] & x.vals > usr[1] & y.vals < usr[4] & y.vals > usr[3]
  if(!any(invals))
    return()
  which.samples <- rep(FALSE, n)
  
  #This is to plot the points outside the alphabag
  if(!is.null(alpha.bag.outside)){
    for (j in 1:length(alpha.bag.aes$which))
      which.samples[group.aes == g.names[sample.aes$which[j]]] <- alpha.bag.outside[[j]]
  }
  else {
    for (j in 1:length(sample.aes$which))
      which.samples[group.aes == g.names[sample.aes$which[j]]] <- TRUE
  }
  
  
  groups <- levels(group.aes)
  
  if (any(stats::na.omit(sample.aes$label=="ggrepel")))
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
      graphics::text(ggrepel.labs$coords[j, 1], ggrepel.labs$coords[j, 2], labels = sample.aes$label.name[j],
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
    ZZ <- data.frame (no=1:n, names=sample.aes$label.name, label=sample.aes$label,
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
    points (x=ZZ[i,1], y=ZZ[i,2], pch=ZZ.points$pch[i],
            col=ZZ.points$col[i],
            cex=ZZ.points$cex.vec[i]*ZZ.points$cex[i])
}



#' CA plot
#'
#' @param rowcoor row coordinates
#' @param colcoor column coordinates
#' @param group.aes factor aesthetics
#' @param sample.aes sample aesthetics
#' @param r number of row levels
#' @param c number of column nlevels
#' @param g.names factor names
#'
#' @noRd
.CA.plot <- function(rowcoor, colcoor, group.aes, sample.aes, r, c, g.names)
{
  #first factor rowcoor
  graphics::points(x = rowcoor[,1], y = rowcoor[,2], pch = sample.aes$pch[1],
                   col = sample.aes$col[1], cex = sample.aes$cex[1])
  text.pos <- match(sample.aes$label.side[1], c("bottom", "left", "top", "right"))
  graphics::text(x = rowcoor[,1], y = rowcoor[,2], labels = rownames(rowcoor), 
                 col = sample.aes$col[1], cex = sample.aes$label.cex[1],
                 pos = text.pos, offset = sample.aes$label.offset)
  #second factor colcoor
  graphics::points(x = colcoor[,1], y = colcoor[,2], pch = sample.aes$pch[2],
                   col = sample.aes$col[2], cex = sample.aes$cex[2])
  text.pos <- match(sample.aes$label.side[2], c("bottom", "left", "top", "right"))
  graphics::text(x = colcoor[,1], y = colcoor[,2], labels = rownames(colcoor),
                 col = sample.aes$col[2], cex = sample.aes$label.cex[2],
                 pos = text.pos, offset = sample.aes$label.offset)
}

#' Title
#'
#' @param Z 
#' @param sample.aes 
#' @param g.names 
#' @param ggrepel.labs 
#'
#'
#' @noRd
.means.plot <- function(Z, sample.aes, g.names, ggrepel.labs,usr)
{
  x.vals <- Z[, 1]
  y.vals <- Z[, 2]
  invals <- x.vals < usr[2] & x.vals > usr[1] & y.vals < usr[4] & y.vals > usr[3]
  if(!any(invals))
    return()
  ZZ <- Z[sample.aes$which,]

  toetsers<-invals[sample.aes$which]
  if (sample.aes$label[1]=="ggrepel")
  {
    for (j in 1:nrow(ggrepel.labs$coords)){
      graphics::text(ggrepel.labs$coords[j, 1], ggrepel.labs$coords[j, 2], 
                     labels = ggrepel.labs$coords[j,3],
                     cex = sample.aes$label.cex[ggrepel.labs$visible[j]],
                     col = sample.aes$label.col[ggrepel.labs$visible[j]])
    }
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
    { if(!toetsers[j])
        next
      text.pos <- match(sample.aes$label.side[j], c("bottom", "left", "top", "right"))
    if (sample.aes$label[j]) graphics::text(ZZ[j, 1], ZZ[j, 2], labels = Z.labels[j],
                                            cex = sample.aes$label.cex[j], col = sample.aes$label.col[j],
                                            pos = text.pos, offset = sample.aes$label.offset[j])
    }
  }
  for (j in 1:length(sample.aes$which)){
    if(!toetsers[j])
      next
    graphics::points(x = Z[sample.aes$which[j], 1], y = Z[sample.aes$which[j], 2],
                     pch = sample.aes$pch[j], col = sample.aes$col[j], cex = sample.aes$cex[j])
  }
}

#' Title
#'
#' @param Z 
#' @param sample.aes 
#' @param ggrepel.labs 
#'
#' @noRd
.newsamples.plot <- function(Z, sample.aes, ggrepel.labs, usr)
{
  if(is.null(sample.aes$label.name[1]))
    label.names<-rownames(Z)
  else
    label.names<-sample.aes$label.name
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
    if (sample.aes$label[j]) graphics::text(Z[j, 1], Z[j, 2], labels = label.names[j],
                                            cex = sample.aes$label.cex[j], col = sample.aes$label.col[j],
                                            pos = text.pos, offset = sample.aes$label.offset[j])
    }
  }
  graphics::points(x = Z[, 1], y = Z[, 2], pch = sample.aes$pch, col = sample.aes$col,
                   cex = sample.aes$cex)
}

#' Title
#'
#' @param p.point 
#' @param coef 
#' @param col 
#' @param lty 
#' @param lwd 
#'
#' @noRd
.predict.func <- function(p.point, coef, col, lty, lwd) {
  
  if (is.na(coef[2])) 
    graphics::lines(c(p.point[1], coef[1]), rep(p.point[2], 2), 
                    col = col, lwd = lwd, lty = lty)
  else if (coef[2] == 0) 
    graphics::lines(rep(p.point[1], 2), p.point[2:3], 
                    col = col, lwd = lwd, lty = lty)
  else 
  {  intercept.projection <- p.point[2] + p.point[1]/coef[2]
     project.on.x <- (intercept.projection - coef[1])/(coef[2] + 1/coef[2])
     project.on.y <- coef[1] + coef[2] * project.on.x
     graphics::lines(c(p.point[1], project.on.x), c(p.point[2], project.on.y), 
                  col = col, lwd = lwd, lty = lty)
  }
}


#----------
#' Title
#'
#' @param j Index of axis to calibrate
#' @param Xhat Matrix of predicted values
#' @param means vector: column means of X
#' @param sd vector: column standard deviations
#' @param axes.rows 
#' @param ax.which 
#' @param ax.tickvec 
#' @param ax.orthogxvec 
#' @param ax.orthogyvec 
#'
#' @noRd
.calibrate.axis <- function (j, Xhat, means, sd, 
                             axes.rows, ax.which, ax.tickvec,
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



#' Insert tick marks and label - called by .marker.func
#'
#' @param x x-coo of tick
#' @param y y-coo
#' @param grad gradient of axis
#' @param marker.val marker label
#' @param expand expansion factor of tick. Numeric >0
#' @param col color tickmark
#' @param label.on.off 1 to display label, 0 otherwise
#' @param side REMOVE
#' @param pos REMOVE
#' @param offset REMOVE
#' @param label.col color of label
#' @param cex Bigness of label
#' @param label_pos 1 for above -1 for below
#'
#' @return inserts tick marks and label to plot
#' @noRd
.marker.label <- function(x, y, grad, marker.val, 
                             expand = 1, col, label.on.off, 
                             label.col, cex,
                             label_pos=-1,usr)
{
  
  #get fraction of plot width and expand
  mm<- 0.007*c(usr[2] - usr[1])
  d <- expand * mm
  if (grad == "v"){
    graphics::lines(rep(x, 2), c(y - d, y + d), col = col)
    if (label.on.off == 1) 
      graphics::text(x, y - label_pos(d+2.5*mm), label = marker.val,
                     col = label.col, 
                     cex = cex)
  }
  if (grad == "h"){
    graphics::lines(c(x - d, x + d), rep(y, 2), col = col)
    if (label.on.off == 1) 
      graphics::text(x - label_pos*(d+2.5*mm), y, label = marker.val, 
                     col = label.col, cex = cex,
                     srt=90)
  }
  if (is.numeric(grad)){
    
    #calculate tick half-width
    b <- d * sqrt(1/(1 + grad * grad))
    a <- b * grad
    
    x_shift<-mm * sqrt(1/(1 + grad * grad))
    y_shift<-x_shift*grad
    
    graphics::lines(c(x - b, x + b), c(y - a, y + a), col = col)
    
    if (label.on.off == 1){
      if(grad>0)
        graphics::text(x+label_pos*(2.5*x_shift+b), 
                       y+label_pos*(2.5*y_shift+a), 
                       label = marker.val, 
                       col = label.col,
                       cex = cex,srt=180+(90+atan(grad)*180/pi))
      
      if(grad<0)
        graphics::text(x-label_pos*(2.5*x_shift+b), y-label_pos*(2.5*y_shift+a), 
                       label = marker.val, 
                       col = label.col,
                       cex = cex,srt=(90+atan(grad)*180/pi))
    }
    
  }
}



#' Main function to insert markers to plot
#'
#' @param vec vector: = (x_coo, y_coo, marker val, show)
#' @param coef (intercept, slope) of axis
#' @param col Color axis
#' @param tick.size Expansion factor tick
#' @param side REMOVE
#' @param pos REMOVE
#' @param offset REMOVE
#' @param label.col color label
#' @param cex cex of label
#'
#' @return prints markers to plot
#' @noRd
.marker.func <- function(vec, coef, col, tick.size, 
                         label_pos, label.col, cex,
                         usr)
{

  if(label_pos=="below")
    label_pos<--1
  if(label_pos=="above")
    label_pos<- 1
  

  ## vec = [x_coo, y_coo, marker val, show]
  ## coef = [intercept, slope]
  x <- vec[1]
  y <- vec[2]
  marker.val <- vec[3]
  label.on.off <- vec[4]
  if (is.na(coef[2]))
    .marker.label(x, y, grad = "h", marker.val, 
                     expand = tick.size, col = col, 
                     label.on.off = label.on.off, 
                     label.col = label.col, 
                     cex = cex,usr=usr, label_pos=label_pos)
  
  else if (coef[2] == 0) # line is verticle
    .marker.label(x, y, grad = "v", marker.val, 
                     expand = tick.size, col = col, 
                     label.on.off = label.on.off,
                     label.col = label.col,
                     cex = cex, usr=usr,label_pos=label_pos)
  else
    .marker.label(x, y, grad = -1/coef[2], marker.val, 
                     expand = tick.size, col = col, 
                     label.on.off = label.on.off, 
                     label.col = label.col, 
                     cex = cex, usr=usr,label_pos=label_pos)
}




#' Plot linear axes on biplots
#'
#' @param z.axes list containing all the info to draw axis. see below
#' @param ax.aes Axis aestetics
#' @param predict.mat Xhat
#' @param too.small cutoff: predictivity smaller than cutoff not plotted
#' @param usr plot dim
#'
#' z.axes:    List containing following components for each axis:
#' $coords:   Tick mark coordinates
#' $a $b $v:  Intercept, slope, vertical
#' 
#' @noRd
.lin.axes.plot <- function(z.axes, ax.aes, predict.mat, 
                           too.small, usr, predict_which)
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
                          label.col = ax.aes$tick.label.col[i], label_pos=ax.aes$tick.label.side[i],
                          cex = ax.aes$tick.label.cex[i], usr=usr)
  
  
  if(ax.num %in% predict_which)
  {
    if (!is.null(predict.mat)) apply(cbind(predict.mat,y.vals[1]), 1, .predict.func, coef = lin.coef,col=ax.aes$predict.col,lwd=ax.aes$predict.lwd,lty=ax.aes$predict.lty)
  }
  
  }
}

.CLPs.plot <- function (coords, colvec, cexvec)
{
  text (x=coords[,1], y=coords[,2], labels=rownames(coords), col=colvec, cex=cexvec)
}

#' Vectors to represent variables
#'
#' @param Vr V matrix.
#' @param ax.aes axes aesthetics
#'
#'
#' @noRd
.lin.axes.vector.plot <- function(Vr,ax.aes)
{
  for (i in 1:length(ax.aes$which)) 
  {
    graphics::arrows(0,0,Vr[i,1],Vr[i,2],
                     lwd=2,length = 0.1,col="black")
    # graphics::text(Vr[i,1],Vr[i,2],ax.aes$names[i],
    #                 col="black",cex=0.8,
    #                 pos=1)
    if(ax.aes$unit.circle) plotrix::draw.circle(0,0,1,lty=2)
  }

}

#' Title
#'
#' @param z.bags 
#' @param bag.aes 
#'
#' @noRd
.bags.plot <- function(z.bags, bag.aes)
{
  for (i in 1:length(z.bags))
    graphics::polygon (z.bags[[i]], border=bag.aes$col[i], lty=bag.aes$lty[i], lwd=bag.aes$lwd[i], 
                       col=grDevices::adjustcolor(bag.aes$col[i],bag.aes$opacity[i]))
}



#' Title
#'
#' @param z.ellipse 
#' @param ellipse.aes 
#'
#' @noRd
.conc.ellipse.plot <- function(z.ellipse, ellipse.aes)
{
  for (i in 1:length(z.ellipse))
    graphics::polygon(z.ellipse[[i]], border=ellipse.aes$col[i], lty=ellipse.aes$lty[i], lwd = ellipse.aes$lwd[i], 
                      col=grDevices::adjustcolor(ellipse.aes$col[i],ellipse.aes$opacity[i]))
}




#' Get coordinates from ggrepel
#'
#' @param df dataframe containing (x_coo, y_coo, marker)
#'
#' @noRd
.get.ggrepel.coords <- function(df)
{
  pp <- ggplot2::ggplot (df, ggplot2::aes(x,y,label=z)) + ggplot2::geom_point() + ggrepel::geom_text_repel()
  xrg <- ggplot2::ggplot_build(pp)$layout$panel_params[[1]]$x.range
  yrg <- ggplot2::ggplot_build(pp)$layout$panel_params[[1]]$y.range
  print(pp)
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
    grb <- grid::grid.get(n)
    data.frame(x = xrg[1]+diff(xrg)*grid::convertX(grb$x, "native", valueOnly = TRUE),
               y = yrg[1]+diff(yrg)*grid::convertY(grb$y, "native", valueOnly = TRUE))
  }
  ggrepel.labs <- do.call (rbind, lapply(kids, get.xy.pos.labs))
  ggrepel.labs$lab <- df$z[textvisible]
  list (coords = ggrepel.labs, visible=textvisible, textlines=textlines)
}



#' Title
#'
#' @param Z.density 
#' @param density.style 
#'
#' @noRd
.density.plot <- function(Z.density, density.style) 
{
  levels.rect <- pretty(range(Z.density$z), n = density.style$cuts)
  col.use <- colorRampPalette(density.style$col)
  col.use <- col.use(length(levels.rect) - 1)
  graphics::image(Z.density, breaks = levels.rect, col = col.use, add = TRUE)
  if (density.style$contours) 
    graphics::contour(Z.density, levels = levels.rect, col = density.style$contour.col, add = TRUE)
  list(levels.rect, col.use)
}


#' Plot spline based axes on biplots
#'
#' @param j 
#' @param X 
#' @param Ytilde 
#' @param means 
#' @param sd 
#' @param n.int 
#' @param spline.control 
#' @param dmeth 
#' @param ... 
#' 
#' @useDynLib biplotEZ, .registration = TRUE
#'
#'
#' @noRd
biplot.spline.axis <- function(j, X, Ytilde, means, sd, n.int, spline.control, dmeth=0, ... )
{
  n <- nrow(X)
  p <- ncol(X)
  if (n > 103)
    {  
      my.sample <- sample (1:n, size=103, replace=F)
      X <- X[my.sample,]
      Ytilde <- Ytilde[my.sample,]
      n <- nrow(X)
    }
  
  tau <- spline.control$tau
  nmu <- spline.control$nmu
  u <- spline.control$u
  v <- spline.control$v
  lambda <- spline.control$lambda
  smallsigma <- spline.control$smallsigma
  bigsigma <- spline.control$bigsigma
  gamma <- spline.control$gamma
  bigsigmaactivate <- spline.control$bigsigmaactivate
  eps <- spline.control$eps
  tiny <- spline.control$tiny
  itmax <- spline.control$itmax
  ftol <- spline.control$ftol
  
  cat ("Calculating spline axis for variable", j, "\n")
  if(dmeth==1) stop("dmeth should be equal to zero or integer greater than 1 \n")  
  Y <- scale(Ytilde,center=means,scale=sd)
  
  ytilde <- Ytilde[,j]
  mutilde <- seq(from=min(ytilde),to=max(ytilde),length.out=nmu)
  y <- Y[,j]
  rangey <- max(y)-min(y)
  mu <- seq(from=min(y)-.3*rangey,to=max(y)+.3*rangey,length.out=nmu)
  markers <- (pretty(ytilde)-means[j])/sd[j]
  mu <- sort(c(mu,markers))
  mu <- unique(mu)
  nmu <- length(mu)
  
  if (v>0)
  {
    knots <- seq.int(from=0,to=1,length.out=v+2)[-c(1,v+2)]
    knots <- stats::quantile(y,knots)
    M <- splines::bs(mu,knots=knots,degree=u,intercept=FALSE)
  } else M <- splines::bs(mu,df=u+v,degree=u,intercept=FALSE)
  M <- scale(M,scale=FALSE,center=M[which.min(abs(mu)),]) # To ensure that the spline passes through the origin at the calibration which represents the mean of the variable
  Breg <- t(solve(t(X)%*%X)%*%t(X)%*%y)
  Zreg <- mu%*%Breg/sum(Breg^2)
  Bvec <- as.vector(solve(t(M)%*%M)%*%t(M)%*%Zreg)  # Closest to regression biplot
  
  const1 <- sum(y^2)
  const2 <- sum(X^2)/(n*p)
  TotalNumberOfLossFunctionCalls <- 0
  
  optimtouse <- function(Bvec)
  {
    timetemp <- proc.time()[3]
    LOSS <- 1.0
    LOSS1 <- 1.0
    Ind <- rep(1,n)
    pred <- rep(0,nmu)
    deltmp <- 0
    tau <- tau
    #.5 # the choice of tau seems to affect perfomance quite substantially.
    # tau is used to specify the points on the inital simplex.
    Ay <- rep(0,(u+v)*p+1)
    TEMPVK <- rep(0,(u+v)*p)
    iter1 <- 0
    iter <- 0
    ERRO <- 0
    
    # Prepare for Fortran subroutine
    storage.mode(X) <- "double"
    storage.mode(Ind) <- "integer"
    storage.mode(mu) <- "double"
    storage.mode(pred) <- "double"
    storage.mode(y) <- "double"
    storage.mode(M) <- "double"
    storage.mode(Bvec) <- "double"
    storage.mode(Ay) <- "double"
    storage.mode(TEMPVK) <- "double"
  
    
    returned_data <-.Fortran('L',LOSS=as.double(LOSS),X=X,n=as.integer(n),p=as.integer(p),nmu=as.integer(nmu),Ind=Ind,
                            mu=mu,pred=pred,lambda=as.double(lambda),y=y,const1=as.double(const1),const2=as.double(const2),u=as.integer(u),
                            v=as.integer(v),M=M,Bvec=Bvec,tau=as.double(tau),Ay=Ay,TEMPVEK=TEMPVK,iter=as.integer(iter),
                            ftol=as.double(ftol),LOSS1=as.double(LOSS1),iter1=as.integer(iter1),fout = as.integer(ERRO),
                            const3=as.double(tiny), itmax=as.integer(itmax))
    if(returned_data$fout > 0)
    {
      cat("Fout is: ", returned_data$fout, "\n")
      warning("Increase itmax for Fortran \n")
    }
    
    B <- matrix(returned_data$Bvec,ncol=p)
    Z <- M%*%B 
    
    aa <- list(BestValue=returned_data$LOSS,BestSolution=returned_data$Bvec,ConvergenceCode=returned_data$fout, iter1=returned_data$iter1,
             iter=returned_data$iter,TimeTaken=proc.time()[3]-timetemp)
    aa
  }
  EuclidDist2 <- function (X, Y) 
  {
    n <- nrow(X)
    m <- nrow(Y)
    bx <- rowSums(X^2)
    by <- rowSums(Y^2)
    outer(bx, by, FUN = "+") - 2 * X %*% t(Y)
  }
  
  ### Variable initialisation
  outBestValues <- rep(NA,gamma+1)
  outBestSolutions <- matrix(nrow=2*(u+v),ncol=gamma+1)
  outTimeTaken <- rep(NA,gamma+1) # Is made one element longer at each iteration.
  BestSolutionsFrequency <- rep(NA,gamma+1)
  BestSolutionsIndices <- rep(NA,gamma+1) # Is made one element longer at each iteration.
  SquaredDistancesBetweenBestSolutions <- matrix(nrow=gamma+1,ncol=gamma+1)
  
  ### Initial coefficients closest to regression biplot
  temp <- optimtouse(Bvec)
  outBestValues[1] <- temp$BestValue
  outBestSolutions[,1] <- temp$BestSolution
  outTimeTaken[1] <- temp$TimeTaken
  BestSolutionsFrequency[1] <- 1
  BestSolutionsIndices[1] <- 1
  DistinctSolutions <- 1
  PreviousBestSolution <- NA
  nSameSolutionConsecutively <- 0
  BigSigmaActivations <- NULL
  
  test.iter <- temp$iter
  test.iter1 <- temp$iter1
  
  ### Last best coefficients perturbed
  for (gammacounter in 2:(gamma+1))
  {
    if (nSameSolutionConsecutively>=bigsigmaactivate)
    {
      temp <- optimtouse(outBestSolutions[,which.min(outBestValues)]+stats::rnorm((u+v)*2,mean=0,sd=bigsigma))
      BigSigmaActivations <- c(BigSigmaActivations,gammacounter)
    }
    else temp <- optimtouse(outBestSolutions[,which.min(outBestValues)]+stats::rnorm((u+v)*2,mean=0,sd=smallsigma))
    outTimeTaken[gammacounter] <- temp$TimeTaken
    tempSquaredDistances <- EuclidDist2(matrix(temp$BestSolution,nrow=1),t(outBestSolutions[,1:DistinctSolutions]))
    if (any(tempSquaredDistances<eps))
    {
      BestSolutionsIndices[gammacounter] <- tempAA<-which.min(tempSquaredDistances)
      BestSolutionsFrequency[tempAA] <- BestSolutionsFrequency[tempAA]+1
      if (!is.na(PreviousBestSolution) && tempAA==PreviousBestSolution) nSameSolutionConsecutively<-nSameSolutionConsecutively+1
      else
      {
        PreviousBestSolution <- tempAA
        nSameSolutionConsecutively <- 0
      }
    }
    else
    {
      DistinctSolutions <- DistinctSolutions+1
      outBestValues[DistinctSolutions] <- temp$BestValue
      outBestSolutions[,DistinctSolutions] <- temp$BestSolution
      BestSolutionsFrequency[DistinctSolutions] <- 1
      BestSolutionsIndices[gammacounter] <- DistinctSolutions
      SquaredDistancesBetweenBestSolutions[1:(DistinctSolutions-1),DistinctSolutions]<-tempSquaredDistances
      nSameSolutionConsecutively <- 0
    }
  }
  axis.points <- cbind(M%*%matrix(outBestSolutions[,which.min(outBestValues)],ncol=2), mu, 0)
  
  for (i in 1:nrow(axis.points)) if (any(zapsmall(axis.points[i,3] - markers) == 0)) axis.points[i, 4] <- 1
  axis.points[,3] <- axis.points[,3]*sd[j] + means[j]
  axis.points
}


#' Plot nonlinear axes on biplots
#'
#' @param z.axes list containing all the info to draw axis.
#' @param ax.style Axis aestetics
#' @param predict.mat Matrix of sample points to predict
#' @param too.small cutoff: predictivity smaller than cutoff not plotted
#' @param usr plot dim
#' @param x x
#'
#' @noRd
.nonlin.axes.plot <- function(z.axes, ax.style, predict.mat,too.small, usr, x=x) 
{
  for (i in 1:length(ax.style$which))
  {
    ax.num <- ax.style$which[i]
    axis.mat <- z.axes[[i]]
    axis.mat <- axis.mat[rev(order(axis.mat[, 3])),]
    x.vals <- axis.mat[, 1]
    y.vals <- axis.mat[, 2]
    xy.before <- rbind(axis.mat[-1, 1:2], axis.mat[nrow(axis.mat), 1:2])
    xy.after <- rbind(axis.mat[1, 1:2], axis.mat[-nrow(axis.mat), 1:2])
    coef.mat <- matrix(NA, nrow = nrow(axis.mat), ncol = 2)
    for (j in 1:nrow(axis.mat))
      coef.mat[j, ] <-
      stats::coefficients(stats::lm(c(xy.after[j, 2], xy.before[j, 2]) ~ c(xy.after[j, 1], xy.before[j, 1])))
    invals <-
      x.vals < usr[2] &
      x.vals > usr[1] & y.vals < usr[4] & y.vals > usr[3]
    axis.mat <- axis.mat[invals, , drop = F]
    coef.mat <- coef.mat[invals, , drop = F]
    lines(
      axis.mat[, 1],
      axis.mat[, 2],
      col = ax.style$col[i],
      lwd = ax.style$lwd[i],
      lty = ax.style$lty[i]
    )
    if (ax.style$tick.label.side[i] == "below")
      label.pos <- 1
    #if (ax.style$tick.label.side[i] == "left")
    #  label.pos <- 2
    if (ax.style$tick.label.side[i] == "above")
      label.pos <- 3
    #if (ax.style$tick.label.side[i] == "right")
    #  label.pos <- 4
    if (nrow(axis.mat) > 0)
    {
      text(x = axis.mat[1, 1], y = axis.mat[1, 2],
        label = ax.style$names[i],
        offset = 0.1, # ax.style$label.dist[i],
        pos = label.pos,
        col = ax.style$label.col[i],
        cex = ax.style$label.cex[i])
    }
      
    marker.mat <- axis.mat[axis.mat[, 4] == 1, 1:3, drop = F]
    if (nrow(marker.mat) > 0) {
      marker.mat[, 3] <- zapsmall(marker.mat[, 3])
      coef.mat <- coef.mat[axis.mat[, 4] == 1, , drop = F]
      if (ax.style$tick.label[i])
        label.on.off <-
        rep(1, nrow(marker.mat))
      else
        rep(0, nrow(marker.mat))
      if (!ax.style$tick.label[i])
        label.on.off[c(1, length(label.on.off))] <- 1
      for (j in 1:nrow(marker.mat))
        .marker.func (
          c(marker.mat[j, ], label.on.off[j]),
          coef = coef.mat[j, ],
          col = ax.style$tick.col[i],
          tick.size = ax.style$tick.size[i],
          label_pos = ax.style$tick.label.side[i],
          #pos = ax.style$tick.label.pos[i],
          #offset = ax.style$tick.label.offset[i],
          label.col = ax.style$tick.label.col[i],
          cex = ax.style$tick.label.cex[i],
          usr = usr
        )
    }
  }
}