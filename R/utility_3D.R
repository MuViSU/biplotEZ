.samples.plot3d <- function(Z, group.aes, sample.aes, n, g.names,
                            too.small, cex.vec, usr=usr) {
  x.vals <- Z[, 1]
  y.vals <- Z[, 2]
  z.vals <- Z[, 3]
  
  invals <- x.vals < usr[2] & x.vals > usr[1] & y.vals < usr[4] & y.vals > usr[3] & z.vals < usr[6] & z.vals > usr[5]
  
  which.samples <- rep(FALSE, n)
  for (j in 1:length(sample.aes$which))
    which.samples[group.aes == g.names[sample.aes$which[j]]] <- TRUE
  groups <- levels(group.aes)
  
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
  # ZZ <- ZZ[usr$invals[which.samples],]
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
    rgl::text3d(ZZ[j, 1], ZZ[j, 2],ZZ[j,3], labels = ZZ.labels$names[j],
                   cex = ZZ.labels$label.cex[j], col = ZZ.labels$label.col[j],
                   pos = text.pos, offset = ZZ.labels$label.offset[j])
  }
  
  for (i in 1:nrow(ZZ.points))
    rgl::points3d(x=ZZ[i,1], y=ZZ[i,2], z=ZZ[i,3], pch=ZZ.points$pch[i],
                  col=ZZ.points$col[i], radius=ZZ.points$cex.vec[i]*ZZ.points$cex[i])
}


.abline3d <- function (mat, eps=1e-8, stretch.amount=0.05, ...)  {         
  # mat:2x3 contains two coordinates on the line
  # z.vec = a.vec + r*b.vec
  #   a.vec is point on the line
  #   b.vec is direction vector
  # symmetric form
  #   (z1-a1)/b1 = (z2-a2)/b2 = (z3-a3)/b3
  
  stretch.order <- order(diff(apply(mat,2,range)))
  mat <- mat[,stretch.order]
  a.vec <- mat[1,]
  b.vec <- mat[2,]-mat[1,]
  
  z1 <- zz1 <- z2 <- zz2 <- z3 <- zz3 <- 0
  if (all(abs(b.vec)>eps))
  {  min3 <- min(mat[,3])
  max3 <- max(mat[,3])
  z3 <- min3-(max3-min3)*stretch.amount
  zz3 <- max3+(max3-min3)*stretch.amount
  z2 <- (b.vec[2]/b.vec[3])*(z3-a.vec[3])+a.vec[2]
  zz2 <- (b.vec[2]/b.vec[3])*(zz3-a.vec[3])+a.vec[2]
  z1 <- (b.vec[1]/b.vec[3])*(z3-a.vec[3])+a.vec[1]
  zz1 <- (b.vec[1]/b.vec[3])*(zz3-a.vec[3])+a.vec[1]
  }
  if (abs(b.vec[1])<eps & abs(b.vec[2])>eps)
  {  min3 <- min(mat[,3])
  max3 <- max(mat[,3])
  z3 <- min3-(max3-min3)*stretch.amount
  zz3 <- max3+(max3-min3)*stretch.amount
  z2 <- (b.vec[2]/b.vec[3])*(z3-a.vec[3])+a.vec[2]
  zz2 <- (b.vec[2]/b.vec[3])*(zz3-a.vec[3])+a.vec[2]
  z1 <- zz1 <- mat[1,1]
  }
  if (abs(b.vec[1])<eps & abs(b.vec[2])<eps)
  {  min3 <- min(mat[,3])
  max3 <- max(mat[,3])
  z3 <- min3-(max3-min3)*stretch.amount
  zz3 <- max3+(max3-min3)*stretch.amount
  z2 <- zz2 <- mat[1,2]
  z1 <- zz1 <- mat[1,1]
  }
  mat <- rbind (c(z1,z2,z3),c(zz1,zz2,zz3))
  mm <- mat[,order(stretch.order)]
  rgl::lines3d(x=mm[,1],y=mm[,2],z=mm[,3],...)
  list (a=a.vec[order(stretch.order)], b=b.vec[order(stretch.order)], axis.mat=mm)
}


.lin.axes.plot3d <- function(bp, z.axes, ax.aes, predict.mat,usr) {
  
  rgl.scale <- (usr[2] - usr[1])/50
  for (i in 1:length(ax.aes$which)) 
  {  
    ax.num <- ax.aes$which[i]
    marker.dat <- z.axes[[ax.num]]$coords
    marker.mat <- matrix (nrow=nrow(marker.dat),ncol=ncol(marker.dat))
    for (j in 1:ncol(marker.mat)) marker.mat[,j] <- as.numeric(marker.dat[,j])
    invals <- marker.mat[,1] < usr[2] & marker.mat[,1] > usr[1] &
    marker.mat[,2] < usr[2] & marker.mat[,2] > usr[1] &
    marker.mat[,3] < usr[2] & marker.mat[,3] > usr[1]
    if (sum(invals)<2) {  dd <- as.matrix(stats::dist(rbind (0, marker.mat[,1:3])))[1,-1]
      mid.vals <- order(dd)
    invals[mid.vals] <- TRUE
    }
    marker.mat <- marker.mat[invals,]
    marker.mat <- marker.mat[rev(order(marker.mat[, 4])), ]
    std.markers <- marker.mat[,4]
    if (is.numeric(std.markers)) std.markers <- zapsmall(std.markers)
    marker.mat <- marker.mat[,1:3]
    h <- nrow(marker.mat)
  
    ab <- .abline3d(marker.mat[c(1,h),],col = ax.aes$col[i], lwd = ax.aes$lwd[i]) # no lty for rgl
    away <- abs(ab$axis.mat[1,]-ab$axis.mat[2,])*0.01  # + ax.aes$label.dist[i]
    
    # find which side is ab$axis.mat is closest to highest marker
    dd <- as.matrix(stats::dist(rbind (marker.mat[1,],ab$axis.mat)))
    if (dd[1,2]<dd[1,3]) 
      label.coord <- ab$axis.mat[1,] - away*ab$b/sqrt(sum(ab$b^2)) 
    else 
      label.coord <- ab$axis.mat[2,] + away*ab$b/sqrt(sum(ab$b^2))

    rgl::text3d(label.coord, text = ax.aes$names[i], cex=0.8)  
    rgl::points3d(marker.mat, col=ax.aes$tick.col[i])
    if (ax.aes$tick.label[i]) 
    {
      if (ax.aes$tick.label.side[i]=="left") label.coord <- marker.mat - rgl.scale
      else label.coord <- marker.mat + rgl.scale
      rgl::text3d (label.coord, texts=std.markers, col=ax.aes$tick.label.col[i], cex=ax.aes$tick.label.cex[i],
            pos = ax.aes$tick.label.pos[i], offset = ax.aes$tick.label.offset[i])
    }
    
    if(ax.num %in% bp$predict$which.var)
    {
      if (!is.null(predict.mat)) apply(predict.mat, 1, .predict.func3d, b=ab$b, 
                                       col = ax.aes$predict.col[i], lwd = ax.aes$predict.lwd[i])
    }
    

  }
}


.predict.func3d <- function(p.point, b, col, lwd) {
  proj <- (sum(p.point*b)/sum(b^2))*b
  ab <- .abline3d (rbind (p.point,proj),stretch.amount=0, col=col, lwd=lwd)
}


.new.samples.plot3d <- function(Z.new, new.sample.style) {
  rgl::points3d(Z.new[, 1], Z.new[, 2], Z.new[, 3], col = new.sample.style$col,
            alpha = new.sample.style$alpha)
  text.adj <- c(0.5, 0.5)
  if (new.sample.style$label.side[1] == "bottom") text.adj[2] <- 1
  if (new.sample.style$label.side[1] == "top") text.adj[2] <- 0
  if (new.sample.style$label.side[1] == "left") text.adj[1] <- 0
  if (new.sample.style$label.side[1] == "right") text.adj[1] <- 1
  if (any(new.sample.style$label)) rgl::text3d(Z.new[new.sample.style$label, 1], Z.new[new.sample.style$label, 2], Z.new[new.sample.style$label, 3], 
                                          text = dimnames(Z.new)[[1]][new.sample.style$label], cex = new.sample.style$label.cex[new.sample.style$label], 
                                          adj = text.adj)
}

.bags.plot3d <- function(z.bags, bag.style) {
  for (i in 1:length(z.bags)) 
  {  mat <- cbind(unlist(z.bags[[i]][1]), unlist(z.bags[[i]][2]))
  mat <- rbind(mat, mat[1, ])
  lines(mat, col = bag.style$col[i], lty = bag.style$lty[i], lwd = bag.style$lwd[i])
  }
}

.ellipse.plot3d <- function(z.ellipse, ellipse.style) {
  for (i in 1:length(z.ellipse)) 
    rgl::plot3d(z.ellipse[[i]], col = ellipse.style$col[i], 
                alpha = ellipse.style$alpha.transparency[i], add = T)
}

