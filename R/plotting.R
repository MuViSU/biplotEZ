# ----------------------------------------------------------------------------------------------
#' Generic Plotting function of objects of class biplot
#'
#' @param x An object of class \code{biplot}.
#' @param exp.factor numeric value with default axes of the biplot. Larger values for zooming out and
#' smaller values for zooming in with respect to sample points
#' in the biplot display.
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
#' @examples
#' biplot (iris[,1:4]) |> PCA() |> plot()
plot.biplot <- function(x, exp.factor=1.2, axis.predictivity=NULL, sample.predictivity=NULL, ...)
{
  #----- See all the internal functions in utility_2D.R
  
  if (is.null(x$Z)) stop ("Add a biplot method before generating a plot")
  else Z <- x$Z

  if (is.null(x$samples)) x <- samples(x)
  if (!is.null(x$class.means)) if(x$class.means) if (is.null(x$means.aes)) x <- means(x)
  if (!is.null(x$Znew)) if (is.null(x$newsamples)) x <- newsamples(x)
  if (!is.null(x$predict$samples)) 
    predict.mat <- Z[x$predict$samples, , drop = F] 
  else predict.mat <- NULL
  if (!is.null(x$predict$means)) 
    predict.mat <- rbind(predict.mat, x$Zmeans[x$predict$means, , drop = F])
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


  if (x$dim.biplot == 3) plot3D(bp=x, exp.factor=exp.factor, ...)
  else
  {
    old.par <- graphics::par(pty = "s", ...)
    withr::defer(graphics::par(old.par))

    if(x$dim.biplot == 1) plot1D (bp=x, exp.factor=exp.factor)
    else
    {
      plot(Z[, 1] * exp.factor, Z[, 2] * exp.factor, xlim = range(Z[, 1] * exp.factor), ylim = range(Z[, 2] * exp.factor),
           xaxt = "n", yaxt = "n", xlab = "", ylab = "", type = "n", xaxs = "i", yaxs = "i", asp = 1)
      usr <- graphics::par("usr")
      
      # Classification Regions 
      if(!is.null(x$classify)) x <- classify(x)
      classify.aes <- x$classify$aes
      
      if(!is.null(x$classify$classify.regions)) {
        if(x$classify$classify.regions)
        {
          a <- predict.regions(x$Zmeans,usr)
          for(i in 1:length(a))
          {
            graphics::polygon(a[[i]],col=grDevices::adjustcolor(classify.aes$col[i],classify.aes$shade),
                              border = classify.aes$border)
          }
        }}
      
      # Density 
      if(!is.null(x$z.density)) .density.plot(x$z.density, x$density.style)
      
      # Axes 
      if(!inherits(x,"CA")) {  # If x does not inherit object of class "CA" then and axes() is not called, create default aesthetics for axes. 
        if (is.null(x$axes)) x <- axes(x)}
      
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
      
      if(!inherits(x,"CA")) {
        if(x$axes$calibrated.axes) { # If x does not inherit object of class "CA" then call to calibrate axes. 
          if (length(ax.aes$which) > 0)
          {
            if (!is.null(x$Lmat))
              if (nrow(x$Lmat) == ncol(x$Lmat)) 
                Xhat <- x$Z %*% solve(x$Lmat)[x$e.vects,]
              else Xhat <- x$X
            else
              Xhat <- x$X
            if (x$scaled) Xhat <- scale(Xhat, center=FALSE, scale=1/x$sd)
            if (x$center) Xhat <- scale(Xhat, center=-1*x$means, scale=FALSE)
            
            
            if(!is.null(x$PCOaxes)) { if(x$PCOaxes == "splines") # Only for PCO - if axes (type) is set to splines.  
            {
              z.axes <- lapply(1:length(ax.aes$which), biplot.spline.axis, Z, Xhat, means=x$means, sd=x$sd, n.int=ax.aes$ticks, 
                               spline.control=x$spline.control)
              .nonlin.axes.plot(z.axes,ax.aes,predict.mat,too.small, usr=usr,x=x)
              
            }} else { # Otherwise calibrate linear axes
              
              z.axes <- lapply(1:length(ax.aes$which), .calibrate.axis, Xhat, x$means, x$sd, x$ax.one.unit, ax.aes$which,
                               ax.aes$ticks, ax.aes$orthogx, ax.aes$orthogy)
              .lin.axes.plot(z.axes, ax.aes, predict.mat, too.small,usr=usr,x=x)
              
            }
            
            
            }
          
        } else  { # if calibrated axes = FALSE, then show vectors - only for PCA biplot. 
          if(inherits(x,"PCA")) .lin.axes.vector.plot(x$V.mat[,1:2],ax.aes)
          }
        
      }
      
      # Interpolate new axes 
      if(!is.null(x$newvariable)) { if(is.null(x$newaxes)) x <- newaxes(x) 
      
      new.ax.aes <- x$newaxes
      
      if (length(new.ax.aes$which) > 0)
      {
        
        z.axes.new <- lapply(1:length(new.ax.aes$which), .calibrate.axis, 
                             x$newvariable, x$new.means, x$new.sd, x$new.ax.one.unit, new.ax.aes$which,
                             new.ax.aes$ticks, new.ax.aes$orthogx, new.ax.aes$orthogy)
        .lin.axes.plot(z.axes.new, new.ax.aes, predict.mat, too.small, x=x, usr=usr)
      }
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
            
      if (!is.null(x$samples$which)) 
        .samples.plot(Z, x$group.aes, x$samples, 
                      x$n, x$g.names, ggrepel.samples,
                      too.small, cex.vec, usr=usr)
      
      if (!is.null(x$Znew)) .newsamples.plot (x$Znew, x$newsamples, ggrepel.new, usr=usr)
      
      if (!is.null(x$class.means)) if (x$class.means)
      {
        if (is.null(x$means.aes)) x <- means(x)
        .means.plot (x$Zmeans, x$means.aes, x$g.names, ggrepel.means,usr=usr)
      }
      
      # Alpha bags 
      if (!is.null(x$alpha.bags)) .bags.plot (x$alpha.bags, x$alpha.bag.aes)
      
      # Ellipse 
      if (!is.null(x$conc.ellipses)) .conc.ellipse.plot (x$conc.ellipses, x$conc.ellipse.aes)
      
      # Title   
      if (!is.null(x$Title)) graphics::title(main=x$Title)
      
      # Legends 
      if (!is.null(x$legend)) do.call(biplot.legend, list(bp=x, x$legend.arglist))
      
    }
        
  }

  invisible(x)
}

#' Generic Plotting function of objects of class biplot in three dimensions
#'
#' @param bp an object of class \code{biplot}
#' @param exp.factor factor to expand plotting area beyond samples.
#' @param ... more arguments
#'
#' @return an object of class \code{biplot}
#' 
#' @export
#'
#' @examples
#' biplot(data = iris) |> PCA(dim.biplot = 3) |> plot3D()

plot3D <- function(bp,
                    exp.factor = 1.2,...)
{
  
  if (is.null(bp$Z)) stop ("Add a biplot method before generating a plot")
  else Z <- bp$Z
  
  usr <- apply(Z, 2, range) * 1.2 * exp.factor
  usr <- c(min(usr[1, ]), max(usr[2, ]))
  rgl.scale <- (usr[2] - usr[1])/50
  rgl::open3d()
  rgl::aspect3d("iso")
  rgl::bg3d("#FFFFFF", fogtype = "lin")
  rgl::view3d(theta = 200, phi = 25, fov = 1)
  rgl::points3d(usr, usr, usr, alpha = 0)
  
  too.small <- NULL
  cex.vec <- rep(1, bp$n)
  
  # Plot samples 
  if(is.null(bp$samples)) bp <- samples(bp)
  if (!is.null(bp$Znew)) if (is.null(bp$newsamples)) bp <- newsamples(bp)
  if (!is.null(bp$samples$which)) 
    .samples.plot3d(Z, bp$group.aes, bp$samples, 
                  bp$n, bp$g.names, too.small, cex.vec, usr=usr)
 
  # Plot new samples
  if(!is.null(bp$Xnew)) .new.samples.plot3d(bp$Znew,bp$newsamples)
  
  # Means plot
  if (!is.null(bp$class.means)) if(bp$class.means) if (is.null(bp$means.aes)) bp <- means(bp)
  
  if (!is.null(bp$class.means)) if (bp$class.means)
  {
    if (is.null(bp$means.aes)) bp <- means(bp)
    .means.plot3d (bp$Zmeans, bp$means.aes,usr=usr)
  }
  
  # Prediction
  if (!is.null(bp$predict$samples)) 
    predict.mat <- Z[bp$predict$samples, , drop = F] 
  else predict.mat <- NULL
  if (!is.null(bp$predict$means)) 
    predict.mat <- rbind(predict.mat, bp$Zmeans[bp$predict$means, , drop = F])

  
  
  # Axes 
  if (is.null(bp$axes)) bp <- axes(bp)
  ax.aes <- bp$axes
  
  if (length(ax.aes$which) > 0)
  {
    if (!is.null(bp$Lmat))
      Xhat <- bp$Z %*% solve(bp$Lmat)[bp$e.vects,]
    else
      Xhat <- bp$X
    if (bp$scaled) Xhat <- scale(Xhat, center=FALSE, scale=1/bp$sd)
    if (bp$center) Xhat <- scale(Xhat, center=-1*bp$means, scale=FALSE)
    
    
    z.axes <- lapply(1:length(ax.aes$which), .calibrate.axis, Xhat, bp$means, bp$sd, bp$ax.one.unit, 
                     ax.aes$which, ax.aes$ticks, ax.aes$orthogx, ax.aes$orthogy)
    .lin.axes.plot3d(bp,z.axes, ax.aes, predict.mat,usr)
    
  }
  
  # New Axes 
  if(!is.null(bp$newvariable)) { if(is.null(bp$newaxes)) bp <- newaxes(bp) 
  new.ax.aes <- bp$newaxes
  
  if (length(new.ax.aes$which) > 0)
  {
    z.axes.new <- lapply(1:length(new.ax.aes$which), .calibrate.axis, 
                         bp$newvariable, bp$new.means, bp$new.sd, bp$new.ax.one.unit, 
                         new.ax.aes$which, new.ax.aes$ticks, new.ax.aes$orthogx, new.ax.aes$orthogy)
    .lin.axes.plot3d(bp,z.axes.new, new.ax.aes, predict.mat, usr=usr)
  }
  
  }
  

  # Bags 
  
  
  # Ellipses 
  if (!is.null(bp$conc.ellipses)) .ellipse.plot3d(bp$conc.ellipses, bp$conc.ellipse.aes)
  
}  


#' Plotting function for 1D biplots
#'
#' @param x object of class `biplot`
#' @param exp.factor expansion factor
#' @param ... more arguments
#'
#' @return an object of class `biplot`
#'
#' @noRd
#' 
plot1D <-  function(bp, exp.factor = 1.2,...) 
{
  .samples.plot <- function(bp) 
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
    
#----------
  .density.plot <- function(Z.density, density.style) 
  {
      for (i in 1:length(Z.density))
        graphics::lines(Z.density[[i]]$bp, Z.density[[i]]$y+0.5, col = density.style$col[i], lwd = density.style$lwd[i])#, lty = density.style$lty[i])
  }
    
  #----------
  .prediction.regions.plot <- function(borders, col,border,bounds=graphics::par('usr'))
  {
     graphics::rect(borders[1], 0.5, borders[2], bounds[4], col = col, border = border) 
  }
  #----------
  .means.plot <- function(Z, sample.aes, g.names, ggrepel.labs, usr)
  {
      x.vals <- Z[, 1, drop = FALSE]
      y.vals <- rep(0, length(Z))
      invals <- x.vals < usr[2] & x.vals > usr[1]
      Z <- Z[invals, , drop = FALSE]
      ZZ <- Z[sample.aes$which,]
      
      {
        Z.labels <- rownames(Z)[sample.aes$which]
        for (j in 1:length(sample.aes$label.side))
        {
          text.pos <-
            match(sample.aes$label.side[j],
                  c("bottom", "left", "top", "right"))
          if (sample.aes$label[j])
            graphics::text(
              ZZ[j, 1],
              0,
              labels = Z.labels[j],
              cex = sample.aes$label.cex[j],
              col = sample.aes$label.col[j],
              pos = text.pos,
              offset = sample.aes$label.offset[j]
            )
        }
      }
      for (j in 1:length(sample.aes$which))
        graphics::points(
          x = Z[sample.aes$which[j], 1],
          y = 0,
          pch = sample.aes$pch[j],
          col = sample.aes$col[j],
          cex = sample.aes$cex[j]
        )
  }

#----------
  .bags.plot <- function(z.bags, bag.style) 
  {
      for (i in 1:length(z.bags))
        graphics::rect(z.bags[[i]][1], -0.25, z.bags[[i]][2], 0.25,
                       col = grDevices::adjustcolor(bag.style$col[i],alpha.f = bag.style$opacity[i]),
                       border = bag.style$col[i],
                       cex = bag.style$lwd[i] * 1.2)
      
  }
    
#----------
  .conc.ellipse.plot <- function(z.interval, ellipse.style)
    {
      for (i in 1:length(z.interval))
        graphics::rect(z.interval[[i]][1], -0.25, z.interval[[i]][2], 0.25,
                       col = grDevices::adjustcolor(ellipse.style$col[i],alpha.f = ellipse.style$opacity[i]),
                       border = ellipse.style$col[i],
                       cex = ellipse.style$lwd[i] * 1.2)
    }
    
#----------
  .predict.func <- function(p.point, num.vars, col, lty, lwd) 
  {
    for (i in 1:nrow(p.point)) 
      graphics::lines(rep(p.point[i], 2), c(0, -1 * num.vars),
                     col = col, lwd = lwd, lty = lty)
      
  }
    
#----------
  .newsamples.plot <- function(Z, sample.aes)
  {
      graphics::points(x = Z[, 1], y = rep(0, length(Z)),
                       pch = sample.aes$pch, col = sample.aes$col, cex = sample.aes$cex)
  }
    
#----------
  .marker.label.cm <- function(x, y, grad, marker.val, expand = 1, col, label.on.off,
                               side, pos, offset, label.col, cex, usr)
  {
    uin <- graphics::par("pin") / c(usr[2] - usr[1], usr[4] - usr[3])
    mm <- 1 / (uin[1] * 25.4)
    d <- expand * mm
    if (grad == "v")
        {
          graphics::lines(rep(x, 2), c(y - d, y + d), col = col)
          if (label.on.off == 1)
            graphics::text(x, y - d, label = marker.val, pos = pos, offset = offset,
                           col = label.col, cex = cex)
        }
    if (grad == "h")
        {
          graphics::lines(c(x - d, x + d), rep(y, 2), col = col)
          if (label.on.off == 1)
            if (side == "right")
              graphics::text(x + d, y, label = marker.val, pos = pos, offset = offset,
                            col = label.col, cex = cex)
          else
            graphics::text(x - d, y, label = marker.val, pos = pos, offset = offset,
                           col = label.col, cex = cex)
        }
        if (is.numeric(grad))
        {
          b <- d * sqrt(1 / (1 + grad * grad))
          a <- b * grad
          graphics::lines(c(x - b, x + b), c(y - a, y + a), col = col)
          if (label.on.off == 1)
            if (side == "right")
              graphics::text(x + b, y + a, label = marker.val, pos = pos, offset = offset,
                             col = label.col, cex = cex)
          else
            graphics::text(x - b, y - a, label = marker.val, pos = pos, offset = offset,
                           col = label.col, cex = cex)
        }
    }
    
#----------
  .marker.func <- function(vec, coef, col, tick.size, side, pos, offset, label.col, cex, usr)
  {
        x <- vec[1]
        y <- vec[2]
        marker.val <- vec[3]
        label.on.off <- vec[4]
        .marker.label.cm(x, y, grad = "h", marker.val, expand = tick.size, col = col,
                           label.on.off = label.on.off, side = side, pos = pos,
                           offset = offset, label.col = label.col, cex = cex, usr = usr)
  }
    
#----------
  .lin.axes.plot <- function(z.axes, ax.aes, too.small, usr)
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
          if (is.null(this.axis$b))
            graphics::abline(v = this.axis$v, col = ax.aes$col[i], lwd = ax.aes$lwd[i], lty = ax.aes$lty[i])
          else
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
                    1, .marker.func, coef = lin.coef, col = ax.aes$tick.col[i],
                    tick.size = ax.aes$tick.size[i], side = ax.aes$tick.label.side[i],
                    pos = ax.aes$tick.label.pos[i], offset = ax.aes$tick.label.offset[i],
                    label.col = ax.aes$tick.label.col[i], cex = ax.aes$tick.label.cex[i], usr=usr
            )

          }
        }
        
  }
    
#----------
  .calibrate.axis <- function (j, Xhat, means, sd, axes.rows, ax.which, ax.tickvec,
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

  
    if (is.null(bp$Z)) stop ("Add a biplot method before generating a plot")
    else Z <- bp$Z    
    
    if (is.null(bp$samples)) bp <- samples(bp)
    if (!is.null(bp$class.means)) if(bp$class.means) if (is.null(bp$means.aes)) bp <- means(bp)
    if (is.null(bp$axes))  bp <- axes(bp)
    if (!is.null(bp$Znew)) if (is.null(bp$newsamples)) bp <- newsamples(bp)
    if (!is.null(bp$predict.samples)) predict.mat <- Z[bp$predict.samples, , drop = F] else predict.mat <- NULL
    if (!is.null(bp$predict.means)) predict.mat <- rbind(predict.mat, bp$Zmeans[bp$predict.means, , drop = F])
    
    if(is.null(bp$z.density)& is.null(bp$classify)) maxy <- 0.5 else maxy <- 3 
    total.num.vars <- ncol(bp$X)

    graphics::plot(range(Z * exp.factor), c(maxy, -1 * total.num.vars - 1),
                   xaxt = "n", yaxt = "n", xlab = "", ylab = "", type = "n")

    graphics::abline(h = 0)
    usr <- graphics::par('usr')
    too.small <- NULL
    ax.aes <- bp$axes
    
    # Classification Regions 
    if(!is.null(bp$classify)){ #bp <- classify(bp)
      classify.aes <- bp$classify$aes
      
      if(!is.null(bp$classify$classify.regions)) {
        if(bp$classify$classify.regions)
        {
          a <- predict.regions1D(bp,usr)
          for(i in 1:nrow(a))
          {
            .prediction.regions.plot(a[i,], col=grDevices::adjustcolor(classify.aes$col[i],alpha.f = classify.aes$shade),
                                     border = classify.aes$border, bounds = usr)
          }
        }}
    }
    

    if (length(ax.aes$which) > 0)
    {
      Xhat <- bp$Z %*% solve(bp$Lmat)[bp$e.vects, ]
      if (bp$scaled)
        Xhat <- scale(Xhat, center = FALSE, scale = 1 / bp$sd)
      if (bp$center)
        Xhat <- scale(Xhat, center = -1 * bp$means, scale = FALSE)
      z.axes <- lapply(1:length(ax.aes$which), .calibrate.axis, Xhat, bp$means, bp$sd,
                       bp$ax.one.unit, ax.aes$which, ax.aes$ticks, ax.aes$orthogx, ax.aes$orthogy)
      
      for (i in 1:length(z.axes)) {
        z.axes[[i]][[2]] <- -i
        z.axes[[i]][[3]] <- 0
        z.axes[[i]][[1]] <- cbind(z.axes[[i]][[1]][, 1], -i, z.axes[[i]][[1]][, 2])
      }
      
      .lin.axes.plot(z.axes, ax.aes, too.small, usr)
    }

    if (!is.null(bp$predict.samples) | !is.null(bp$predict.means)){
      .predict.func(p.point=predict.mat, total.num.vars, ax.aes$predict.col,
                    ax.aes$predict.lty, ax.aes$predict.lwd)
    }
    .samples.plot(bp)
    
    if(!is.null(bp$z.density)) {
      z.density <- bp$z.density
      density.style <- bp$density.style 
      .density.plot(z.density, density.style = density.style)
    }
    graphics::abline(h = 0.5)
    
    if (!is.null(bp$alpha.bags))
      .bags.plot (bp$alpha.bags, bp$alpha.bag.aes)
    
    if (!is.null(bp$conc.ellipses))
      .conc.ellipse.plot (bp$conc.ellipses, bp$conc.ellipse.aes)
    
    if (!is.null(bp$Znew))
      .newsamples.plot (bp$Znew, bp$newsamples)
    
    if (!is.null(bp$Title))
      graphics::title(main = bp$Title)
    
    if (!is.null(bp$legend))
      do.call(biplot.legend, list(bp = bp, bp$legend.arglist))
    
    if (!is.null(bp$class.means)) if (bp$class.means)
    {
      if (is.null(bp$means.aes)) bp <- means(bp)
      .means.plot (bp$Zmeans, bp$means.aes, bp$g.names, ggrepel.means=FALSE)
    }
    
    invisible(bp)
  }



