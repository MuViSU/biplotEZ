# ----------------------------------------------------------------------------------------------
#' Generic Plotting function of objects of class biplot
#'
#' @param x An object of class \code{biplot}.
#' @param exp.factor a numeric value with default axes of the biplot. Larger values are specified for zooming out with respect to sample points in the biplot display and smaller values are specified for zooming in with respect to sample points in the biplot display.
#' @param axis.predictivity either a logical or a numeric value between \code{0} and \code{1}. If it is a numeric value, this value is used as threshold so that only axes with axis predictivity larger than the threshold is displayed. If \code{axis.predictivity = TRUE}, the axis colour is 'diluted' in proportion with the axis predictivity.
#' @param sample.predictivity either a logical or a numeric value between 0 and 1. If it is a numeric value, this value is used as threshold so that only samples with sample predictivity larger than the threshold is displayed. If \code{sample.predictivity = TRUE}, the sample size is shrinked in proportion with the sample predictivity.
#' @param zoom a logical value allowing the user to select an area to zoom into.
#' @param xlim the horizontal limits of the plot.
#' @param ylim the vertical limits of the plot.
#' @param ... additional arguments.
#'
#' @return An object of class \code{biplot}.
#'
#' @export
#'
#' @examples
#' biplot (iris[,1:4]) |> PCA() |> plot()
plot.biplot <- function(x, exp.factor=1.2, axis.predictivity=NULL, sample.predictivity=NULL,
                        zoom=FALSE, xlim = NULL, ylim = NULL, ...)
{
  #----- See all the internal functions in utility_2D.R
  if (is.null(x$Z)) stop ("Add a biplot method before generating a plot")
    else Z <- x$Z

  #aesthetics for samples
  if (is.null(x$samples)) x <- samples(x) 
  
  if(zoom)
    grDevices::dev.new()

  # Predict samples 
  if (!is.null(x$predict$samples)) 
    predict.mat <- Z[x$predict$samples, , drop = F] 
  else predict.mat <- NULL
  
  # Predict means 
  if (!is.null(x$predict$means)) 
    predict.mat <- rbind(predict.mat, x$Zmeans[x$predict$means, , drop = F])
  
  ###  ggrepel for samples, new samples and class means 
  if (is.null(x$samples$which)) samples.ggrepel <- FALSE
    else samples.ggrepel <- any(stats::na.omit(x$samples$label=="ggrepel"))
  newsamples.ggrepel <- FALSE
  if (!is.null(x$Znew)) newsamples.ggrepel <- any(stats::na.omit(x$newsamples$label=="ggrepel"))
  means.ggrepel <- FALSE
  if (!is.null(x$class.means)) if (x$class.means) means.ggrepel <- any(stats::na.omit(x$means.aes$label=="ggrepel"))

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
    out <- R.devices::suppressGraphics(.get.ggrepel.coords(df))
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

  ### end of ggrepel 
  
  
  if (x$dim.biplot == 3) plot3D(bp=x, exp.factor=exp.factor, ...)
    else
    {
    old.par <- graphics::par(pty = "s", ...)
    withr::defer(graphics::par(old.par))

    if(x$dim.biplot == 1){ 
      if(inherits(x,"CA")){ # CA map
        plot_CA_1D(bp=x)
      } else {
        plot1D (bp=x, exp.factor=exp.factor)
      }
    }

    else # Plot 2D biplot 
    { 
      if(is.null(xlim) & is.null(ylim)){
        xlim <- range(Z[, 1] * exp.factor)
        ylim <- range(Z[, 2] * exp.factor)
      }
      
      # Start with empty plot
      plot(Z[, 1] * exp.factor, Z[, 2] * exp.factor, xlim = xlim, ylim = ylim,
           xaxt = "n", yaxt = "n", xlab = "", ylab = "", type = "n", xaxs = "i", yaxs = "i", asp = 1)

      usr <- graphics::par("usr")

      # Classification Regions - this should be plotted first. 
      # if(!is.null(x$classify)) x <- classify(x)
      classify.aes <- x$classify$aes
      
      if(!is.null(x$classify$classify.regions)) {
        if(x$classify$classify.regions)
        {
          a <- predict.regions(x$classify$region.midpoints,usr)
          for(i in 1:length(a))
          {
            graphics::polygon(a[[i]],col=classify.aes$col[i],
                              border = classify.aes$border)
          }
        }}
      
      # Density
      if(!is.null(x$z.density)) .density.plot(x$z.density, x$density.style)
      
      # Axes 
      # If x does not inherit object of class "CA" then and axes() is not called, create default aesthetics for axes. 
      if(!inherits(x,"CA")) {
        if (is.null(x$axes)) x <- axes(x)}
      ax.aes <- x$axes

      # Axis predictivity 
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

      if(inherits(x,"CA")){ # CA map
        if(x$dim.biplot == 2) .CA.plot(x$rowcoor, x$colcoor, x$group.aes, x$samples, x$r, x$c, x$g.names) else{
          if(x$dim.biplot == 3) .CA.plot3d(x$rowcoor, x$colcoor, x$group.aes, x$samples, x$r, x$c, x$g.names, ...)
        }
      } else
      {
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
            
            if(!is.null(x$PCOaxes)) 
              { if (x$PCOaxes == "splines") # Only for PCO - if axes (type) is set to splines.  
                  {
                    z.axes <- lapply(1:length(ax.aes$which), biplot.spline.axis, Z, x$raw.X, 
                               means=x$means, sd=x$sd, n.int=ax.aes$ticks, 
                               spline.control=x$spline.control)
                    .nonlin.axes.plot(z.axes,ax.aes,predict.mat,too.small, usr=usr,x=x)
              
                  } 
                else if(x$PCOaxes == "regression") # Only for PCO - if axes (type) is set to regression. 
                       {
                         z.axes <- lapply(1:length(ax.aes$which), .calibrate.axis, Xhat, x$means, x$sd, x$ax.one.unit, ax.aes$which,
                                        ax.aes$ticks, ax.aes$orthogx, ax.aes$orthogy)
                        .lin.axes.plot(z.axes, ax.aes, predict.mat, too.small,usr=usr,predict_which=x$predict$which)
                       }
              } 
            else 
              { # Otherwise calibrate linear axes
                z.axes <- lapply(1:length(ax.aes$which), .calibrate.axis, Xhat, x$means, x$sd, x$ax.one.unit, ax.aes$which,
                               ax.aes$ticks, ax.aes$orthogx, ax.aes$orthogy)
                .lin.axes.plot(z.axes, ax.aes, predict.mat, too.small,usr=usr,predict_which=x$predict$which)
              }
            }
            if (ax.aes$vectors) { # Draw vectors on the calibrated axes
              # this only draws vectors on top of the chosen calibrated axis 
              if(inherits(x,"PCA")) .lin.axes.vector.plot(x$Lmat[,1:2],ax.aes)
            }

      # Interpolate new axes 
      if(!is.null(x$newvariable)) { if(is.null(x$newaxes)) x <- newaxes(x) 
      
      new.ax.aes <- x$newaxes
      
      if (length(new.ax.aes$which) > 0)
      {
        
        z.axes.new <- lapply(1:length(new.ax.aes$which), .calibrate.axis, 
                             x$newvariable, x$new.means, x$new.sd, x$new.ax.one.unit, new.ax.aes$which,
                             new.ax.aes$ticks, new.ax.aes$orthogx, new.ax.aes$orthogy)
        .lin.axes.plot(z.axes.new, new.ax.aes, predict.mat, too.small, usr=usr, predict_which=x$predict$which)
      }
      }
      
      
      # Fit measures 
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
         
      # Samples 
      if (is.null(x$samples)) x <- samples(x)
      if  (!is.null(x$samples$which) & !inherits(x, "CA"))
        {
        .samples.plot(Z, x$group.aes, x$samples, 
                      x$n, x$g.names, ggrepel.samples,
                      too.small, cex.vec, usr=usr,x$alpha.bag.outside,
                      x$alpha.bag.aes)}

      # New samples 
      if (!is.null(x$Znew)) if (is.null(x$newsamples)) x <- newsamples(x)
      if (!is.null(x$Znew)) .newsamples.plot (x$Znew, x$newsamples, ggrepel.new, usr=usr)
      
      # Means
      if (!is.null(x$class.means)) if (x$class.means)
      {
        if (is.null(x$means.aes)) x <- means(x)
        .means.plot (x$Zmeans, x$means.aes, x$g.names, ggrepel.means,usr=usr)
      }

      # CLPs 
      if (!is.null(x$CLP.coords)) 
        {
        if (is.null(x$CLP.aes)) x <- CLPs(x)
        if  (!is.null(x$CLP.aes$which) & !inherits(x, "CA"))
        {
          for (i in 1:length(x$CLP.aes$which))
            .CLPs.plot(x$CLP.coords[[x$CLP.aes$which[i]]], 
                       x$CLP.aes$col[[i]], x$CLP.aes$cex[[i]])
        }
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
      
}

  if(zoom){
    cat("Choose upper left hand corner:\n")
    a <- graphics::locator(1)
    cat("Choose lower right hand corner:\n")
    b <- graphics::locator(1)
    arguments <- as.list(match.call())
    arguments[[1]] <- NULL
    arguments$x <- x
    arguments$zoom <- FALSE
    arguments$xlim <- c(a$x,b$x)[order(c(a$x,b$x))]
    arguments$ylim <- c(a$y,b$y)[order(c(a$y,b$y))]
    grDevices::dev.off()
    do.call(plot.biplot,arguments)
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
#' @noRd
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
if(!inherits(bp,"CA")){
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
  
} else {
  .CA.plot3d(bp$rowcoor, bp$colcoor, bp$group.aes, bp$samples, bp$r, bp$c, bp$g.names)
}
  
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

  
    if (is.null(bp$Z)) stop ("Add a biplot method before generating a plot")
    else Z <- bp$Z    
    
    # Ensure all necessary aesthetics is attached to bp. If aesthetics is missing, 
    #     aesthetics functions are run with defaults values.
    if (is.null(bp$samples)) bp <- samples(bp)
    if (!is.null(bp$class.means)) if(bp$class.means) if (is.null(bp$means.aes)) bp <- means(bp)
    if (is.null(bp$axes))  bp <- axes(bp)
    if (!is.null(bp$Znew)) if (is.null(bp$newsamples)) bp <- newsamples(bp)
    if (!is.null(bp$predict$samples)) predict.mat <- Z[bp$predict$samples, , drop = F] else predict.mat <- NULL
    if (!is.null(bp$predict$means)) predict.mat <- rbind(predict.mat, bp$Zmeans[bp$predict$means, , drop = F])
    if(!is.null(bp$newvariable)) { if(is.null(bp$newaxes)){ bp <- newaxes(bp)};total.num.vars <- ncol(bp$X) + length(bp$newaxes$which)} else {total.num.vars <- ncol(bp$X)}
    
    # Create white space at top of plot for classification regions and/or densities if needed
    if(is.null(bp$z.density)& is.null(bp$classify)) maxy <- 0.5 else maxy <- 3 

    # Draw empty plot
    graphics::plot(range(Z * exp.factor), c(maxy, -1 * total.num.vars - 1),
                   xaxt = "n", yaxt = "n", xlab = "", ylab = "", type = "n")
    # Draw line on which scatter points are plotted.
    graphics::abline(h = 0)
    # If classification regions and/or densities must be drawn separating plotting line.
    if(!(is.null(bp$z.density)& is.null(bp$classify))){ graphics::abline(h = 0.5) }
    usr <- graphics::par('usr')
    too.small <- NULL
    ax.aes <- bp$axes
    
    # Classification Regions 
    if(!is.null(bp$classify)){ #bp <- classify(bp)
      classify.aes <- bp$classify$aes
      
      if(!is.null(bp$classify$classify.regions)) {
        if(bp$classify$classify.regions)
        {
          a <- predict_regions1D(bp,usr)
          for(i in 1:nrow(a))
          {
            .prediction.regions.plot1D(a[i,], col=classify.aes$col[i],
                                     border = classify.aes$border, bounds = usr)
          }
        }}
    }
    

    # This section calculates calibrated axes, replaces the y-coordinates with -i and draws axes. 
    if (length(ax.aes$which) > 0)
    {
      Xhat <- bp$Z %*% solve(bp$Lmat)[bp$e.vects, ]
      if (bp$scaled)
        Xhat <- scale(Xhat, center = FALSE, scale = 1 / bp$sd)
      if (bp$center)
        Xhat <- scale(Xhat, center = -1 * bp$means, scale = FALSE)
      z.axes <- lapply(1:length(ax.aes$which), .calibrate.axis1D, Xhat, bp$means, bp$sd,
                       bp$ax.one.unit, ax.aes$which, ax.aes$ticks, ax.aes$orthogx, ax.aes$orthogy)
      
      for (i in 1:length(z.axes)) {
        z.axes[[i]][[2]] <- -i
        z.axes[[i]][[3]] <- 0
        z.axes[[i]][[1]] <- cbind(z.axes[[i]][[1]][, 1], -i, z.axes[[i]][[1]][, 2])
      }
      
      .lin.axes.plot1D(z.axes, ax.aes, too.small, usr)
    }
    
    # Interpolate new axes. Drawn below axes used in biplot.
    if(!is.null(bp$newvariable)) { if(is.null(bp$newaxes)) bp <- newaxes(bp)

    new.ax.aes <- bp$newaxes

    if (length(new.ax.aes$which) > 0)
    {

      z.axes.new <- lapply(1:length(new.ax.aes$which), .calibrate.axis,
                           bp$newvariable, bp$new.means, bp$new.sd, bp$new.ax.one.unit, new.ax.aes$which,
                           new.ax.aes$ticks, new.ax.aes$orthogx, new.ax.aes$orthogy)
      for (i in 1:length(z.axes.new)) { 
        z.axes.new[[i]][[2]] <- -i - ncol(bp$X)
        z.axes.new[[i]][[3]] <- 0
        z.axes.new[[i]][[1]] <- cbind(z.axes.new[[i]][[1]][, 1], -i -ncol(bp$X), z.axes.new[[i]][[1]][, 2])
      }
      .lin.axes.plot1D(z.axes.new, new.ax.aes, too.small, usr=usr)
    }
    }

    # Predictions of points with lines down to the axes.
    if (!is.null(predict.mat)){

      .predict1D(p.point=predict.mat, total.num.vars, ax.aes$predict.col,
                    ax.aes$predict.lty, ax.aes$predict.lwd)
    }
    
    # Plot samples
    #.samples.plot1D(bp)
    .samples.plot1D(bp$Z,samples.aes = bp$samples,group.aes = bp$group.aes,g.names = bp$g.names,usr = graphics::par('usr'))
    
    # Densities
    if(!is.null(bp$z.density)) {
      z.density <- bp$z.density
      density.style <- bp$density.style 
      .density.plot1D(z.density, density.style = density.style)
    }
    
    # Alpha bags
    if (!is.null(bp$alpha.bags))
      .bags.plot1D (bp$alpha.bags, bp$alpha.bag.aes)
    
    # Concentration Ellipses
    if (!is.null(bp$conc.ellipses))
      .conc.ellipse.plot1D (bp$conc.ellipses, bp$conc.ellipse.aes)
    
    # New samples interpolated onto plot
    if (!is.null(bp$Znew))
      .newsamples.plot1D (bp$Znew, bp$newsamples)
    
    # Main title
    if (!is.null(bp$Title))
      graphics::title(main = bp$Title)
    
    # Plot Means
    if (!is.null(bp$class.means)) if (bp$class.means)
    {
      if (is.null(bp$means.aes)) bp <- means(bp)
      .means.plot1D (bp$Zmeans, bp$means.aes, bp$g.names, usr=usr)#, ggrepel.means=FALSE)
    }
    
    # Legend
    if (!is.null(bp$legend))
      do.call(biplot.legend, list(bp = bp, bp$legend.arglist))
    
    invisible(bp)
}



