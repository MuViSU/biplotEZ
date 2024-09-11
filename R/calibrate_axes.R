#' Calibrate spline axes for the biplot display
#' 
#' Convenience function to obtain the coordinates of the calibrated ticks marks on the biplot
#' 
#' @param x an object of class `biplot'
#'
#' @return A ordered list containing the coordinates of tick marks to plotted on the biplot
#' @export
#'
#' @examples
#' x<-biplot(iris) |> PCA()
#' coordinates<-axes_coordinates(x)
axes_coordinates<-function(x){
  Z <- x$Z
  if(!inherits(x,"CA")) {
    if (is.null(x$axes)) x <- axes(x)}
  ax.aes <- x$axes
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

      
    } 
      else if(x$PCOaxes == "regression") # Only for PCO - if axes (type) is set to regression. 
      {
        z.axes <- lapply(1:length(ax.aes$which), .calibrate.axis, Xhat, x$means, x$sd, x$ax.one.unit, ax.aes$which,
                         ax.aes$ticks, ax.aes$orthogx, ax.aes$orthogy)
        
        for(i in 1:length(z.axes)){
          z.axes[[i]]<-z.axes[[i]][[1]]
        }
      }
    } 
    else 
    { # Otherwise calibrate linear axes
      z.axes <- lapply(1:length(ax.aes$which), .calibrate.axis, Xhat, x$means, x$sd, x$ax.one.unit, ax.aes$which,
                       ax.aes$ticks, ax.aes$orthogx, ax.aes$orthogy)
      for(i in 1:length(z.axes)){
        z.axes[[i]]<-z.axes[[i]][[1]]
      }
    }
  }
  
  return(z.axes)
}