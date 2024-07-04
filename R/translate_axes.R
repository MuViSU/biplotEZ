#' Translate biplot axes
#' 
#' Automatically or manually translate the axes away from the center of the plot
#'
#' @param bp An object of class \code{biplot}
#' @param delta numeric value indicating distance between axes
#' @param swop logical. Change the direction in which axes are translated
#' @param distances numeric vector of distances. Used to manually parallel translate the axes.
#' 
#' @details
#' This function uses the same algorithm implemented in \code{\link[bipl5]{TDAbiplot}}
#' in the \code{bipl5} package. It translates the axes out of the center of the plot. 
#' Correlated axes generally gets translated in the same direction.
#' 
#' This function calculates the \code{orthogx} and \code{orthogy} paramaters in [axes()]
#' 
#' @return
#' 
#' An object of class \code{biplot} with the translated distances appended under bp$axes
#' @export
#' 
#'
#' @examples
#' 
#' #Translate the axes out of the plot center
#' 
#' bp <- biplot(state.x77,scaled = TRUE)|> 
#'       CVA(state.region) |> 
#'       translate_axes(swop=TRUE,delta =0.2)|>
#'       plot(exp.factor=3)
#' 
#' #adjust the distance of an axis
#' 
#' dist <- bp$axes$translate_distance
#' dist[7] <- 0.4
#' bp |> translate_axes(delta = 0.2, distances=dist) |> plot()
translate_axes<-function(bp, delta=0,swop=FALSE,distances=NULL){
  p<-bp$p
  m<-bp$ax.one.unit[,2]/bp$ax.one.unit[,1] #slopes of the axes
  thetas<-atan(m)                         #angles of the axes
  quadrant<-getquad(V=bp$ax.one.unit,m=m)
  RotMatrix<-RotationConstructor(thetas) #Rotation matrix
  
  elip_deets<-cluster::ellipsoidhull(bp$Z) #spanning ellipse
  elip<-cluster::predict.ellipsoid(elip_deets,n.out=101)
  
  RotatedElip<-elip%*%RotMatrix
  
  distShifted<-numeric(p)
  
  FinalPos2<-matrix(NA,nrow=1,ncol=2)

  #customise the ordering
  ordering<-order(m,decreasing = TRUE)
  
  for(i in ordering){
    ends<-matrix(c(min(RotatedElip[,(2*i-1)]),0,
                   max(RotatedElip[,(2*i-1)]),0),
                 ncol=2,byrow = TRUE) %*% RotationConstructor(-thetas[i])
    translate_deets<-translate(RotatedElip[,(2*i-1):(2*i)],quadrant[i],
                               FinalPos2,delta,ends,thetas[i],swop)
    FinalPos2<-rbind(FinalPos2,translate_deets$ends)
    distShifted[i]<-translate_deets$distance
  }
  
  if(!is.null(distances))
    distShifted<-distances
  
  orthog_x<-distShifted/sqrt(1+1/m^2)
  orthog_y<--1/m*orthog_x
  

  if(is.null(bp$axes))
    bp<- bp |> axes(orthogx = orthog_x, orthogy = orthog_y)
  else{
    bp$axes$orthogx<-orthog_x
    bp$axes$orthogy<-orthog_y
  }
  names(distShifted)<-bp$axes$names
  bp$axes$translate_distance<-distShifted
  
  return(bp)
}



#' Get quadrant of vector loading
#'
#' @param V Matrix of vector loadings from SVD
#' @param m Vector of slopes
#'
#' @return vector of quadrants
#' @noRd
getquad<-function(V,m){
  quads<-numeric(length(m))
  p<-length(m)
  for(i in 1:p){
    if(m[i]>0 && V[i,1]>0)
      quads[i]<-1
    if (m[i]>0 && V[i,1]<0)
      quads[i]<-3
    if(m[i]<0 && V[i,1]<0)
      quads[i]<-2
    if(m[i]<0 && V[i,1]>0)
      quads[i]<-4
  }
  return(quads)
}





#' Contruct Rotation Matrix
#'
#' This function appends multiple rotation matrices against one another
#'
#' @param angles Vector of angles to which plot must be rotated
#'
#' @return rotation matrix n x 2*p
#' @noRd
RotationConstructor<-function(angles){
  mat<-matrix(NA,nrow=2,ncol=2*length(angles))
  n<-length(angles)
  for(i in 1:n){
    mat[1,(2*i-1):(2*i)]<-c(cos(angles[i]),-sin(angles[i]))
    mat[2,(2*i-1):(2*i)]<-c(sin(angles[i]),cos(angles[i]))
  }
  mat
}




#' Translate lines up or down out of big ellipse
#'
#' Translate lines out of rotated ellipse up or down such that no intersections
#' occur
#'
#' @param elip Rotated ellipse
#' @param quadrant quadrant in which orginal vector loading fell
#' @param other details of other lines' coordinates. Matrix px2
#' @param d minimum distance to shift
#' @param endpoints endpoints of current rotated line
#' @param theta angle of axis
#' @param swop Swop the quadrants to which must rotated - up instead of down
#'
#' @return List containing distance shifted, as well as final endpoints
#' @noRd
translate<-function(elip,quadrant,other,d,endpoints,theta,swop=FALSE){
  
  other<-other%*%RotationConstructor(theta)
  q1<-c(2,3)
  q2<-c(1,4)
  if(swop){
    q2<-c(2,3)
    q1<-c(1,4)
  }
  
  if(quadrant%in%q1){
    #shift line downward and subtract d
    btm<-min(elip[,2])
    endpoints[,2]<-min(other[,2],btm,na.rm=TRUE)-d
  }
  if(quadrant%in%q2){
    #shift line upward and add d
    top<-max(elip[,2])
    endpoints[,2]<-max(top,other[,2],na.rm=TRUE)+d
  }
  shifted<-endpoints[1,2]
  true_endpoints<-endpoints%*%RotationConstructor(-theta)
  returnvalues<-list(distance=shifted,ends=true_endpoints)
  return(returnvalues)
}




