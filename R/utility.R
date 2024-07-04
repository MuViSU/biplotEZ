# ----------------------------------------------------------------------------------------------
#' indmat
#'
#' @param groep.vec vector of grouping labels
#'
#' @return an indicator matrix.
#'
#' @noRd
indmat  <- function (groep.vec)
{
  elements <- levels(factor(groep.vec))
  Y <- matrix(0, nrow = length(groep.vec), ncol = length(elements))
  dimnames(Y) <- list(NULL, paste(elements))
  for (i in 1:length(elements)) Y[groep.vec == elements[i], i] <- 1
  return(Y)
}

# ----------------------------------------------------------------------------------------------

#' Get points of intersection between decision boundaries with plot frame
#'
#' @param x Matrix of points from which boundaries are to be drawn
#' @param bounds Bounds of plotting frame
#'
#' @return Matrix containing x-coo,y-coo, line a intersect with line b
#' @noRd
getintersects<-function(x,bounds){
  eq<-matrix(c(0,bounds[3],NA,bounds[2],0,bounds[4],NA,bounds[1]),ncol=2,byrow=TRUE)
  intersects<-matrix(ncol=2)
  whatlines<-matrix(ncol=2)
  n<-5
  for(i in 1:(nrow(x)-1)){
    for(j in (i+1):nrow(x)){
      eq<-rbind(eq,equation(x[i,],x[j,]))
      snypunte<-sol(eq[n,],eq[-n,])
      countermat<-cbind(rep(n-4,length(nrow(snypunte))),snypunte[,3])
      whatlines<-rbind(whatlines,countermat)
      intersects<-rbind(intersects,snypunte[,-3])
      n<-n+1
    }
  }
  inside_x<-intersects[-1,1]>=(bounds[1]-0.0000001)&intersects[-1,1]<=(bounds[2]+0.0000001)
  inside_y<-intersects[-1,2]>=(bounds[3]-0.0000001)&intersects[-1,2]<=(bounds[4]+0.0000001)
  inside<-intersects[-1,]
  inside<-inside[inside_x&inside_y,]
  whatlines<-whatlines[-1,]
  return(cbind(inside,whatlines[inside_x&inside_y,]))
}

#' Get equation of line with equal distance between two points
#'
#' @param p1 Point 1 vector
#' @param p2 Point 2 vector
#'
#' @return vector containing slope and intercept
#' @noRd
equation<-function(p1,p2){
  
  #generalise to p-dimensional space. See the following 
  # link https://math.stackexchange.com/questions/987488/equation-for-a-plane-perpendicular-to-a-line-through-two-given-points#:~:text=If%20the%20plane%20is%20perpendicular,%E2%88%92z0)%3D0.
  if(length(p1)>2){
    
    #note corner cases not covered for as of yet
    point<-(p1+p2)/2
    n<-p1-p2 #normal vector
    rhs<-sum(n*point) #n*p
    a<-rhs/n[2]
    b<--n[1]/n[2]
    return(c(b,a))
  }
  
  
  if(p1[2]==p2[2]){
    return(c(NA,(p1[1]+p2[1])/2))
  }
  point<-(p1+p2)/2
  b<--1*(p1[1]-p2[1])/(p1[2]-p2[2])
  #c=y-mx
  a<-point[2]-b*point[1]
  return(c(b,a))
}

#' Solve for point where two lines intersect
#'
#' @param v1 Vector containing slope and intercept of line
#' @param v2 Matrix of other lines' slopes and intercepts
#'
#' @return Matrix containing intersections with indicator third column (x,y,ind)
#' @noRd
sol<-function(v1,v2){
  #v1 is vector containing slope and intercept >>>c(slope,intercept) of new line
  #V2 is matrix containing slopes and intercepts of previous lines Col1=slope, col2=intercept
  #This function solves for the point where two lines intersect
  x<-numeric()
  y<-numeric()
  n<-numeric()
  for(i in 1:nrow(v2)){
    
    #make provision for lines that are both infinite slope
    if(is.na(v2[i,1]) & is.na(v1[1])) next
    if(is.na(v1[1])){
      x<-append(x,v1[2])
      y<-append(y,v2[i,2]+v2[i,1]*v1[2])
      n<-append(n,i)
      next
    }
    
    if(is.na(v2[i,1])){
      x<-append(x,v2[i,2])
      y<-append(y,v1[2]+v1[1]*v2[i,2])
      n<-append(n,i)
      next
    }
    if(v1[1]==v2[i,1]) next
    x_coo<-(v2[i,2]-v1[2])/(v1[1]-v2[i,1])
    x<-append(x,x_coo)
    y<-append(y,v1[1]*x_coo+v1[2])
    n<-append(n,i)
  }
  matrix(c(x,y,n),ncol=3)
}


#' Make a matrix containing line segments on the boundary
#' 
#' Coordinates are repeated to make up little line segments
#'
#' @param x entire decision boundary with all intersections
#'
#' @return matrix containing segments on the boundary
#' @noRd
makemat<-function(x){
  volgorde<-order(x[,1])
  if(length(unique(x[,1]))==1){ #that is, line perfectly vertical, then sort by y
    x<-x[!duplicated(round(x[,2],15)),] #round to 15 decimal places to ensure no repeat coordinates. Tuning paramater
    volgorde<-order(x[,2])
  }
  else{ #sort by ascending x
    x<-x[!duplicated(round(x[,1],15)),]
    volgorde<-order(x[,1])
  }
  x<-x[volgorde,]
  if(nrow(x)==2) return(x) #there is no intersections with any line other than edge
  temp1<-matrix(x[1,],ncol=2,byrow=TRUE)
  for(i in 2:(nrow(x)-1)){
    temp1<-rbind(temp1,x[i,],x[i,])
  }
  temp1<-rbind(temp1, x[nrow(x),])
  return(temp1)
}

#' Compute euclidean distance between two points
#'
#' @param x1 Vector (x,y)
#' @param x2 Vector (x,y). Can also generalise to p-dimensions. Function makes provision
#'
#' @return Euclidean distance - scalar
#' @noRd
euc.dist<- function(x1, x2) {
  if(length(x2)>2)
    x1<-c(x1,rep(0,length(x2-2)))
  
  sqrt(sum((x1 - x2) ^ 2))
}

#' Obtain minimum two Euclidean distances
#'
#' Distance between a points and a matrix of points
#'
#' @param p1 Point 1 - vector (x,y)
#' @param p2 Matrix of points (x,y). Can generalise to have p columns as well. euc.dist sorts this bad boy out!
#'
#' @return indicator vector indicating which points in p2 are closest and second
#' closest to p1
#' @noRd
minEucDist <- function(p1,p2) {
  distances<-apply(p2,MARGIN=1,euc.dist,x1=p1)
  closest1<-which(distances==min(distances))
  distances[closest1]<-NA
  closest2<-which(distances==min(distances,na.rm = TRUE))
  return(c(closest1,closest2))
}


#' Determine which parts of line segment constitutes a true boundary
#' 
#' Have a line segment of different parts. This function tests each part if 
#' it is in fact a separating line between the two classes contained in 
#' points parameter
#'
#' @param seg Line segment - matrix [x,y]
#' @param original coordinates of class means - matrix [x,y]. Has been generalised to accept p columns as well.
#' @param points vector showing which two points the proposed line segment
#'               is separating - ex (1,2)
#'
#' @return matrix containing coordinates of true boundaries on the line
#' @noRd
TrueBoundary<-function(seg,original,points){
  n<-nrow(seg)
  print("Awe")
  print(n)
  midpoints<-matrix(nrow=(nrow(seg)/2),ncol=2)
  for(i in 1:(nrow(seg)/2)){
    lil<-seg[(2*i-1):(2*i),]
    midpoints[i,]<-c(mean(lil[,1]),mean(lil[,2]))
  }
  #okay so now need to see if it is true boundary or not
  discard<-numeric()
  for(i in 1:nrow(midpoints)){
    close_point<-minEucDist(midpoints[i,],original)
    if(!(close_point[1]%in% points & close_point[2]%in% points)){
      discard<-append(discard,c(2*i-1,2*i))
    }
  }
  if(length(discard)==0) return(seg)
  
  remaining<-seg[-discard,]
  if(nrow(remaining)==0) return(remaining)
  
  #remove all the interior points on the straight line
  if(length(unique(remaining[,1]))==1){ #that is, line perfectly vertical,
    idx_1<-which(remaining[,2]==max(remaining[,2]))
    idx_2<-which(remaining[,2]==min(remaining[,2]))
    return(remaining[c(idx_1,idx_2),])
  }
  
  idx_1<-which(remaining[,1]==max(remaining[,1]))
  idx_2<-which(remaining[,1]==min(remaining[,1]))
  return(remaining[c(idx_1,idx_2),])
  
}

#' Make list containing matrix of segments on each line
#'
#' @param x Output obtained  from getintersects function
#'
#' @return List containing matrices of segments
#' @noRd
makelistmat<-function(x){
  x[,4]<-x[,4]-4
  segments<-list()
  for(i in unique(x[,3])){
    segments[[i]]<-makemat(x[x[,3]==i | x[,4]==i,1:2])
  }
  return(segments)
}


#' Order the coordinates of a polygon such that it is circular
#'
#' @param x Polygons in a list
#' @param points Coordinates of class means
#'
#' @return List with polygons, coordinates ordered
#' @noRd
makeCircular<-function(x,points){
  for(i in 1:length(x)){
    #first center the points
    cent_x<-x[[i]][,1]-points[i,1]
    cent_y<-x[[i]][,2]-points[i,2]
    thetas<-atan(cent_y/cent_x)
    for(j in 1:length(thetas)){
      if(cent_x[j]<0 & cent_y[j]>0) 
        thetas[j]<-pi+thetas[j]
      if(cent_x[j]<0 & cent_y[j]<0)
        thetas[j]<-pi+thetas[j]
      if(cent_x[j]>0 & cent_y[j]<0)
        thetas[j]<-2*pi+thetas[j]
    }
    x[[i]]<-x[[i]][order(thetas),]
  }
  return(x)
}


#' Main entry function to get the prediction regions.
#'
#' @param x Matrix of coordinates - class centers. [x,y,z,p,...]
#' @param bounds Bounds of the plotting frame
#'
#' @return Polygon regions 
#' 
#' @noRd
#'
predict.regions <- function (x, bounds=graphics::par("usr"))
{ 
  Deets <- getintersects(x=x,bounds=bounds)
  Get_segments <- makelistmat(Deets)
  print(Get_segments)
  TruePoints <- matrix(ncol=2)
  
  for(i in 1:(nrow(x)-1)){
    for(j in (i+1):nrow(x)){
      TruePoints<-rbind(TruePoints,c(i,j))
    }
  }
  
  bounds_coors<-matrix(c(bounds[1],bounds[3],
                         bounds[2],bounds[3],
                         bounds[2],bounds[4],
                         bounds[1],bounds[4]),ncol=2,byrow = T)
  #need to break early if only have two class means
  if(nrow(TruePoints)==2){
    PolygonRegion<-list(Get_segments[[1]],Get_segments[[1]])
    for(i in 1:4){
      idx<-minEucDist(bounds_coors[i,],x)[1]
      PolygonRegion[[idx]]<-rbind(PolygonRegion[[idx]],bounds_coors[i,])
    }
    # order the coordinates such that it is circular
    PolygonRegion<-makeCircular(PolygonRegion,x)
    return(PolygonRegion) 
  } 
  TruePoints<-TruePoints[-1,]
  finalsegs<-list()
  for(i in 1:nrow(TruePoints)){
    finalsegs[[i]]<-TrueBoundary(seg=Get_segments[[i]],original=x,points=TruePoints[i,])
  }
  PolygonRegion<-list()
  #now only need to group together the points to each polygon
  for(i in 1:nrow(x)){
    index<-c(which(TruePoints[,1]==i),which(TruePoints[,2]==i))
    tempmat<-finalsegs[[index[1]]]
    if(length(index)==1){
      PolygonRegion[[i]]<-tempmat
      next
    }
    for(j in 2:length(index)){
      tempmat<-rbind(tempmat,finalsegs[[index[j]]])
    }
    PolygonRegion[[i]]<-tempmat
  }
  
  #next enter corners into relevant matrices
  
  bounds_coors<-matrix(c(bounds[1],bounds[3],
                         bounds[2],bounds[3],
                         bounds[2],bounds[4],
                         bounds[1],bounds[4]),ncol=2,byrow = T)
  for(i in 1:4){
    idx<-minEucDist(bounds_coors[i,],x)[1]
    PolygonRegion[[idx]]<-rbind(PolygonRegion[[idx]],bounds_coors[i,])
  }
  
  # order the coordinates such that it is circular
  PolygonRegion<-makeCircular(PolygonRegion,x)
  return(PolygonRegion)
}



#' Title
#'
#' @param x Vector of coordinates - class centers.
#' @param bounds Bounds of the plotting frame
#'
#' @return Bounds for classification intervals
#' @export
#'
#' @noRd
predict.regions1D <- function(x, bounds = graphics::par("usr")) {
  tmp.Zmeans <- x$Zmeans
  tmp.borders <-
    (sapply(2:length(tmp.Zmeans), function(x)
      (tmp.Zmeans[order(tmp.Zmeans)][x] + tmp.Zmeans[order(tmp.Zmeans)][x - 1])/2))
  tmp.vals <- c(min = bounds[1], tmp.borders, max = bounds[2])
  tmp.vals <- tmp.vals[order(tmp.vals)]
  borders.coords <-
    cbind(tmp.vals[1:(length(tmp.vals) - 1), drop = FALSE] , tmp.vals[2:length(tmp.vals), drop =
                                                                        FALSE])
  borders.which <-
    sapply(tmp.Zmeans, function(x)
      which(x > borders.coords[, 1] & x <= borders.coords[, 2]))
  borders.coords <- borders.coords[borders.which, ]
  # Returns the classification region borders for each of the classes in the order that the classes are in the bp function
  return(borders.coords)
}

# ----------------------------------------------------------------------------------------------
#' Computes the square root of the Manhattan distance
#' An example of a Euclidean embeddable distance metric
#'
#' @param X matrix of samples x variables for computation of samples x samples distance matrix
#'
#' @return a dist object
#' @export
#'
#' @examples
#' sqrtManhattan(iris[,1:4])
sqrtManhattan <- function (X)
{
  sqrt(stats::dist(X, method="manhattan"))
}

#' Extended matching coefficient
#'
#' @param X a data frame containing the categorical variables used for computing the EMC distance
#'
#' @return a dist object
#' @export
#'
#' @examples
#' mtdf <- as.data.frame(mtcars)
#' mtdf$cyl <- factor(mtdf$cyl)
#' mtdf$vs <- factor(mtdf$vs)
#' mtdf$am <- factor(mtdf$am)
#' mtdf$gear <- factor(mtdf$gear)
#' mtdf$carb <- factor(mtdf$carb)
#' extended.matching.coefficient(mtdf[,8:11])
#' 
extended.matching.coefficient <- function (X)
{
  n <- nrow(X)
  Dmat <- matrix (0, nrow=n, ncol=n)
  for (k in 1:ncol(X))
  {
    Gk <- indmat(X[,k])
    Dmat <- Dmat + matrix (1, nrow=n, ncol=n) - Gk %*% t(Gk)
  }
  stats::as.dist(sqrt(Dmat))
}