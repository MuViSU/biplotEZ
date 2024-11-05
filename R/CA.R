# ----------------------------------------------------------------------------------------------
#' Correspondence Analysis (CA) method
#'
#' @description
#' This function produces a list of elements to be used for CA biplot construction by approximation of the Pearson residuals.
#'
#' @param bp object of class \code{biplot} obtained from preceding function \code{biplot(center = FALSE)}. In order to maintain the frequency table, the input should not be centered or scaled. For \code{CA}, \code{bp} should be a contingency table.
#' @param dim.biplot dimension of the biplot. Only values 1, 2 and 3 are accepted, with default \code{2}.
#' @param e.vects which eigenvectors (canonical variates) to extract, with default \code{1:dim.biplot}.
#' @param variant which correspondence analysis variant, with default "Princ", presents a biplot with rows in principal coordinates
#'  and columns in standard coordinates. \code{variant = "Stand"}, presents a biplot with rows in standard coordinates and columns in 
#'  principal coordinates. \code{variant = "symmetric"}, presents a symmetric biplot with row and column standard coordinates scaled 
#'  equally by the singular values. 
#' @param lambda.scal logical value to request lambda-scaling, default is \code{FALSE}. Controls stretching or shrinking of
#'  column and row distances.
#'
#' @return A list with the following components is available:
#' \item{Z}{Combined data frame of the row and column coordinates.}
#' \item{r}{Numer of levels in the row factor.}
#' \item{c}{Numer of levels in the column factor.}
#' \item{rowcoor}{Row coordinates based on the selected \code{variant}.}
#' \item{colcoor}{Column coordinates based on the selected \code{variant}.}
#' \item{P}{Correspondence Matrix}
#' \item{Smat}{Standardised Pearson residuals.}
#' \item{SVD}{Singular value decomposition solution: \code{d, u, v}.}
#' \item{qual}{Quality of the approximation.}
#' \item{lambda.val}{The computed lambda value if lambda-scaling is requested.}
#'
#' @seealso [biplot()]
#'
#' @usage CA(bp, dim.biplot = c(2,1,3), e.vects = 1:ncol(bp$X), variant = "Princ", 
#' lambda.scal = FALSE)
#' @aliases CA
#'
#' @export
#'
#' @examples
#' # Creating a CA biplot with rows in principal coordinates:
#' biplot(HairEyeColor[,,2], center = FALSE) |> CA() |> plot()
#' # Creating a CA biplot with rows in standard coordinates:
#' biplot(HairEyeColor[,,2], center = FALSE) |> CA(variant = "Stand") |> 
#' samples(col=c("magenta","purple"), pch = c(15,17), label.col = "black") |> plot()
#' # Creating a CA biplot with rows and columns scaled equally:
#' biplot(HairEyeColor[,,2], center = FALSE) |> CA(variant = "Symmetric") |> 
#' samples(col = c("magenta","purple"), pch = c(15,17), label.col = "black") |> plot()

CA <- function(bp, dim.biplot = c(2,1,3), e.vects = 1:ncol(bp$X), variant = "Princ", lambda.scal = FALSE)
{
  UseMethod("CA")
}

#' CA biplot
#'
#' @description Performs calculations for a CA biplot.
#'
#' @inheritParams CA
#'
#' @return an object of class CA, inherits from class biplot.
#' @export
#'
#' @examples
#' biplot(HairEyeColor[,,2], center = FALSE) |> CA() |> plot()
#'
CA.biplot <- function(bp, dim.biplot = c(2,1,3), e.vects = 1:ncol(bp$X), variant = "Princ", lambda.scal = FALSE)
{
  if (bp$center == TRUE) 
  {  warning (paste("Centering was not performed. Set biplot(center = FALSE) when performing CA()."))}
  if (bp$scale == TRUE) 
  {  warning (paste("Scaling was not performed. Set biplot(scale = FALSE) when performing CA()."))}
  
  dim.biplot <- dim.biplot[1]
  if (dim.biplot != 1 & dim.biplot != 2 & dim.biplot != 3) stop("Only 1D, 2D and 3D biplots")
  e.vects <- e.vects[1:dim.biplot]
  
  if (is.na(match(variant, c("Stand", "Princ", "Symmetric")))) 
    stop("variant must be one of: Stand, Princ or Symmetric \n")
  
  #standard CA method
  X <- bp$raw.X     #to ensure that unscaled and uncentered frequency table is used
  r <- nrow(X)      #number of rows / levels of row factor
  c <- ncol(X)      #number of columns / levels of column factor
  N <- sum(X)       #grand total
  P <- X/N          #using the correspondence matrix
  
  rMass <- rowSums(P)									
  cMass <- colSums(P)									
  Dr <- diag(apply(P, 1, sum))				    #diagonal matrix of column profiles
  Dc <- diag(apply(P, 2, sum))				    #diagonal matrix of row profiles
  Drh <- sqrt(solve(Dr))	    						#weighted column profiles
  Dch <- sqrt(solve(Dc))			    				#weighted row profiles
  Emat <- (Dr %*%matrix(1, nrow = r, ncol = c) %*%  Dc)
  Smat <- Drh%*%(P-Emat)%*%Dch				    #standardised Pearson residuals
  
  svd.out <- svd(Smat)
  
  if(variant == "Stand") gamma <- 0
  if(variant == "Princ") gamma <- 1
  if(variant == "Symmetric") gamma <- 0.5
  
  rowcoor <- svd.out[[2]]%*%(diag(svd.out[[1]])^gamma)
  colcoor <- svd.out[[3]]%*%(diag(svd.out[[1]])^(1-gamma))
  
  rownames(rowcoor) <- rownames(X)
  rownames(colcoor) <- colnames(X)
  
  lambda.val <- 1

  #UB page 24
  if (lambda.scal) {
    lamb.4 <- r*sum(colcoor*colcoor)/(c*sum(rowcoor*rowcoor))
    lambda.val <- sqrt(sqrt(lamb.4))
  }
  
  rowcoor = rowcoor*lambda.val
  colcoor = colcoor/lambda.val
  
  #plotting: combine the samples for bp update
  Z <- rbind(rowcoor,colcoor)
  rownames(Z) <- c(rownames(X),colnames(X))
  
  #updating group information for two-way contingency table
  bp$g <- 2
  
  if(is.null(names(dimnames(X)))) {
    bp$g.names <- c("Row","Column")
  } else {
  bp$g.names <- c(names(dimnames(X))[1],names(dimnames(X))[2])}
  bp$group.aes <- as.factor(c(rep(names(dimnames(X))[1],r),rep(names(dimnames(X))[2],c)))
  
  bp$Z <- Z
  bp$r <- r
  bp$c <- c
  bp$Dc <- Dc
  bp$Rc <- Dr
  bp$Dch <- Dch
  bp$Drh <- Drh
  bp$rowcoor <- rowcoor         
  bp$colcoor <- colcoor
  bp$P <- P
  bp$Smat <- Smat
  bp$SVD <- svd.out
  bp$e.vects <- e.vects
  bp$dim.biplot <- dim.biplot
  bp$lambda.val <- lambda.val
  bp$gamma <- gamma

  class(bp) <- append(class(bp), "CA")
  bp
}