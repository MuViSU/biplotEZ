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
#'  principal coordinates. \code{variant = "symmetric"}, presents a symmtric biplot with row and column standard coordinates scaled 
#'  equally by the singular values. 
#'
#' @return A list with the following components is available:
#' \item{Z}{matrix with each row containing the details of the point to be plotted (i.e. coordinates).}
#' \item{rowcoor}{row coordinates based on the selected \code{variant}.}
#' \item{colcoor}{column coordinates based on the selected \code{variant}.}
#' \item{rowPrinc}{row principal coordinates from the SVD of the Pearson residuals.}
#' \item{colPrinc}{column principal coordinates from the SVD of the Pearson residuals.}
#' \item{rowStand}{row standard coordinates from the SVD of the Pearson residuals.}
#' \item{colStand}{column standard coordinates from the SVD of the Pearson residuals.}
#' \item{P}{correspondence matrix}
#' \item{Smat}{Pearson residuals.}
#' \item{SVD}{singular value decomposition solution: \code{d, u, v}.}
#' \item{qual}{quality of the approximation.}
#' \item{dim.biplot}{dimension of the biplot}
#'
#' @usage CA(bp, dim.biplot = c(2,1,3), e.vects = 1:ncol(bp$X), variant = "Princ")
#' @aliases CA
#'
#' @export
#'
#' @examples
#' # Creating a CA biplot with rows in principal coordinates:
#' biplot(HairEyeColor[,,2], center = FALSE) |> CA(variant = "Princ") |> 
#' samples(col=c("magenta","purple"), pch=c(15,17)) |> plot()
#' # Creating a CA biplot with rows in standard coordinates:
#' biplot(HairEyeColor[,,2], center = FALSE) |> CA(variant = "Stand") |> 
#' samples(col=c("magenta","purple"), pch=c(15,17)) |> plot()
#' # Creating a CA biplot with rows and columns scaled equally:
#' biplot(HairEyeColor[,,2], center = FALSE) |> CA(variant = "Symmetric") |> 
#' samples(col=c("magenta","purple"), pch=c(15,18)) |> plot()

CA <- function(bp, dim.biplot = c(2,1,3), e.vects = 1:ncol(bp$X), variant = "Princ")
  #, lambda.scal = TRUE)
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
CA.biplot <- function(bp, dim.biplot = c(2,1,3), e.vects = 1:ncol(bp$X), variant = "Princ")#, lambda.scal = TRUE)
{
  
  if (bp$center == TRUE) 
  {  warning (paste("Centering was not performed. Set biplot(center = FALSE) when performing CA()."))}
  if (bp$scale == TRUE) 
  {  warning (paste("Scaling was not performed. Set biplot(scale = FALSE) when performing CA()."))}
  
  dim.biplot <- dim.biplot[1]
  if (dim.biplot != 1 & dim.biplot != 2 & dim.biplot != 3) stop("Only 1D, 2D and 3D biplots")
  e.vects <- e.vects[1:dim.biplot]
  
    #standard CA method
  X <- bp$raw.X #to ensure that unscaled and uncentered frequency table is used
  n <- bp$n
  p <- bp$p
  N <- sum(X)
  P <- X/N      #using the correspondence matrix
  
  rMass <- rowSums(P)									
  cMass <- colSums(P)									
  Dr <- diag(apply(P, 1, sum))				    #diagonal matrix of column profiles
  Dc <- diag(apply(P, 2, sum))				    #diagonal matrix of row profiles
  Drh <- sqrt(solve(Dr))	    						#weighted column profiles
  Dch <- sqrt(solve(Dc))			    				#weighted row profiles
  Emat <- (Dr %*%matrix(1, nrow = n, ncol = p) %*%  Dc)
  Smat <- Drh%*%(P-Emat)%*%Dch				    #standardised residuals
  
  svd.out <- svd(Smat)
  
  ##principal coordinates
  Rprin <- sqrt(solve(Dr))%*%svd.out[[2]]%*%diag(svd.out[[1]])
  Cprin <- sqrt(solve(Dc))%*%svd.out[[3]]%*%diag(svd.out[[1]])
  ##standard coordinates
  Rstan <- sqrt(solve(Dr))%*%svd.out[[2]]
  Cstan <- sqrt(solve(Dc))%*%svd.out[[3]]

  bipl.qual <- sum((svd.out[[1]]^2)[e.vects[1:dim.biplot]])/sum((svd.out[[1]]^2))*100 

  if(variant == "Stand") gamma <- 0
  if(variant == "Princ") gamma <- 1
  if(variant == "Symmetric") gamma <- 0.5
    
  rowcoor <- svd.out[[2]]%*%(diag(svd.out[[1]])^gamma)
  colcoor <- svd.out[[3]]%*%(diag(svd.out[[1]])^(1-gamma))
    
  #To incorporate labmda scaling
  #vdhalf <- svd.out[[2]]%*% diag(sqrt(svd.out[[1]]))
  #udhalf <- svd.out[[3]]%*% diag(sqrt(svd.out[[1]]))

  #to plot combine rowpcoord and colcoord in one dataframe
  # if (lambda.scal) {
  #   lam.4 <- p*sum(vdhalf*vdhalf)/(p*sum(udhalf*udhalf))
  #   lam <- sqrt(sqrt(lam.4))
  #   print(lam)
  #   udhalf <- udhalf*lam
  #   vdhalf <- vdhalf/lam
  #   
  #   rowcoor = rowcoor*lam
  #   colcoor = colcoor/lam
  # }
  
  #plotting: combine the samples to utilise plot
  Z <- rbind(rowcoor,colcoor)[,1:2]
  rownames(Z) <- c(colnames(X),rownames(X))
  
  #updating group information for two-way contingency table
  bp$g <- 2
  bp$g.names <- c(names(dimnames(X))[1],names(dimnames(X))[2])
  bp$group.aes <- as.factor(c(rep(names(dimnames(X))[1],n),rep(names(dimnames(X))[2],p)))
  
  bp$Z <- Z
  bp$rowcoor <- rowcoor         
  bp$colcoor <- colcoor
  bp$rowPrinc <- Rprin
  bp$colPrinc <- Cprin
  bp$rowStand <- Rstan
  bp$colStand <- Cstan
  bp$P <- P
  bp$Smat <- Smat
  bp$SVD <- svd.out
  bp$qual <- bipl.qual
  bp$dim.biplot <- dim.biplot
 # bp$lambda <- lam

  class(bp) <- append(class(bp),"CA")
  bp
}
