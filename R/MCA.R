# ----------------------------------------------------------------------------------------------
#' Multiple Correspondence Analysis (MCA) method
#' 
#' @description
#' This function produces a list of elements to be used for MCA biplot construction.
#' 
#' @param bp object of class \code{biplot} obtained from preceding function \code{biplot(center = FALSE)}.
#' @param dim.biplot dimension of the biplot. Only values 1, 2 and 3 are accepted, with default \code{2}.
#' @param e.vects which eigenvectors (canonical variates) to extract, with default \code{1:dim.biplot}.
#' @param variant which multiple correspondence analysis variant, with default an only option in the current version being "Indicator", SVD performed on the weighted indicator matrix..
#' @param lvl.num logical value to specify the labels of the category levels, with default \code{TRUE} to indicate levels with numbers (V1, V2, etc.). Alternatively the level names are used.
#'
#' @return A list with the following components is available:
#' \item{Z}{Combined data frame of the sample and category level coordinates.}
#' \item{Zprin}{Sample coordinates (principal coordinates).}
#' \item{CLPstan}{Category level point coordinates (standard coordinates).}
#' \item{CLPnames}{Labels of the category level points as determined by \code{lvl.num} specification.}
#' \item{g}{the number of groups for MCA will always be 2.}
#' \item{g.names}{the group names refer to samples and variables in MCA}
#' \item{group.aes}{the vector of category levels for the grouping variable. This is to be used for \code{colour}, \code{pch} and \code{cex} specification.}
#' \item{dim.biplot}{The dimension of the biplot.}
#' \item{e.vects}{Depending on what was specified in \code{MCA} argument.}
#' 
#' @seealso [biplot()]
#'
#' @usage MCA(bp, dim.biplot = c(2,1,3), e.vects = 1:ncol(bp$Xcat), 
#'        variant = "Indicator", lvl.num = TRUE)
#' @aliases MCA
#'
#' @export
#'
#' @examples
#' biplot(as.data.frame(HairEyeColor)[,-4], center = FALSE) |> 
#' MCA(lvl.num = FALSE) |>  samples(col=c("purple3","forestgreen"), 
#' pch=c(15,17), label = TRUE, label.offset = 1, label.col="gray37") |> 
#' plot()

MCA <- function(bp, dim.biplot = c(2,1,3), e.vects = 1:ncol(bp$Xcat), variant = "Indicator", lvl.num = TRUE)
{
  UseMethod("MCA")
}

#' MCA biplot
#'
#' @description Performs calculations for an MCA biplot.
#'
#' @inheritParams MCA
#'
#' @return an object of class MCA, inherits from class biplot.
#' @export
#'
#' @examples
#' biplot(as.data.frame(HairEyeColor)[,-4], center = FALSE) |> 
#' MCA(lvl.num = FALSE) |>  samples(col=c("purple3","forestgreen"), 
#' pch=c(15,17), label = TRUE, label.offset = 1, label.col="gray37") |> 
#' plot()
#'
MCA.biplot <- function(bp, dim.biplot = c(2,1,3), e.vects = 1:ncol(bp$Xcat), variant = "Indicator", lvl.num = TRUE)
{
  if (bp$center == TRUE) 
  {  warning (paste("Centering was not performed. Set biplot(center = FALSE) when performing MCA()."))}
  if (bp$scale == TRUE) 
  {  warning (paste("Scaling was not performed. Set biplot(scale = FALSE) when performing MCA()."))}
  
  dim.biplot <- dim.biplot[1]
  if (dim.biplot != 1 & dim.biplot != 2 & dim.biplot != 3) stop("Only 1D, 2D and 3D biplots")
  e.vects <- e.vects[1:dim.biplot]
  
  if (is.na(match(variant, "Indicator"))) 
    stop("only Indicator variant accepted for the current CRAN version\n")
  
  #manual MCA steps
  Glist <- vector("list")
  for (i in 1:bp$p2) Glist[[i]] <- indmat(bp$Xcat[, i])
  Gmat.colnames <- dimnames(Glist[[1]][[2]])
  for (i in 2:bp$p2) Gmat.colnames <- append(Gmat.colnames, dimnames(Glist[[i]])[[2]])
  Gmat <- matrix(unlist(Glist), nrow = bp$n) #indicator matrix
  
  ncat <- ncol(Gmat)                #number of CLs
  
  if (variant == "Indicator") {
    Gmat = Gmat
  } else #Burt
    Gmat <- t(Gmat) %*% Gmat          #Burt matrix
  #EMCmat <- Gmat %*% t(Gmat)/p      #EMC matrix
  
  ### shared methodology
  pData <- as.matrix(Gmat/sum(Gmat))	
  rPro <- rowSums(pData)						#row masses
  cPro <- colSums(pData)						#column masses
  Dr <- diag(rPro)							    #diagonal matrix of row masses
  Dc <- diag(cPro)							    #diagonal matrix of column masses
  Drh <- diag(diag(1/Dr^0.5))				#weighted row masses
  Dch <- diag(diag(1/Dc^0.5))				#weighted column masses
  Emat <- rPro%o%cPro						
  Smat <- sqrt(solve(Dr))%*%(pData-Emat)%*%sqrt(solve(Dc))
  
  svd.out <- svd(Smat)		
  
  if (variant == "Indicator") {
    Zprin <- diag(diag(Dr)^(-0.5))%*%svd.out[[2]]%*%diag(svd.out[[1]])
    CLPstan <- diag(diag(Dc)^(-0.5))%*%svd.out[[3]]
  } else {
    Zprin <- diag(diag(Dr)^(-0.5))%*%svd.out[[2]]%*%diag(sqrt(svd.out[[1]]))
    CLPstan <- diag(diag(Dc)^(-0.5))%*%svd.out[[3]]
  }
  
  s_names <- paste0("s", seq_len(bp$n))
  clp_names <- c()
  
  if(lvl.num) {
    for (j in colnames(bp$Xcat)) {
      nlev <- nlevels(bp$Xcat[[j]])
      new_names <- paste0(j, 1:nlev)
      clp_names <- c(clp_names, new_names)
    } 
    } else 
      {
      for (j in colnames(bp$Xcat)) {
        lv <- levels(bp$Xcat[[j]])
        new_names <- paste0(j, ".", lv)
        clp_names <- c(clp_names, new_names)
      }
    }
  
  #plotting: combine the samples for bp update
  Z <- rbind(Zprin, CLPstan)  
  Z_spec <- c(rep("Samples", nrow(Zprin)), rep("Variables", nrow(CLPstan)))
  
  if (variant == "Indicator") {
  rownames(Z) <- c(s_names, clp_names) } else
  { 
    rownames(Z) <- NULL
  }
  
  bp$Gmat <- Gmat
  bp$Z <- Z
  bp$CLPnames <- clp_names
  bp$Zprin <- Zprin
  bp$CLPstan <- CLPstan
  bp$g <- 2 #indicates grouping for variables and samples
  bp$g.names <- unique(Z_spec)
  bp$group.aes <- Z_spec
  bp$dim.biplot <- dim.biplot
  bp$e.vects <- e.vects
  bp$SVD <- svd.out

  class(bp) <- append(class(bp), "MCA")
  bp
  
}