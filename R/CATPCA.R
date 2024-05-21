#' Categorical Principal Component Analysis
#'
#' @param bp an object of class \code{biplot} obtained from preceding function \code{biplot()}.
#' @param dim.biplot dimension of the biplot. Only values 1, 2 and 3 are accepted, with default \code{2}.
#' @param e.vects which eigenvectors (principal components) to extract, with default \code{1:dim.biplot}.
#'
#' @return
#' @export
#'
#' @examples
CATPCA<-function(bp){
  indmat  <- function (groep.vec) 
  {  elements <- levels(factor(groep.vec))
  Y <- matrix(0, nrow = length(groep.vec), ncol = length(elements))
  dimnames(Y) <- list(NULL, paste(elements))
  for (i in 1:length(elements)) Y[groep.vec == elements[i], i] <- 1
  return(Y)
  }
  
  biplot.check.G <- function (G, n) 
  {  if (is.null(G)) { G <- matrix(indmat(rep(1, n)), ncol = 1)
  dimnames(G) <- list(1:n, "AllData")      }
    if (nrow(G) != n) stop("number of rows of X and G differ")
    if (is.null(dimnames(G))) dimnames(G) <- list(NULL, paste("class", 1:ncol(G), sep = ""))
    if (length(dimnames(G)[[2]]) == 0) dimnames(G)[[2]] <- paste("class", 1:ncol(G), sep = "")
    if (ncol(G) == 1) class.vec <- rep(dimnames(G)[[2]], n) else class.vec <- apply(t(apply(G, 1, function(x) x == max(x))), 1, function(s, G) dimnames(G)[[2]][s], G = G)
    G
  }
  ##Possibly combine these two functions to obtain indicator matrices and overall indicator matrix (could be useful for MCA as well)
}
