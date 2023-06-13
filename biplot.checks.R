#biplot.check.G function
#biplot.check.X function 

biplot.check.G <- function (G, n) 
{  if (is.null(G)) { G <- matrix(indmat(rep(1, n)), ncol = 1)
dimnames(G) <- list(1:n, "AllData")      }
  if (nrow(G) != n) stop("number of rows of X and G differ")
  if (is.null(dimnames(G))) dimnames(G) <- list(NULL, paste("class", 1:ncol(G), sep = ""))
  if (length(dimnames(G)[[2]]) == 0) dimnames(G)[[2]] <- paste("class", 1:ncol(G), sep = "")
  if (ncol(G) == 1) class.vec <- rep(dimnames(G)[[2]], n) else class.vec <- apply(t(apply(G, 1, function(x) x == max(x))), 1, function(s, G) dimnames(G)[[2]][s], G = G)
  G
}

biplot.check.X <- function (X, scaled.mat, centred.mat=TRUE) 
{  X <- as.matrix(X)
unscaled.X <- X
means <- apply(X, 2, mean)
sd <- sqrt(apply(X, 2, var))
if (!centred.mat) {  X <- X
means <- rep(0, ncol(X))
sd <- rep(1, ncol(X))     }      
else { if (scaled.mat) X <- scale(X) else { X <- scale(X, scale = FALSE)
sd <- rep(1, ncol(X))        }
}  
if (is.null(dimnames(X))) dimnames(X) <- list(paste(1:nrow(X)), paste("V", 1:ncol(X), sep = ""))
if (length(dimnames(X)[[1]]) == 0) dimnames(X)[[1]] <- paste(1:nrow(X))
if (length(dimnames(X)[[2]]) == 0) dimnames(X)[[2]] <- paste("V", 1:ncol(X), sep = "")
list(X = X, unscaled.X = unscaled.X, means = means, sd = sd)
}
