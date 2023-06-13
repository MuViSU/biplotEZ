### MuViSU (Centre for Multi-Dimensional Data Visualisation
### biplotEZ functions
### ========================================================

#' Create a new biplot
#'
#' @param X       Default dataset to use for the biplot. Dataframe or matrix.
#' @param center  Logical value indicating whether to column centre the dataset.
#' @param scaled  Logical value indicating wehther to standardise to unit
#'                standard deviation.
#'
#' @return        returns an object of class biplot
#'                An object of class "biplot" is a list containing at least the
#'                following components
#'                X dataset ready for biplot calculations
#'                raw.X original data
#'                scaled center ...
#'                means vector of column means
#'                sd vector of column standard deviations
#' @export
#' @examples
biplot <- function(X, center = TRUE, scaled = FALSE)
{
  # check for numeric vs non-numeric columns
  # check for missing values
  # more options for scaled, including value is a function

  # scaling of numeric data
  X <- as.matrix(X)
  raw.X <- X
  means <- apply(X, 2, mean)
  sd <- apply(X, 2, stats::sd)
  if (!center) {  X <- X
                  means <- rep(0, ncol(X))
                  sd <- rep(1, ncol(X))     }
  else { if (scaled) X <- scale(X)
         else { X <- scale(X, scale = FALSE)
                sd <- rep(1, ncol(X))        }
       }

  # making sure we have row and column names

  if (is.null(rownames(X))) rownames(X) <- paste(1:nrow(X))
  if (is.null(colnames(X))) colnames(X) <- paste("V", 1:ncol(X), sep = "")

  # X is the matrix of numeric variables
  # Xcat is the dataframe of non-numeric variables

  object <- list(X = X, raw.X = raw.X, center=center, scaled=scaled, means = means, sd = sd)
  class(object) <- "biplot"
  object
}

# -------------------------------------------------------------------------------------------
#' PCA method
#'
#' @param bp object of class biplot
#' @param e.vects which principal components to extract, defaults to first two
#' @param ... more stuff
#'
#' @return  object of class PCA
#' @export
#'
#' @examples
PCA <- function (bp, e.vects=1:2, ...)
{
  UseMethod("PCA")
}

#' PCA biplot
#'
#' @inheritParams PCA
#'
#' @return object of class PCA
#' @export
#'
#' @examples
PCA.biplot <- function (bp, e.vects=1:2, ...)
{
  if (!bp$center)
  {
    warning("PCA requires a centred datamatrix. Your data was centred before computation. Use center = TRUE in the call to biplot()")
    bp <- biplot (bp$raw.X, center = TRUE, bp$scaled)
  }
  X <- bp$X

  out <- svd(X)
  Z <- out$u[,e.vects] %*% diag(out$d[e.vects])
  bp$samples = list(Z = Z, Vr=out$v[,e.vects])
  bp
}

# ----------------------------------------------------------------------------------------------
#' Formatting of samples in biplot
#'
#' @param bp biplot objects
#' @param col sample colour
#' @param pch sample plotting character
#' @param cex sample character expansion
#' @param label logical, whether samples should be labelled or not
#' @param label.cex label character expansion
#' @param label.side side of the plotting character where label appears
#' @param connected logical, whether samples are connected in order of rows of data matrix
#' @param alpha opacity
#'
#' @return
#' @export
#'
#' @examples
samples <- function (bp, col = c("blue","green","gold","cyan","magenta","black","red","grey","purple","salmon"),
                         pch = 3, cex = 1, label = F, label.cex = 0.75, label.side = "bottom",
                         connected=F, alpha = 1)
{
   J <- ncol(bp$X)
   while (length(col) < J) col <- c(col, col)
   col <- as.vector(col[1:J])
   while (length(pch) < J) pch <- c(pch, pch)
   pch <- as.vector(pch[1:J])
   while (length(cex) < J) cex <- c(cex, cex)
   cex <- as.vector(cex[1:J])
   while (length(label) < J) label <- c(label, label)
   label <- as.vector(label[1:J])
   while (length(label.cex) < J) label.cex <- c(label.cex, label.cex)
   label.cex <- as.vector(label.cex[1:J])
   while (length(label.side) < J) label.side <- c(label.side, label.side)
   label.side <- as.vector(label.side[1:J])
   while (length(alpha) < J) alpha <- c(alpha, alpha)
   if (length(connected)>1) connected <- connected[1]
   alpha <- as.vector(alpha[1:J])

   bp$samples = list(Z = bp$samples$Z, col = col, pch = pch, cex = cex, label = label, label.cex = label.cex,
                     label.side = label.side, connected=connected, alpha = alpha)
   bp
}

# ----------------------------------------------------------------------------------------------
#' Plotting of the biplot
#'
#' @param x object of class biplot
#' @param y NULL
#' @param ... more arguments
#'
#' @return biplot is created on graphics device
#' @export
#'
#' @examples
plot.biplot <- function(x, y = NULL, ...)
{
  sample.components <- names(x$samples)
  if (!("col") %in% sample.components)
    x$samples$col <- c("blue","green","gold","cyan","magenta","black","red","grey","purple","salmon")

  plot(x$samples$Z, col=x$samples$col)
  invisible(NULL)
}

