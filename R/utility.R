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

