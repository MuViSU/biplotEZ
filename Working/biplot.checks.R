
#'
#' @description
#'  This function produces a list of elements to be used when producing a biplot.
#'
#' @param datmat A dataframe or matrix containing all variables the user wants to analyse
#' @param center TRUE or FALSE
#' @param scaled TRUE or FALSE
#' @param group.aes Variable from the data to be used as a grouping variable
#' @param Title Title
#'
#' @return
#' \itemize{
#' \item{X}{Matrix of the centered and scaled numeric variables}
#' \item{raw.X}{Original data}
#' \item{center}{TRUE or FALSE, should the numeric data be centred?}
#' \item{scaled}{TRUE or FALSE, should the numeric data be scaled?}
#' \item{means}{Vector of means for each numeric variable}
#' \item{sd}{Vector of standard deviations for each numeric variable}
#' \item{group.aes}{Vector of category levels for the grouping variable. This is to be used for color, pch and cex specifications}
#' }
#' @export
#'
#' @examples
#' biplot(datmat = iris)
biplot <- function(datmat, center = TRUE, scaled = FALSE,group.aes = NULL,Title = NULL)
{
  # check for missing values
  ##for numeric: na.omit(X), na.action is a vector of row numbers that were deleted
  ##for categorical: create new level called "missing" warning: "new level 'missing' created in [name columns]
  ##for both: add warning "m rows deleted due to missing values"
  # more options for scaled, including value is a function
  dim.mat<-dim(datmat)
  if(is.null(dim.mat)) stop("Not enough variables to construct a biplot \n Consider using data with more columns")
  ##Creating vector of row indices containing missing values
  na.vec.df<-na.action(na.omit(datmat))
  if(length(na.vec.df)==nrow(datmat)) {stop("No observations left after deleting missing observations")
  } else if(!is.null(na.vec.df)) {print(paste("Warning:",length(na.vec.df),"rows deleted due to missing values"))
  } else if(is.null(na.vec.df)) {datmat <- datmat
    }

  datmat<-datmat[complete.cases(datmat),]
  raw.X <- datmat
  ##Separating numeric and categorical data
  # X is the matrix of numeric variables, if there are no numeric columns, then X = NULL
  # Xcat is the dataframe of non-numeric variables, if there are no categorical columns, then Xcat = NULL
  ###Function for ncol>1
  temp.func<-function(datmat){
    type.vec<-NULL
    for(j in 1:ncol(datmat)){
      if(is.numeric(datmat[,j])){type.vec[j] <- "num"}
      else if(is.factor(datmat[,j])) {type.vec[j] <- "cat"}
      else NA}
    if("num"%in%type.vec){X<-as.matrix(datmat[,type.vec == "num",drop = FALSE])
    } else if("num"%in%type.vec==FALSE) {X <- NULL}
    if("cat"%in%type.vec){Xcat<-as.data.frame(datmat[,type.vec == "cat",drop = FALSE])
    } else if("cat"%in%type.vec==FALSE) {Xcat <- NULL}
    list(X = X, Xcat = Xcat)
  }

  if(is.null(dim.mat)&&is.numeric(datmat)){X<-datmat
  Xcat<-NULL
  } else if(is.null(dim.mat)&&is.factor(datmat)) {Xcat<-datmat
  X<-NULL
  } else if(dim.mat[2]>1) {X<-temp.func(datmat)$X
  Xcat<-temp.func(datmat)$Xcat}

  # scaling of numeric data
  if(is.null(X)){means <- NULL
  sd <- NULL
  } else if(!is.null(X)){
    X <- as.matrix(X)
    means <- apply(X, 2, mean)
    sd <- apply(X, 2, stats::sd)
    if (!center) {  X <- X
    means <- rep(0, ncol(X))
    sd <- rep(1, ncol(X))
    } else if (scaled) {X <- scale(X)
    } else {X <- scale(X, scale = FALSE)
    sd <- rep(1, ncol(X))
    if (is.null(rownames(X))) rownames(X) <- paste(1:nrow(X))
    if (is.null(colnames(X))) colnames(X) <- paste("V", 1:ncol(X), sep = "")
    }
    # making sure we have row and column names
    if (is.null(rownames(X))) rownames(X) <- paste(1:nrow(X))
    if (is.null(colnames(X))) colnames(X) <- paste("V", 1:ncol(X), sep = "")
    }

  if(is.null(group.aes)){group.aes <- rep(1,nrow(datmat))}
  g.names<-levels(factor(group.aes))
  g<-length(g.names)
  object <- list(X = X, raw.X = raw.X, center=center, scaled=scaled,
                 means = means, sd = sd, group.aes = group.aes,g.names = g.names,g = g,Title = Title)
  class(object) <- "biplot"
  object
}
