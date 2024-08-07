% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/more_biplots.R
\name{classification.biplot}
\alias{classification.biplot}
\title{classification biplot}
\usage{
\method{classification}{biplot}(
  bp,
  Pmat,
  dim.biplot = c(2, 1, 3),
  e.vects = 1:ncol(bp$X),
  group.aes = NULL,
  axes = "regression",
  col = ez.col,
  opacity = 0.4,
  borders = FALSE
)
}
\arguments{
\item{bp}{an object of class \code{biplot} obtained from preceding function \code{biplot()}.}

\item{Pmat}{a matrix containing the posterior probability for the classes}

\item{dim.biplot}{dimension of the biplot. Only values 1, 2 and 3 are accepted, with default \code{2}.}

\item{e.vects}{which eigenvectors (principal components) to extract, with default \code{1:dim.biplot}.}

\item{group.aes}{vector of the same length as the number of rows in the data matrix
for differentiated aesthetics for samples.}

\item{axes}{type of axes, defaults to "regression"}

\item{col}{colour of the classification regions}

\item{opacity}{opacity of classification regions}

\item{borders}{logical, indicating whether borders should be added to classification regions}
}
\value{
an object of class biplot.
}
\description{
Performs calculations for a classification biplot.
}
