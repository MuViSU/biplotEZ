% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PCA.R
\name{PCA}
\alias{PCA}
\title{Principal Component Analysis (PCA) method}
\usage{
PCA(bp, dim.biplot = c(2, 1, 3), e.vects = 1:ncol(bp$X),
group.aes = NULL, show.class.means = FALSE, correlation.biplot = FALSE)
}
\arguments{
\item{bp}{an object of class \code{biplot} obtained from preceding function \code{biplot()}.}

\item{dim.biplot}{dimension of the biplot. Only values 1, 2 and 3 are accepted, with default \code{2}.}

\item{e.vects}{which eigenvectors (principal components) to extract, with default \code{1:dim.biplot}.}

\item{group.aes}{vector of the same length as the number of rows in the data matrix
for differentiated aesthetics for samples.}

\item{show.class.means}{logical, indicating whether group means should be plotted in the biplot.}

\item{correlation.biplot}{logical, if \code{FALSE}, the distances between sample points are
optimally approximated in the biplot. If \code{TRUE}, the correlations between
variables are optimally approximated by the cosine of the angles between
axes. Default is \code{FALSE}.}
}
\value{
Object of class PCA with the following elements:
\item{X}{matrix of the centered and scaled numeric variables.}
\item{Xcat}{data frame of the categorical variables.}
\item{raw.X}{original data.}
\item{classes}{vector of category levels for the class variable. This is to be used for \code{colour}, \code{pch} and \code{cex} specifications.}
\item{na.action}{vector of observations that have been removed.}
\item{center}{logical. indicating whether X is centered.}
\item{scaled}{logical. indicating whether X is scaled.}
\item{means}{vector of means for each numerical variable.}
\item{sd}{vector of standard deviations for each numerical variable.}
\item{n}{number of observations.}
\item{p}{number of variables.}
\item{group.aes}{vector of the same length as the number of rows in the data matrix for differentiated aesthetics for samples.}
\item{g.names}{descriptive names to be used for group labels.}
\item{g}{number of groups.}
\item{Title}{title of the biplot to be rendered.}
\item{Z}{matrix with each row containing the details of the point to be plotted (i.e. coordinates).}
\item{Lmat}{matrix for transformation to the principal components.}
\item{Linv}{inverse of the Lmat matrix.}
\item{eigenvalues}{vector of eigenvalues of the covariance matrix of \code{X}.}
\item{ax.one.unit}{one unit in the positive direction of each biplot axis.}
\item{e.vects}{vector indicating which principal components are plotted in the biplot.}
\item{Vr}{the \code{1:dim.biplot} columns of the \code{V.mat}.}
\item{dim.biplot}{dimension of the biplot.}
\item{V.mat}{the matrix containing the right singular vectors of \code{X}.}
\item{Sigma.mat}{the matrix with the singular values of \code{X} on the diagonal.}
\item{U.mat}{the matrix containing the left singular vectors of \code{X}.}
\item{class.means}{logical. indicating whether group means should be plotted in the biplot.}
\item{Zmeans}{matrix of class mean coordinates to be plotted in the biplot.}
}
\description{
This function produces a list of elements to be used for PCA biplot construction.
}
\examples{
biplot(iris[,1:4]) |> PCA()
# create a PCA biplot
biplot(data = iris) |> PCA() |> plot()
}
\references{
Gabriel, K.R. (1971) The biplot graphic display of matrices with application to principal component analysis. \emph{Biometrika.} 58(3):453–467.
}
