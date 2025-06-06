% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/biplot.R
\name{fit.measures}
\alias{fit.measures}
\title{Compute measures of fit for the biplot.}
\usage{
fit.measures(bp)
}
\arguments{
\item{bp}{an object of class \code{biplot}.}
}
\value{
An object of class \code{biplot}. The object is augmented with
additional items, depending on the type of biplot object.
\item{quality}{the overall quality of fit.}
\item{adequacy}{the adequacy of representation of variables.}

For an object of class \code{PCA}:
\item{axis.predictivity}{the fit measure of each individual axis.}
\item{sample.predictivity}{the fit measure for each individual sample.}
For an object of class \code{CVA}:
\item{axis.predictivity}{the fit measure of each individual axis.}
\item{class.predictivity}{the fit measure for each class mean.}
\item{within.class.axis.predictivity}{the fit measure for each axis based on values
expressed as deviations from their class means.}
\item{within.class.sample.predictivity}{the fit measure for each sample expressed as
deviation from its class mean.}

For an object of class \code{CA}:
\item{row.predictivity}{the fit measure for each row of the input matrix individual sample.}
\item{col.predictivity}{the fit measure for each column of the input matrix individual sample.}
\item{Xhat}{predicted matrix per row profile}
}
\description{
This function computes the measures of fit for the biplot. The biplot object is augmented with additional items, which can differ depending on the type of biplot. The measures provide information on the overall quality of fit and the adequacy of representation of variables.
}
\examples{
out <- biplot (iris[,1:4]) |> PCA() |> fit.measures()
summary(out)

}
