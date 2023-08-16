
<!-- README.md is generated from README.Rmd. Please edit that file -->

# biplotEZ

<!-- badges: start -->
<!-- badges: end -->

The goal of biplotEZ is to provide users an EZ-to-use platform for
visually representing their data with biplots. Currently, this package
includes principal component analysis (PCA) and canonical variate
analysis (CVA) biplots. This is accompanied by various formatting
options for the samples and axes. Alpha-bags and concentration ellipses
are included for visual enhancements and interpretation.

## Installation

You can install the development version of biplotEZ like this:

``` r
library(githubinstall)
githubinstall("biplotEZ")
```

or alternatively

``` r
library(devtools)
devtools::install_github("MuViSU/biplotEZ")
```

## Example

This is a basic example which shows you how to construct a PCA biplot:

``` r
library(biplotEZ)
#> 
#> Attaching package: 'biplotEZ'
#> The following object is masked from 'package:stats':
#> 
#>     biplot
biplot (iris[,1:4], Title="Test PCA biplot") |> PCA() |> plot()
```

<img src="man/figures/README-PCAexample-1.png" width="100%" />

While the PCA biplot provides a visual representation of the overall
data set, optimally representing the variance in 1, 2 or 3 dimensions,
the CVA biplot aims to optimally separate specified groups in the data.
This is a basic example which shows you how to constrct a CVA biplot:

``` r
biplot (iris[,1:4], group.aes=iris[,5], Title="Test CVA biplot") |> CVA() |> plot()
```

<img src="man/figures/README-CVAexample-1.png" width="100%" />

An over-the-top example of chaning all the formatting and adding all the
bells and whistles:

``` r
biplot (iris[,1:4], group.aes=iris[,5]) |> PCA() |> 
  samples(col="gold", pch=15) |>
  axes(which=2:3, col="cyan", label.cex=1.2, tick.col="blue", tick.label=FALSE) |>
  alpha.bags (alpha=c(0.5,0.75,0.95), which=1, col=rainbow(3), lty=3) |>
  concentration.ellipse(alpha=c(0.9,0.9), which=2:3, col=c("green","olivedrab"), lwd=3) |>
  legend.type(bags = TRUE) |>
  plot()
#> Computing 0.5 -bag for setosa 
#> Computing 0.75 -bag for setosa 
#> Computing 0.95 -bag for setosa 
#> [1] 1 1 1 2 2 2
#> [1] 1 1 1 2 2 2
#> Computing 2.14596602628935 -ellipse for versicolor 
#> Computing 2.14596602628935 -ellipse for virginica 
#> Computing 2.14596602628935 -ellipse for versicolor 
#> Computing 2.14596602628935 -ellipse for virginica
```

<img src="man/figures/README-aes_example-1.png" width="100%" /> \## Bug
Reports and Support If you encounter any issues or have questions,
please open an issue on the GitHub repository.
