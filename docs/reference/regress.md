# Regression biplot method

Regression biplot method

## Usage

``` r
regress(bp, Z, group.aes=NULL, show.group.means = TRUE, 
               axes = c("regression", "splines"))
```

## Arguments

- bp:

  an object of class `biplot` obtained from preceding function
  [`biplot()`](biplot.md).

- Z:

  the matrix of coordinates of the samples

- group.aes:

  vector of the same length as the number of rows in the data matrix for
  differentiated aesthetics for samples.

- show.group.means:

  logical, indicating whether group means should be plotted in the
  biplot.

- axes:

  the type of axes to be fitted to the biplot. Options are 'regression'
  for linear regression axes (default) and 'splines' for B-spline axes.

## Value

Object of class biplot

## Examples

``` r
biplot(iris[,1:4]) |> regress(Z=cmdscale(dist(iris[,1:4]))) |> plot()
```
