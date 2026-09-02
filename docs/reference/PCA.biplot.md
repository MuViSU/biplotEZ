# Calculate elements for the PCA biplot

This function performs calculations for the construction of a PCA
biplot.

## Usage

``` r
# S3 method for class 'biplot'
PCA(
  bp,
  dim.biplot = c(2, 1, 3),
  e.vects = 1:ncol(bp$X),
  group.aes = NULL,
  show.class.means = FALSE,
  correlation.biplot = FALSE,
  ...
)
```

## Arguments

- bp:

  an object of class `biplot` obtained from preceding function
  [`biplot()`](biplot.md).

- dim.biplot:

  the dimension of the biplot. Only values `1`, `2` and `3` are
  accepted, with default `2`.

- e.vects:

  the vector indicating which eigenvectors (principal components) should
  be plotted in the biplot, with default `1:dim.biplot`.

- group.aes:

  a vector of the same length as the number of rows in the data matrix
  for differentiated aesthetics for samples.

- show.class.means:

  a logical value indicating whether group means should be plotted in
  the biplot.

- correlation.biplot:

  a logical value. If `FALSE`, the distances between sample points are
  optimally approximated in the biplot. If `TRUE`, the correlations
  between variables are optimally approximated by the cosine of the
  angles between axes. Default is `FALSE`.

- ...:

  additional arguments.

## Value

an object of class `PCA`, inherits from class `biplot`.

## Examples

``` r
biplot(iris[,1:4]) |> PCA()
#> Object of class biplot, based on 150 samples and 4 variables.
#> 4 numeric variables.
# create a PCA biplot
biplot(data = iris) |> PCA() |> plot()

```
