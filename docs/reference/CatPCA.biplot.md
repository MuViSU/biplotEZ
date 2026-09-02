# Calculate elements for the CatPCA biplot

This function performs calculations for the construction of a CatPCA
biplot.

## Usage

``` r
# S3 method for class 'biplot'
CatPCA(
  bp,
  cat.type = NULL,
  dim.biplot = c(2, 1, 3),
  e.vects = 1:ncol(bp$X),
  group.aes = NULL,
  show.class.means = FALSE,
  epsilon = 1e-06
)
```

## Arguments

- bp:

  an object of class `biplot` obtained from preceding function
  [`biplot()`](biplot.md).

- cat.type:

  a vector indicating whether each categorical variables is `nominal` or
  `ordinal`.

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

- epsilon:

  convergence criteria for iterative algorithm with defaults 1e-6.

## Value

an object of class `CatPCA`, inherits from classes `PCA` and `biplot`.

## Examples

``` r
biplot(iris) |> CatPCA()
#> Object of class biplot, based on 150 samples and 5 variables.
#> 4 numeric variables.
#> 1 categorical variable.
# create a CatPCA biplot
biplot(data = iris) |> CatPCA() |> plot()
#> Warning: The ggplot2 engine does not yet support catPCA nominal/ordinal axes; falling back to base graphics.

#> Error in scale.default(Xhat, center = -1 * x$means, scale = FALSE): length of 'center' must equal the number of columns of 'x'
```
