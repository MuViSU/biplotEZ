# Calculate elements for the CVA biplot

This function performs calculations for the construction of a CVA
biplot.

## Usage

``` r
# S3 method for class 'biplot'
CVA(
  bp,
  classes = bp$classes,
  dim.biplot = c(2, 1, 3),
  e.vects = 1:ncol(bp$X),
  weightedCVA = "weighted",
  show.class.means = TRUE,
  low.dim = "sample.opt"
)
```

## Arguments

- bp:

  an object of class `biplot` obtained from preceding function
  [`biplot()`](biplot.md).

- classes:

  a vector of the same length as the number of rows in the data matrix
  with the class indicator for the samples.

- dim.biplot:

  the dimension of the biplot. Only values `1`, `2` and `3` are
  accepted, with default `2`.

- e.vects:

  the vector indicating which eigenvectors (canonical variates) should
  be plotted in the biplot, with default `1:dim.biplot`.

- weightedCVA:

  a character string indicating which type of CVA to perform. One of
  "`weighted`" (default) for a weighted CVA to be performed (The
  centring matrix will be a diagonal matrix with the class sizes
  (\\\mathbf{C} = \mathbf{N}\\), "`unweightedCent`" for unweighted CVA
  to be performed (The centring matrix is the usual centring matrix
  (\\\mathbf{C} = \mathbf{I}\_{G} -
  G^{-1}\mathbf{1}\_{G}\mathbf{1}\_{G}'\\)) or "`unweightedI`" for
  unweighted CVA to be performed while retaining the weighted centroid
  (The centring matrix is an indicator matrix (\\\mathbf{C} =
  \mathbf{I}\_{G}\\)).

- show.class.means:

  a logical value indicating whether to plot the class means on the
  biplot.

- low.dim:

  a character string indicating which method to use to construct
  additional dimension(s) if the dimension of the canonical space is
  smaller than `dim.biplot`. One of "`sample.opt`" (default) for
  maximising the sample predictivity of the individual samples in the
  biplot or "`Bhattacharyya.dist`" which is based on the decomposition
  of the Bhattacharyya distance into a component for the sample means
  and a component for the dissimilarity between the sample covariance
  matrices.

## Value

an object of class `CVA`, inherits from class `biplot`.

## Examples

``` r
biplot(iris[,1:4]) |> CVA(classes=iris[,5])
#> Object of class biplot, based on 150 samples and 4 variables.
#> 4 numeric variables.
#> 3 classes: setosa versicolor virginica 
```
