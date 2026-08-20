# CA biplot

Performs calculations for a CA biplot.

## Usage

``` r
# S3 method for class 'biplot'
CA(
  bp,
  dim.biplot = c(2, 1, 3),
  e.vects = 1:ncol(bp$X),
  variant = "Princ",
  lambda.scal = FALSE
)
```

## Arguments

- bp:

  object of class `biplot` obtained from preceding function
  `biplot(center = FALSE)`. In order to maintain the frequency table,
  the input should not be centered or scaled. For `CA`, `bp` should be a
  contingency table.

- dim.biplot:

  dimension of the biplot. Only values 1, 2 and 3 are accepted, with
  default `2`.

- e.vects:

  which eigenvectors (canonical variates) to extract, with default
  `1:dim.biplot`.

- variant:

  which correspondence analysis variant, with default "Princ", presents
  a biplot with rows in principal coordinates and columns in standard
  coordinates. `variant = "Stand"`, presents a biplot with rows in
  standard coordinates and columns in principal coordinates.
  `variant = "symmetric"`, presents a symmetric biplot with row and
  column standard coordinates scaled equally by the singular values.

- lambda.scal:

  logical value to request lambda-scaling, default is `FALSE`. Controls
  stretching or shrinking of column and row distances.

## Value

an object of class CA, inherits from class biplot.

## Examples

``` r
biplot(HairEyeColor[,,2], center = FALSE) |> CA() |> plot()
#> Warning: The ggplot2 engine does not yet support CA maps; falling back to base graphics.

```
