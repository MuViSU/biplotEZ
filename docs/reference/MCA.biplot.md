# MCA biplot

Performs calculations for an MCA biplot.

## Usage

``` r
# S3 method for class 'biplot'
MCA(
  bp,
  dim.biplot = c(2, 1, 3),
  e.vects = 1:ncol(bp$Xcat),
  variant = "Indicator",
  lvl.num = TRUE
)
```

## Arguments

- bp:

  object of class `biplot` obtained from preceding function
  `biplot(center = FALSE)`.

- dim.biplot:

  dimension of the biplot. Only values 1, 2 and 3 are accepted, with
  default `2`.

- e.vects:

  which eigenvectors (canonical variates) to extract, with default
  `1:dim.biplot`.

- variant:

  which multiple correspondence analysis variant, with default an only
  option in the current version being "Indicator", SVD performed on the
  weighted indicator matrix..

- lvl.num:

  logical value to specify the labels of the category levels, with
  default `TRUE` to indicate levels with numbers (V1, V2, etc.).
  Alternatively the level names are used.

## Value

an object of class MCA, inherits from class biplot.

## Examples

``` r
biplot(as.data.frame(HairEyeColor)[,-4], center = FALSE) |> 
MCA(lvl.num = FALSE) |>  samples(col=c("purple3","forestgreen"), 
pch=c(15,17), label = TRUE, label.offset = 1, label.col="gray37") |> 
plot()

```
