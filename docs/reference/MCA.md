# Multiple Correspondence Analysis (MCA) method

This function produces a list of elements to be used for MCA biplot
construction.

## Usage

``` r
MCA(bp, dim.biplot = c(2,1,3), e.vects = 1:ncol(bp$Xcat), 
       variant = "Indicator", lvl.num = TRUE)
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

A list with the following components is available:

- Z:

  Combined data frame of the sample and category level coordinates.

- Zprin:

  Sample coordinates (principal coordinates).

- CLPstan:

  Category level point coordinates (standard coordinates).

- CLPnames:

  Labels of the category level points as determined by `lvl.num`
  specification.

- g:

  the number of groups for MCA will always be 2.

- g.names:

  the group names refer to samples and variables in MCA

- group.aes:

  the vector of category levels for the grouping variable. This is to be
  used for `colour`, `pch` and `cex` specification.

- dim.biplot:

  The dimension of the biplot.

- e.vects:

  Depending on what was specified in `MCA` argument.

## See also

[`biplot()`](biplot.md)

## Examples

``` r
biplot(as.data.frame(HairEyeColor)[,-4], center = FALSE) |> 
MCA(lvl.num = FALSE) |>  samples(col=c("purple3","forestgreen"), 
pch=c(15,17), label = TRUE, label.offset = 1, label.col="gray37") |> 
plot()
```
