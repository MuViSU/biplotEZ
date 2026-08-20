# Generic Plotting function of objects of class biplot

Generic Plotting function of objects of class biplot

## Usage

``` r
# S3 method for class 'biplot'
plot(
  x,
  engine = c("ggplot2", "base"),
  exp.factor = 1.2,
  axis.predictivity = NULL,
  sample.predictivity = NULL,
  zoom = FALSE,
  add = FALSE,
  xlim = NULL,
  ylim = NULL,
  ...
)
```

## Arguments

- x:

  An object of class `biplot`.

- engine:

  whether to plot in base R or `ggplot2`.

- exp.factor:

  a numeric value with default axes of the biplot. Larger values are
  specified for zooming out with respect to sample points in the biplot
  display and smaller values are specified for zooming in with respect
  to sample points in the biplot display.

- axis.predictivity:

  either a logical or a numeric value between `0` and `1`. If it is a
  numeric value, this value is used as threshold so that only axes with
  axis predictivity larger than the threshold is displayed. If
  `axis.predictivity = TRUE`, the axis colour is 'diluted' in proportion
  with the axis predictivity.

- sample.predictivity:

  either a logical or a numeric value between 0 and 1. If it is a
  numeric value, this value is used as threshold so that only samples
  with sample predictivity larger than the threshold is displayed. If
  `sample.predictivity = TRUE`, the sample size is shrinked in
  proportion with the sample predictivity.

- zoom:

  a logical value allowing the user to select an area to zoom into.

- add:

  a logical value allowing the user to add the biplot to a current plot.
  If `add = TRUE` the argument `zoom` is inactive.

- xlim:

  the horizontal limits of the plot.

- ylim:

  the vertical limits of the plot.

- ...:

  additional arguments.

## Value

An object of class `biplot`.

## Examples

``` r
biplot (iris[,1:4]) |> PCA() |> plot()
```
