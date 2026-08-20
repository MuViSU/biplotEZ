# ggplot2 plot method for biplotEZ objects

ggplot2 plot method for biplotEZ objects

## Usage

``` r
gg_biplot(
  x,
  exp.factor = 1.2,
  axis.predictivity = NULL,
  sample.predictivity = NULL,
  xlim = NULL,
  ylim = NULL,
  square = TRUE,
  draw = TRUE,
  legend = NULL,
  ...
)
```

## Arguments

- x:

  an object of class `biplot` with a method (PCA, CVA, ...) applied.

- exp.factor:

  expansion factor for the plotting region.

- axis.predictivity:

  logical or numeric in (0,1); as in `plot.biplot`.

- sample.predictivity:

  logical or numeric in (0,1); as in `plot.biplot`.

- xlim, ylim:

  optional axis limits.

- square:

  logical; pad the narrower data range so the panel is square, mimicking
  base `pty = "s"` with `asp = 1`. Default TRUE.

- legend:

  logical; show the ggplot legend for sample groups (default: shown when
  there is more than one group). Legends for bags, ellipses and means
  are controlled by [`legend.type()`](legend.type.md).

- ...:

  reserved.

## Value

the biplot object, invisibly carrying the ggplot in `$gg`, with class
`gg_biplot` prepended so that printing displays the plot.
