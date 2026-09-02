# Creates a kernel density in 1-dimension

Creates a kernel density in 1-dimension

## Usage

``` r
density1D(
  bp,
  which = NULL,
  h = "nrd0",
  kernel = "gaussian",
  col = ez.col,
  lwd = 1.5,
  legend.mar = c(2, 5, 0, 5)
)
```

## Arguments

- bp:

  object of class `biplot`

- which:

  which group.

- h:

  bandwidth.

- kernel:

  character string giving the smoothing kernel to be used.

- col:

  colours to be used for each of the density curves.

- lwd:

  linewidth of density curve.

- legend.mar:

  The margin line of the legend.

## Value

An object of class `biplot`.

## Examples

``` r
biplot (iris,classes=iris[,5]) |> CVA(dim=1) |> density1D() |> plot()
#> Warning: The ggplot2 engine does not yet support 1D biplots; falling back to base graphics.
```
