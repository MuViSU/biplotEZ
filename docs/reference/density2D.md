# Create a density in 2-dimensions

Create a density in 2-dimensions

## Usage

``` r
density2D(
  bp,
  which = NULL,
  contours = F,
  h = NULL,
  n = 100,
  col = c("green", "yellow", "red"),
  contour.col = "black",
  cuts = 50,
  cex = 0.6,
  tcl = -0.2,
  mgp = c(0, -0.25, 0),
  layout.heights = c(100, 10),
  legend.mar = c(2, 5, 0, 5)
)
```

## Arguments

- bp:

  object of class `biplot`

- which:

  which group to create a density; limited to only a single group at a
  time. If NULL, density drawn over all data points.

- contours:

  logical indicating whether contours are added to the density plot

- h:

  vector of bandwidths for x and y directions, see
  [`kde2d`](https://rdrr.io/pkg/MASS/man/kde2d.html).

- n:

  number of grid points in each direction. Can be scalar or a length-2
  integer vector.

- col:

  vector of colours to use to form a 'continuous' sequence of colours.

- contour.col:

  colour of the contours.

- cuts:

  number of colours in `col`.

- cex:

  character expansion.

- tcl:

  The length of tick marks as a fraction of the height of a line of
  text.

- mgp:

  The margin line.

- layout.heights:

  A vector of values for the heights of rows.

- legend.mar:

  The margin line of the legend.

## Value

An object of class `biplot`.

## Examples

``` r
biplot(iris[,1:4],group.aes = iris[,5]) |> PCA() |> 
  density2D(which=3,col=c("white","purple","cyan","blue")) |> plot()

biplot(iris[,1:4],group.aes = iris[,5]) |> PCA() |> 
  density2D(which=3,col=c("white","purple","cyan","blue"),contours = TRUE,
  contour.col = "grey") |> plot()
```
