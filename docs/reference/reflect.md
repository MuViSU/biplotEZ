# Reflect the biplot about a chosen axis

This function provides the user with an option to reflect the biplot
horizontally, vertically or diagonally.

## Usage

``` r
reflect(bp, reflect.axis = c("FALSE", "x", "y", "xy"))
```

## Arguments

- bp:

  an object of class `biplot`

- reflect.axis:

  a character string indicating which axis about to reflect. One of
  `FALSE` (default), "`x`" for reflection about the x-axis, "`y`" for
  reflection about the y-axis and "`xy`" for reflection about both axes.

## Value

An object of class `biplot`

## Examples

``` r
biplot(iris[,1:4],group.aes = iris[,5]) |> PCA() |> reflect("x") |> plot()

biplot(iris[,1:4],group.aes = iris[,5]) |> PCA() |> reflect("y") |> plot()

biplot(iris[,1:4],group.aes = iris[,5]) |> PCA() |> reflect("xy") |> plot()

```
