# Calibrate Biplot Axes

Convenience function to obtain the coordinates of the calibrated ticks
marks on the biplot

## Usage

``` r
axes_coordinates(x)
```

## Arguments

- x:

  an object of class `biplot`

## Value

An ordered list containing the coordinates the of tick marks to plotted
on the biplot

## Examples

``` r
x<-biplot(iris) |> PCA()
coordinates<-axes_coordinates(x)
```
