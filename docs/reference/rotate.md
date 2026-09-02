# Rotate the biplot a chosen amount of degrees

This function provides the user with an option to rotate the biplot
anti-clockwise or clockwise.

## Usage

``` r
rotate(bp, rotate.degrees = 0)
```

## Arguments

- bp:

  an object of class `biplot`

- rotate.degrees:

  a value specifying the degrees the biplot should be rotated, with
  default `0`. A positive value results in anti-clockwise rotation and a
  negative value in clockwise rotation.

## Value

An object of class `biplot`.

## Examples

``` r
biplot(iris[,1:4],group.aes = iris[,5]) |> PCA() |> rotate(200) |> plot()

```
