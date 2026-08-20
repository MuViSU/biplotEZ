# Generic print function for objects of class biplot

This function is used to print output when the biplot object is created.

## Usage

``` r
# S3 method for class 'biplot'
print(x, ...)
```

## Arguments

- x:

  an object of class `biplot`.

- ...:

  additional arguments.

## Value

This function will not produce a return value, it is called for side
effects.

## Examples

``` r
out <- biplot (iris[,1:4]) |> PCA()
out
#> Object of class biplot, based on 150 samples and 4 variables.
#> 4 numeric variables.
```
