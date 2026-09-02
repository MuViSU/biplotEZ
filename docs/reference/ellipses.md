# Concentration ellipses (\\\kappa\\-ellipses)

This function produces \\\kappa\\-ellipses, which is a useful
geometrical description of the data points about the sample mean.

## Usage

``` r
ellipses(bp, df=2, kappa = NULL, which = NULL,
alpha = 0.95, col = bp$sample$col[which], lty = 1, lwd = 1,
opacity = 0.25, trace = TRUE)
```

## Arguments

- bp:

  an object of class `biplot`.

- df:

  degrees of freedom, with default `2`.

- kappa:

  value to construct \\\kappa\\-ellipse (the value of \\\kappa\\).

- which:

  the selection of the group for ellipse construction.

- alpha:

  size of \\\alpha\\-bag, with default `0.95`.

- col:

  colour of ellipse. Multiple \\\kappa\\-ellipse for one group will be
  displayed in the same colour.

- lty:

  line type of ellipse. The same line type will be used per value of
  \\\kappa\\.

- lwd:

  line width of ellipse. The same line width will be used per value of
  \\\kappa\\.

- opacity:

  level of opacity, with default `0.25`.

- trace:

  logical, indicating progress of computation.

## Value

A list with the following components is available:

- conc.ellipses:

  list of coordinates for the \\\kappa\\-ellipses for each group.

- col:

  vector of colours for the \\\kappa\\-ellipses.

- lty:

  vector of line types for the \\\kappa\\-ellipses.

- lwd:

  vector of line widths for the \\\kappa\\-ellipses.

- alpha:

  vector of \\\alpha\\ values.

## References

Gower, J., Gardner-Lubbe, S. & Le Roux, N. (2011, ISBN:
978-0-470-01255-0) *Understanding Biplots.* Chichester, England: John
Wiley & Sons Ltd.\
\

## Examples

``` r
biplot (iris[,1:4]) |> PCA(group.aes=iris[,5]) |> ellipses(kappa=2) |> plot()
#> Computing 2 -ellipse for setosa 
#> Computing 2 -ellipse for versicolor 
#> Computing 2 -ellipse for virginica 

```
