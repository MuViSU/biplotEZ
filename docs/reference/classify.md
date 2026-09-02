# Classify samples into classes

Classify samples into classes

## Usage

``` r
classify(
  bp,
  classify.regions = TRUE,
  col = ez.col,
  opacity = 0.4,
  borders = FALSE
)
```

## Arguments

- bp:

  an object of class `biplot`

- classify.regions:

  a logical value indicating whether classifications regions should be
  shown in the biplot, with default `TRUE`.

- col:

  the colours of the classification regions

- opacity:

  the opacity levels of the classification regions

- borders:

  the border colours of the classification regions

## Value

A list object called `classify` appended to the object of class `biplot`
with the following elements:

- table:

  the confusion matrix resulting from the classification into classes.

- rate:

  the classification accuracy rate.

- classify.regions:

  a logical value indicating whether classification regions are shown in
  the biplot.

- aes:

  a list of chosen aesthetics for the colours, opacity levels and border
  colours of the classification regions.

## Examples

``` r
biplot(iris[,1:4],classes = iris[,5]) |> CVA() |> axes(col="black") |> 
  classify(col=c("red","blue","orange"),opacity=0.1) |> plot()
```
