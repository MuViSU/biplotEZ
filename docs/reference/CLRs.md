# Format aesthetics for the category level regions

This function allows the user to format the aesthetics for the category
level points (CLRs).

## Usage

``` r
CLRs (bp,  which = 1, col = "black")
```

## Arguments

- bp:

  an object of class `biplot`.

- which:

  the column name or number for which the CLRs should be displayed, with
  default `1`. Only one variable can be selected at a time.

- col:

  the colours for the CLRs, with default
  `colorRampPalette(c("black","white"))`.

## Value

The object of class `biplot` will be appended with a list called
`CLP.aes` containing the following elements A list with the following
components is available:

- which:

  the variable number for which the CLRs are displayed.

- col:

  the colours of the CLRs.

## See also

[`biplot`](biplot.md), [`PCO`](PCO.md), [`AoD`](AoD.md)

## Examples

``` r
mtdf <- as.data.frame(mtcars)
mtdf$cyl <- factor(mtdf$cyl)
mtdf$vs <- factor(mtdf$vs)
mtdf$am <- factor(mtdf$am)
mtdf$gear <- factor(mtdf$gear)
mtdf$carb <- factor(mtdf$carb)
#biplot(mtdf[,-11], scaled = TRUE) |> PCO(group.aes = mtdf[,11]) |> 
#CLRs(which = 10, col = "coral") |> plot()
```
