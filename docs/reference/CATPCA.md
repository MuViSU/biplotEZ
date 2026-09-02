# Perform Categorical Principal Components Analysis (CatPCA)

This function appends the `biplot` object with elements resulting from
performing CatPCA.

## Usage

``` r
CatPCA(bp, cat.type = NULL, dim.biplot = c(2, 1, 3), e.vects = 1:ncol(bp$X),
group.aes = NULL, show.class.means = FALSE, epsilon = 1e-6)
```

## Arguments

- bp:

  an object of class `biplot` obtained from preceding function
  [`biplot()`](biplot.md).

- cat.type:

  a vector indicating whether each categorical variables is `nominal` or
  `ordinal`.

- dim.biplot:

  the dimension of the biplot. Only values `1`, `2` and `3` are
  accepted, with default `2`.

- e.vects:

  the vector indicating which eigenvectors (principal components) should
  be plotted in the biplot, with default `1:dim.biplot`.

- group.aes:

  a vector of the same length as the number of rows in the data matrix
  for differentiated aesthetics for samples.

- show.class.means:

  a logical value indicating whether group means should be plotted in
  the biplot.

- epsilon:

  convergence criteria for iterative algorithm with defaults 1e-6.

## Value

An object of class CatPCA with the following elements:

- X:

  the matrix of the centered and scaled numeric variables.

- Xcat:

  the data frame of the categorical variables.

- raw.X:

  the original data.

- classes:

  the vector of category levels for the class variable. This is to be
  used for `colour`, `pch` and `cex` specifications.

- na.action:

  the vector of observations that have been removed.

- center:

  a logical value indicating whether \\\mathbf{X}\\ is centered.

- scaled:

  a logical value indicating whether \\\mathbf{X}\\ is scaled.

- means:

  the vector of means for each numerical variable.

- sd:

  the vector of standard deviations for each numerical variable.

- n:

  the number of observations.

- p:

  the number of variables.

- group.aes:

  the vector of category levels for the grouping variable. This is to be
  used for `colour`, `pch` and `cex` specification.

- g.names:

  the descriptive names to be used for group labels.

- g:

  the number of groups.

- Title:

  the title of the biplot rendered.

- Z:

  the matrix with each row containing the details of the points that are
  plotted (i.e. coordinates).

- Lmat:

  the matrix for transformation to the principal components.

- Linv:

  the inverse of \\\mathbf{L}\\.

- eigenvalues:

  the vector of eigenvalues of the covariance matrix of \\\mathbf{X}\\.

- all.ax.one.unit:

  one unit in the positive direction of each biplot axis.

- ax.type:

  one of numeric, ordinal or nominal.

- ax.one.unit:

  one unit in the positive direction of each numeric biplot axis.

- nom.levels:

  a list with the levels of each nominal variable

- ord.levels:

  a list with the levels of each ordinal variable

- e.vects:

  the vector indicating which principal components are plotted in the
  biplot.

- dim.biplot:

  the dimension of the biplot.

- class.means:

  a logical value indicating whether group means are plotted in the
  biplot.

- Zmeans:

  the matrix of class mean coordinates that are plotted in the biplot.

## Examples

``` r
biplot(iris) |> CatPCA()
#> Object of class biplot, based on 150 samples and 5 variables.
#> 4 numeric variables.
#> 1 categorical variable.
# create a CatPCA biplot
biplot(data = iris) |> CatPCA() |> plot()
#> Warning: The ggplot2 engine does not yet support catPCA nominal/ordinal axes; falling back to base graphics.

#> Error in scale.default(Xhat, center = -1 * x$means, scale = FALSE): length of 'center' must equal the number of columns of 'x'
```
