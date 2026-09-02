# Construct additional dimensions when the dimension of the canonical space is smaller than the dimension of the biplot

This function is used to add dimensions to the CVA biplot when the
dimension of the canonical space \\K\\ is smaller than the dimension of
the biplot (`dim.biplot`). This function is already used in the CVA
calculations, and will therefore not have to be used in isolation.

## Usage

``` r
CVAlowdim(bp, G, W, Mmat, low.dim, K, e.vects)
```

## Arguments

- bp:

  an object of class `biplot`.

- G:

  the indicator matrix defining membership of the classes.

- W:

  the within class sums of squares and cross products matrix.

- Mmat:

  the eigenvector matrix from CVA.

- low.dim:

  a character string indicating which method to use to construct
  additional dimension(s) if the dimension of the canonical space is
  smaller than `dim.biplot`. One of "`sample.opt`" (default) for
  maximising the sample predictivity of the individual samples in the
  biplot or `Bhattacharyya.dist` which is based on the decomposition of
  the Bhattacharyya distance into a component for the sample means and a
  component for the dissimilarity between the sample covariance
  matrices.

- K:

  the dimension of the canonical space.

- e.vects:

  the vector indicating which canonical variates are plotted in the
  biplot, with default `1:dim.biplot`

## Value

A list with three components:

- Mr:

  the first r dimensions of the solution to be plotted.

- Mrr:

  the matrix used for prediction from the canonical space.

- Lmat:

  the matrix for transformation to the canonical space.
