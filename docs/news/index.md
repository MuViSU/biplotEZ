# Changelog

## biplotEZ 3.0

(September 2026)

### New features

- `ggplot2` is now the default plotting engine.
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) gains an
  `engine` argument, which takes `"ggplot2"` or `"base"`.
- New [`gg_biplot()`](../reference/gg_biplot.md) builds a biplot as a
  `ggplot` object.
- New `autoplot()` method for `biplot` objects, plus
  [`print()`](https://rdrr.io/r/base/print.html),
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) and `+`
  methods for the resulting `gg_biplot` objects. The `+` method lets you
  add your own `ggplot2` layers.
- New [`MCA()`](../reference/MCA.md) function for multiple
  correspondence analysis biplots. The indicator matrix variant is
  available in this release.
- [`fit.measures()`](../reference/fit.measures.md) now reports overall
  quality for MCA biplots.
- New [`CatPCA()`](../reference/CatPCA.md) function for categorical PCA
  biplots, with [`nom.axes()`](../reference/nom.axes.md) and
  [`ord.axes()`](../reference/ord.axes.md) to format nominal and ordinal
  axes.
- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) gains an
  `add` argument, so a biplot can be drawn on top of an existing plot.
- [`axes()`](../reference/axes.md),
  [`newaxes()`](../reference/newaxes.md),
  [`nom.axes()`](../reference/nom.axes.md) and
  [`ord.axes()`](../reference/ord.axes.md) gain `label.dir = "Along"`,
  which places each axis title alongside its axis line, rotated to the
  slope of the axis. The default `label.dir` is now `NULL`, meaning
  `"Along"` with the `ggplot2` engine and `"Orthog"` with base graphics.
- The `biplot` object now stores `p2`, the number of categorical
  variables.
- New vignette section on MCA. The CA vignette is now called “CA and MCA
  in biplotEZ”.
- More examples added to the [`PCA()`](../reference/PCA.md) and
  [`CA()`](../reference/CA.md) documentation.

### Breaking changes

- `CATPCA()` has been renamed to [`CatPCA()`](../reference/CatPCA.md).
- `axes_coordinates()` is no longer exported.
- [`biplot()`](../reference/biplot.md) now stops with an error if a
  numeric variable has zero standard deviation.
- The default plotting engine changed from base graphics to `ggplot2`.
  Some features are not supported by the `ggplot2` engine yet and fall
  back to base graphics with a warning. These are zooming, 1D and 3D
  biplots, [`CatPCA()`](../reference/CatPCA.md) nominal and ordinal
  axes, and spline axes.
- MCA biplots are only drawn with the `ggplot2` engine.
- `legend.type(new = TRUE)` is a base graphics feature only. The
  `ggplot2` engine always places the legend next to the biplot.

### Bug fixes

- Fixed correlation PCA.
- Fixed correlation PCA for the case where p \> n.
- Fixed [`newsamples()`](../reference/newsamples.md) for CA biplots in
  base graphics.
- Fixed [`legend.type()`](../reference/legend.type.md) for the `ggplot2`
  engine.
- Fixed label aesthetics and label placement.
- `ggrepel` is no longer used in base graphics plotting.
- Fixed a typo in the CVA vignette.

### Internal changes

- The spline axis code was translated from Fortran to C++ using `Rcpp`.
  The call to [`optim()`](https://rdrr.io/r/stats/optim.html) was
  replaced by a Nelder-Mead routine written in C++.
- Unused Fortran helper functions were removed.
- Compiled build artefacts are no longer tracked in the repository.
- `Depends: R (>= 3.6.0)` added.
- `Rcpp` added to `Imports` and `LinkingTo`.
- `RColorBrewer` added to `Suggests`. `grid`, `R.devices` and `testthat`
  removed.

## biplotEZ 2.2

CRAN release: 2024-11-13

(November 2024)

## biplotEZ 2.1

CRAN release: 2024-09-14

(September 2024)

## biplotEZ 2.0

CRAN release: 2024-07-08

(July 2024)

## biplotEZ 1.2.0

CRAN release: 2023-11-28

(November 2023)

## biplotEZ 1.1.0

CRAN release: 2023-11-18

(November 2023)

## biplotEZ 1.0

CRAN release: 2023-08-31

Initial CRAN submission. (August 2023)
