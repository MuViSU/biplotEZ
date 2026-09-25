# biplotEZ 3.0

(September 2026)

## New features

* `ggplot2` is now the default plotting engine. `plot()` gains an `engine` argument, which takes `"ggplot2"` or `"base"`.
* New `gg_biplot()` builds a biplot as a `ggplot` object.
* New `autoplot()` method for `biplot` objects, plus `print()`, `plot()` and `+` methods for the resulting `gg_biplot` objects. The `+` method lets you add your own `ggplot2` layers.
* New `MCA()` function for multiple correspondence analysis biplots. The indicator matrix variant is available in this release.
* `fit.measures()` now reports overall quality for MCA biplots.
* New `CatPCA()` function for categorical PCA biplots, with `nom.axes()` and `ord.axes()` to format nominal and ordinal axes.
* `plot()` gains an `add` argument, so a biplot can be drawn on top of an existing plot.
* `axes()`, `newaxes()`, `nom.axes()` and `ord.axes()` gain `label.dir = "Along"`, which places each axis title alongside its axis line, rotated to the slope of the axis. The default `label.dir` is now `NULL`, meaning `"Along"` with the `ggplot2` engine and `"Orthog"` with base graphics.
* The `biplot` object now stores `p2`, the number of categorical variables.
* New vignette section on MCA. The CA vignette is now called "CA and MCA in biplotEZ".
* More examples added to the `PCA()` and `CA()` documentation.

## Breaking changes

* `CATPCA()` has been renamed to `CatPCA()`.
* `axes_coordinates()` is no longer exported.
* `biplot()` now stops with an error if a numeric variable has zero standard deviation.
* The default plotting engine changed from base graphics to `ggplot2`. Some features are not supported by the `ggplot2` engine yet and fall back to base graphics with a warning. These are zooming, 1D and 3D biplots, `CatPCA()` nominal and ordinal axes, and spline axes.
* MCA biplots are only drawn with the `ggplot2` engine.
* `legend.type(new = TRUE)` is a base graphics feature only. The `ggplot2` engine always places the legend next to the biplot.

## Bug fixes

* Fixed correlation PCA.
* Fixed correlation PCA for the case where p > n.
* Fixed `newsamples()` for CA biplots in base graphics.
* Fixed `legend.type()` for the `ggplot2` engine.
* Fixed label aesthetics and label placement.
* `ggrepel` is no longer used in base graphics plotting.
* Fixed a typo in the CVA vignette.

## Internal changes

* The spline axis code was translated from Fortran to C++ using `Rcpp`. The call to `optim()` was replaced by a Nelder-Mead routine written in C++.
* Unused Fortran helper functions were removed.
* Compiled build artefacts are no longer tracked in the repository.
* `Depends: R (>= 3.6.0)` added.
* `Rcpp` added to `Imports` and `LinkingTo`.
* `RColorBrewer` added to `Suggests`. `grid`, `R.devices` and `testthat` removed.

# biplotEZ 2.2

(November 2024)

## New features

* New `axes_coordinates()` and `CLRs()` (format aesthetics for category level regions).

## Bug fixes

* Fixed `which` in `samples()`.
* Fixed `AoD()` and spline axes.
* CA quality and interpolation corrected, with additional CA examples and an updated CA vignette.

## Internal changes

* `biplot.spline.axis()` and `plot3D()` are no longer exported.
* `print.CA()` S3 method removed.

# biplotEZ 2.1

(September 2024)

## New features

* New `CLPs()` (format aesthetics for category level points) and `biplot.spline.axis()` for nonlinear (spline) biplot axes, added as compiled Fortran code.

## Bug fixes

* Documentation and `which`/`exp.factor` fixes for `CA()`.

# biplotEZ 2.0

(July 2024)

## New features

* New `CA()` for correspondence analysis biplots.
* New `CATPCA()` for categorical PCA biplots.
* New `PCO()` for principal coordinate analysis biplots and `regress()` for regression biplots, plus `AoD()` for biplots based on the Analysis of Distance method.
* New `classification()`/`classify()` for classification regions, and `prediction()` for CVA and PCA biplots.
* New `density1D()` and `density2D()` for density clouds.
* New `newaxes()`, `translate_axes()`, `rotate()` and `reflect()` for constructing and transforming biplot axes.
* New `extended.matching.coefficient()` and `sqrtManhattan()` distance measures, and `plot3D()` for 3D biplots.

## Bug fixes

* Fixed alpha-bags and `group.aes`.
* Fixed a `ggrepel` error when group 1 is not plotted.
* Fixed an issue where an incorrect `which` eliminated axes.
* Partially fixed 1D density plots.
* Corrected two-group CVA for `fit.measures()`.

## Internal changes

* Compiled (Fortran) code introduced for the first time, registered via `useDynLib()`.
* Experimental `ggplot2`/`ggrepel`/`grid`-based plotting groundwork from 1.2.0 was removed; plotting remained base-graphics only.

# biplotEZ 1.2.0

(December 2023)

## New features

* New `newsamples()`, `interpolate()`, `means()`, `fit.measures()` and a `summary()` method for `biplot` objects.
* `concentration.ellipse()` renamed to `ellipses()`.
* Added `prcomp` and `princomp` support as input to `PCA()`.
* `samples()` gains a `which` argument and `label.col`.
* CVA and alpha-bags/ellipses vignette content completed.

# biplotEZ 1.1.0

(October 2023)

## Bug fixes

* Bug fixes following the initial v1.0 CRAN release.

## Internal changes

* Switched to using `inherits()` for class checks.

# biplotEZ 1.0

Initial CRAN submission. (August 2023)