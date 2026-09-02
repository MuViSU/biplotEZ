# Biplots in 3D

## PCA 3D biplot

Three-dimensional PCA biplots are obtained by specifying
`dim.biplot = 3` in the call to [`PCA()`](../reference/PCA.md). The
package `rgl` is required and on calling the
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) the biplot is
drawn in an `rgl` window. The plot can then be interactively rotated and
zoomed using the mouse buttons.

[`biplot`](../reference/biplot.md)`(``iris``)`` ``|>`` `[`PCA`](../reference/PCA.md)`(``group.aes ``=`` ``iris``$``Species``,dim.biplot ``=`` ``3``)`` ``|>`` `` `[`axes`](../reference/axes.md)`(``col``=``"black"``)`` ``|>`` `[`plot`](https://rdrr.io/r/graphics/plot.default.html)`(``)`` ``#> Warning: The ggplot2 engine does not yet support 3D biplots; falling back to`` ``#> base graphics.`

### Adding new samples

The process of adding new samples to the biplot, called interpolation
utilises the functions [`interpolate()`](../reference/interpolate.md)
and [`newsamples()`](../reference/newsamples.md). These functions work
in the same way as in the call to the two-dimensional biplot. The
function [`interpolate()`](../reference/interpolate.md) accepts the
argument `newdata` to specify a matrix or data frame containing the new
samples to be interpolated. The function
[`newsamples()`](../reference/newsamples.md) operates the same way as
[`samples()`](../reference/samples.md) in that the user can specify the
aesthetics of the interpolated samples.

[`biplot`](../reference/biplot.md)`(``iris``[``1``:``100``,``1``:``4``]``)``|>`` `[`PCA`](../reference/PCA.md)`(``dim.biplot ``=`` ``3``)`` ``|>`` `[`axes`](../reference/axes.md)`(``col``=``"black"``)`` ``|>`` `` `` `[`interpolate`](../reference/interpolate.md)`(``newdata ``=`` ``iris``[``101``:``150``,``1``:``4``]``)`` ``|>`` `[`newsamples`](../reference/newsamples.md)`(``col``=``"purple"``)`` ``|>`` `[`plot`](https://rdrr.io/r/graphics/plot.default.html)`(``)`` ``#> Warning: The ggplot2 engine does not yet support 3D biplots; falling back to`` ``#> base graphics.`

### Adding new variables

To interpolate new variables to the biplot, the function
[`interpolate()`](../reference/interpolate.md) and
[`newaxes()`](../reference/newaxes.md) are called. The function
[`interpolate()`](../reference/interpolate.md) accepts the argument
`newvariable` to specify a matrix or data frame of the same number of
rows in the data specified in [`biplot()`](../reference/biplot.md)
containing the new variables to be interpolated. The function
[`newaxes()`](../reference/newaxes.md) allows the user to specify the
aesthetics of the interpolated variables.

[`biplot`](../reference/biplot.md)`(``iris``[``,``1``:``3``]``)``|>`` `[`PCA`](../reference/PCA.md)`(``dim.biplot ``=`` ``3``)`` ``|>`` `[`axes`](../reference/axes.md)`(``col``=``"black"``)`` ``|>`` `` `` `[`interpolate`](../reference/interpolate.md)`(``newvariable ``=`` ``iris``[``,``4``]``)`` ``|>`` `` `` `[`newaxes`](../reference/newaxes.md)`(``col``=``"darkred"``,X.new.names ``=`` ``"Petal.Width"``)`` ``|>`` `[`plot`](https://rdrr.io/r/graphics/plot.default.html)`(``)`` ``#> Warning: The ggplot2 engine does not yet support 3D biplots; falling back to`` ``#> base graphics.`

### Predicting samples

To add the prediction of samples on the biplot, the
[`prediction()`](../reference/prediction.md) function is used. The
`predict.samples` argument takes in a vector indicating either the row
numbers of the samples to predict or set to TRUE indicating to predict
all samples. The argument `which` allows the user to select which
variable to predict the sample on. In the example below, samples 100 to
150 predictions are shown for variables 1 and 4. The aesthetics for the
display of the predictions are arguments in the
[`axes()`](../reference/axes.md) function: `predict.col` and
`predict.lwd`.

[`biplot`](../reference/biplot.md)`(``iris``)`` ``|>`` `[`PCA`](../reference/PCA.md)`(``group.aes ``=`` ``iris``$``Species``,dim.biplot ``=`` ``3``,show.class.means ``=`` ``TRUE``)`` ``|>`` `` `` `[`axes`](../reference/axes.md)`(``col``=``"black"``,predict.col ``=`` ``"orange"``)`` ``|>`` `` `` `[`prediction`](../reference/prediction.md)`(``predict.samples``=``100``:``150``,which ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``1``,``4``)``)`` ``|>`` `[`plot`](https://rdrr.io/r/graphics/plot.default.html)`(``)`` ``#> Warning: The ggplot2 engine does not yet support 3D biplots; falling back to`` ``#> base graphics.`

### Predicting group means

Similarly, to add the prediction of group means, the function
[`prediction()`](../reference/prediction.md) is used. The argument
`predict.means` takes in a vector specifying which group means to
predict. In the example below, only the first group means is predicted.
Important to note that the argument `show.class.means` must be set to
TRUE in the [`PCA()`](../reference/PCA.md) function.

[`biplot`](../reference/biplot.md)`(``iris``)`` ``|>`` `` `` `[`PCA`](../reference/PCA.md)`(``group.aes ``=`` ``iris``$``Species``,dim.biplot ``=`` ``3``,show.class.means ``=`` ``TRUE``)`` ``|>`` `` `` `[`axes`](../reference/axes.md)`(``col``=``"black"``,predict.col ``=`` ``"darkred"``)`` ``|>`` `` `` `[`prediction`](../reference/prediction.md)`(``predict.means ``=`` ``1``)`` ``|>`` `[`plot`](https://rdrr.io/r/graphics/plot.default.html)`(``)`` ``#> Warning: The ggplot2 engine does not yet support 3D biplots; falling back to`` ``#> base graphics.`

### Ellipses

Ellipses are added to a 3D biplot using the
[`ellipses()`](../reference/ellipses.md) function which works in the
same way as a 2D biplot.

[`biplot`](../reference/biplot.md)`(``iris``)`` ``|>`` `[`PCA`](../reference/PCA.md)`(``group.aes ``=`` ``iris``[``,``5``]``,dim.biplot ``=`` ``3``)`` ``|>`` `` `` `[`axes`](../reference/axes.md)`(``col``=``"black"``)`` ``|>`` `` `` `[`ellipses`](../reference/ellipses.md)`(``kappa ``=`` ``3``,opacity ``=`` ``0.5``)`` ``|>`` `[`plot`](https://rdrr.io/r/graphics/plot.default.html)`(``)`` ``#> Computing 3 -ellipse for setosa `` ``#> Computing 3 -ellipse for versicolor `` ``#> Computing 3 -ellipse for virginica`` ``#> Warning: The ggplot2 engine does not yet support 3D biplots; falling back to`` ``#> base graphics.`

## CVA 3d biplot

Similarly for a CVA 3D biplot, `dim.biplot = 3` is specified in the call
to [`CVA()`](../reference/CVA.md).

[`biplot`](../reference/biplot.md)`(``iris``)`` ``|>`` `[`CVA`](../reference/CVA.md)`(``classes ``=`` ``iris``$``Species``,dim.biplot ``=`` ``3``)`` ``|>`` `` `[`axes`](../reference/axes.md)`(``col``=``"black"``)`` ``|>`` `[`plot`](https://rdrr.io/r/graphics/plot.default.html)`(``)`` ``#> Warning in CVA.biplot(biplot(iris), classes = iris$Species, dim.biplot = 3):`` ``#> The dimension of the canonical space < dim.biplot sample.opt method used for`` ``#> additional dimension(s).`` ``#> Warning: The ggplot2 engine does not yet support 3D biplots; falling back to`` ``#> base graphics.`

## CA 3D biplot

As with [`PCA()`](../reference/PCA.md), three-dimensional CA biplots are
obtained by specifying `dim.biplot = 3` in the call to
[`CA()`](../reference/CA.md). Again, an interactive biplot will be drawn
in a separate `rgl` window.

Consider the `HairEyeColor` example again as discussed in
`CA in biplotEZ`:

[`biplot`](../reference/biplot.md)`(``HairEyeColor``[``,,``2``]``, center ``=`` ``FALSE``)`` ``|>`` `` `` `[`CA`](../reference/CA.md)`(``variant ``=`` ``"Symmetric"``, dim.biplot ``=`` ``3``)`` ``|>`` `` `[`samples`](../reference/samples.md)`(``col``=`[`c`](https://rdrr.io/r/base/c.html)`(``"darkred"``,``"forestgreen"``)``, pch``=`[`c`](https://rdrr.io/r/base/c.html)`(``15``,``17``)``, label.col``=``"black"``)`` ``|>`` `` `[`plot`](https://rdrr.io/r/graphics/plot.default.html)`(``)`` ``#> Warning: The ggplot2 engine does not yet support 3D biplots; falling back to`` ``#> base graphics.`

We also illustrate the three dimensional CA biplot of the `SA Crime`
example:

`SACrime`` ``<-`` `[`matrix`](https://rdrr.io/r/base/matrix.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``1235``,``432``,``1824``,``1322``,``573``,``588``,``624``,``169``,``629``,``34479``,``16833``,``46993``,``30606``,``13670``,`` `` ``16849``,``15861``,``9898``,``24915``,``2160``,``939``,``5257``,``4946``,``722``,``1271``,``881``,``775``,``1844``,``5946``,`` `` ``4418``,``15117``,``10258``,``5401``,``4273``,``4987``,``1956``,``10639``,``29508``,``15705``,``62703``,``37203``,`` `` ``11857``,``18855``,``14722``,``4924``,``42376``,``604``,``156``,``7466``,``3889``,``203``,``664``,``291``,``5``,``923``,``19875``,`` `` ``19885``,``57153``,``29410``,``11024``,``12202``,``10406``,``5431``,``32663``,``7086``,``4193``,``22152``,``9264``,``3760``,`` `` ``4752``,``3863``,``1337``,``8578``,``7929``,``4525``,``12348``,``24174``,``3198``,``1770``,``7004``,``2201``,``45985``,``764``,`` `` ``427``,``1501``,``1197``,``215``,``251``,``345``,``213``,``1850``,``3515``,``879``,``3674``,``4713``,``696``,``835``,``917``,``422``,``2836``,`` `` ``88``,``59``,``174``,``76``,``31``,``61``,``117``,``32``,``257``,``5499``,``2628``,``8073``,``6502``,``2816``,``2635``,``3017``,``1020``,``4000``,`` `` ``8939``,``4501``,``50970``,``24290``,``2447``,``5907``,``5528``,``1175``,``14555``)``,nrow``=``9``, ncol``=``14``)`` `[`dimnames`](https://rdrr.io/r/base/dimnames.html)`(``SACrime``)`` ``<-`` `[`list`](https://rdrr.io/r/base/list.html)`(`[`paste`](https://rdrr.io/r/base/paste.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``"ECpe"``, ``"FrSt"``, ``"Gaut"``, ``"KZN"``, ``"Limp"``, ``"Mpml"``, ``"NWst"``, ``"NCpe"``,`` `` ``"WCpe"``)``)``, `[`paste`](https://rdrr.io/r/base/paste.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``"Arsn"``, ``"AGBH"``, ``"AtMr"``, ``"BNRs"``, ``"BRs"``, ``"CrJk"``,`` `` ``"CmAs"``, ``"CmRb"``, ``"DrgR"``, ``"InAs"``, ``"Mrd"``, ``"PubV"``, `` `` ``"Rape"``, ``"RAC"`` ``)``)``)`` `[`names`](https://rdrr.io/r/base/names.html)`(`[`dimnames`](https://rdrr.io/r/base/dimnames.html)`(``SACrime``)``)``[[``1``]``]`` ``<-`` ``"Provinces"`` `[`names`](https://rdrr.io/r/base/names.html)`(`[`dimnames`](https://rdrr.io/r/base/dimnames.html)`(``SACrime``)``)``[[``2``]``]`` ``<-`` ``"Crimes"`

[`biplot`](../reference/biplot.md)`(``SACrime``, center ``=`` ``FALSE``)`` ``|>`` `[`CA`](../reference/CA.md)`(``variant ``=`` ``"Symmetric"``, dim.biplot ``=`` ``3``)`` ``|>`` `` `[`samples`](../reference/samples.md)`(``col``=`[`c`](https://rdrr.io/r/base/c.html)`(``"royalblue"``,``"darkred"``)``, pch``=`[`c`](https://rdrr.io/r/base/c.html)`(``15``,``17``)``, label.col``=``"black"``)`` ``|>`` `[`plot`](https://rdrr.io/r/graphics/plot.default.html)`(``)`` ``#> Warning: The ggplot2 engine does not yet support 3D biplots; falling back to`` ``#> base graphics.`
