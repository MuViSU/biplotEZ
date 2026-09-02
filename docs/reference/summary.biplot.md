# Generic summary function for objects of class biplot

This function is used to print summary output of the biplot. These
summary outputs are related to measures of fit.

## Usage

``` r
# S3 method for class 'biplot'
summary(
  object,
  adequacy = TRUE,
  axis.predictivity = TRUE,
  sample.predictivity = TRUE,
  class.predictivity = TRUE,
  within.class.axis.predictivity = TRUE,
  within.class.sample.predictivity = TRUE,
  ...
)
```

## Arguments

- object:

  an object of class `biplot`.

- adequacy:

  a logical value indicating whether variable adequacies should be
  reported, with default `TRUE`.

- axis.predictivity:

  a logical value indicating whether axis predictivities should be
  reported, with default `TRUE`.

- sample.predictivity:

  a logical value indicating whether sample predictivities should be
  reported, with default `TRUE`.

- class.predictivity:

  a logical value indicating whether class predictivities should be
  reported, with default `TRUE` (only applicable to objects of class
  `CVA`).

- within.class.axis.predictivity:

  a logical value indicating whether within class axis predictivity
  should be reported, with default `TRUE` (only applicable to objects of
  class `CVA`).

- within.class.sample.predictivity:

  a logical value indicating whether within class sample predictivity
  should be reported, with default `TRUE` (only applicable to objects of
  class `CVA`).

- ...:

  additional arguments.

## Value

This function will not produce a return value, it is called for side
effects.

## Examples

``` r
out <- biplot (iris[,1:4]) |> PCA() |> fit.measures()
summary(out)
#> Object of class biplot, based on 150 samples and 4 variables.
#> 4 numeric variables.
#> 
#> Quality of fit in 2 dimension(s) = 97.8% 
#> Adequacy of variables in 2 dimension(s):
#> Sepal.Length  Sepal.Width Petal.Length  Petal.Width 
#>    0.5617091    0.5402798    0.7639426    0.1340685 
#> Axis predictivity in 2 dimension(s):
#> Sepal.Length  Sepal.Width Petal.Length  Petal.Width 
#>    0.9579017    0.8400028    0.9980931    0.9365937 
#> Sample predictivity in 2 dimension(s):
#>         1         2         3         4         5         6         7         8 
#> 0.9998927 0.9927400 0.9999141 0.9991226 0.9984312 0.9949770 0.9914313 0.9996346 
#>         9        10        11        12        13        14        15        16 
#> 0.9998677 0.9941340 0.9991205 0.9949153 0.9945491 0.9996034 0.9942676 0.9897890 
#>        17        18        19        20        21        22        23        24 
#> 0.9937752 0.9990534 0.9972926 0.9928624 0.9896250 0.9932656 0.9918132 0.9955885 
#>        25        26        27        28        29        30        31        32 
#> 0.9812917 0.9897303 0.9979903 0.9990514 0.9963870 0.9975607 0.9985741 0.9876345 
#>        33        34        35        36        37        38        39        40 
#> 0.9833383 0.9957412 0.9970200 0.9935405 0.9859750 0.9953399 0.9994047 0.9990244 
#>        41        42        43        44        45        46        47        48 
#> 0.9980903 0.9756895 0.9953372 0.9830035 0.9763861 0.9959863 0.9905695 0.9987006 
#>        49        50        51        52        53        54        55        56 
#> 0.9996383 0.9987482 0.9275369 0.9996655 0.9544488 0.9460515 0.9172857 0.9061058 
#>        57        58        59        60        61        62        63        64 
#> 0.9727694 0.9996996 0.8677939 0.8686502 0.9613130 0.9328852 0.4345132 0.9679973 
#>        65        66        67        68        69        70        71        72 
#> 0.7995848 0.9083037 0.7968614 0.5835260 0.7900027 0.8575646 0.8524748 0.6615410 
#>        73        74        75        76        77        78        79        80 
#> 0.9367709 0.8661203 0.8350955 0.8929908 0.8702600 0.9873164 0.9969031 0.6815512 
#>        81        82        83        84        85        86        87        88 
#> 0.8937189 0.8409681 0.7829405 0.9848354 0.6901625 0.8073582 0.9666041 0.6665514 
#>        89        90        91        92        93        94        95        96 
#> 0.6993846 0.9909923 0.9008345 0.9710941 0.8037223 0.9913632 0.9744493 0.7089660 
#>        97        98        99       100       101       102       103       104 
#> 0.9071738 0.9064541 0.9625371 0.9872279 0.9171603 0.9636413 0.9976224 0.9829885 
#>       105       106       107       108       109       110       111       112 
#> 0.9854704 0.9888092 0.8464463 0.9729353 0.9771293 0.9794313 0.9746239 0.9977302 
#>       113       114       115       116       117       118       119       120 
#> 0.9941859 0.9605563 0.8476794 0.9289985 0.9929982 0.9916850 0.9818957 0.9493751 
#>       121       122       123       124       125       126       127       128 
#> 0.9865358 0.8716778 0.9728177 0.9846364 0.9840890 0.9861783 0.9854516 0.9691512 
#>       129       130       131       132       133       134       135       136 
#> 0.9942007 0.9585884 0.9705389 0.9937852 0.9874192 0.9723192 0.9230503 0.9794405 
#>       137       138       139       140       141       142       143       144 
#> 0.8947527 0.9797055 0.9458421 0.9902488 0.9674660 0.9350646 0.9636413 0.9867931 
#>       145       146       147       148       149       150 
#> 0.9500265 0.9470544 0.9688318 0.9886543 0.8735433 0.9281727 
```
