Calculate and Display Kaplan Meier Curves using ggplot2
=======================================================

Installation
------------

This version of the package is not on CRAN. It also requires the
development version of ggplot2. It can be installed with

    devtools::install_github("hadley/ggplot2")
    devtools::install_github("sachsmc/ggkm")

Basic usage
-----------

    library(ggkm)

    ## Loading required package: ggplot2

    ## Loading required package: survival

    ## Loading required package: scales

    ggplot(lung, aes(time = time, status = status)) + geom_km()

![](README_files/figure-markdown_strict/unnamed-chunk-1-1.png)<!-- -->

    ggplot(lung, aes(time = time, status = status)) + geom_km() + facet_wrap(~ sex)

![](README_files/figure-markdown_strict/unnamed-chunk-1-2.png)<!-- -->

    ggplot(lung, aes(time = time, status = status, color = factor(sex))) + geom_km()

![](README_files/figure-markdown_strict/unnamed-chunk-1-3.png)<!-- -->

    ggplot(lung, aes(time = time, status = status)) + geom_km() + geom_kmticks()

![](README_files/figure-markdown_strict/unnamed-chunk-1-4.png)<!-- -->

    ggplot(lung, aes(time = time, status = status)) + geom_km() + geom_kmticks(shape = 1)

![](README_files/figure-markdown_strict/unnamed-chunk-1-5.png)<!-- -->

    ggplot(lung, aes(time = time, status = status, color = factor(sex))) + geom_km() + geom_kmticks()

![](README_files/figure-markdown_strict/unnamed-chunk-1-6.png)<!-- -->

    ggplot(lung, aes(time = time, status = status)) + geom_km(trans = "cumhaz") + geom_kmticks(trans = "cumhaz") + ylab("Cumulative hazard")

![](README_files/figure-markdown_strict/unnamed-chunk-1-7.png)<!-- -->

Acknowledgements
================

This package would not be possible without the following:

-   [ggplot2](http://ggplot2.org/)

License
=======

The MIT License (MIT)

Copyright (c) 2015 Michael C Sachs

Permission is hereby granted, free of charge, to any person obtaining a
copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
