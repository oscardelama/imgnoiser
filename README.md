<!-- README.md is generated from README.Rmd. Please edit that file -->
The :camera: `imgnoiser` R Package
==================================

<!-- Non tested state
[![Build Status](https://travis-ci.org/oscardelama/imgnoiser.png?branch=master)](https://travis-ci.org/oscardelama/imgnoiser)
-->
The `imgnoiser` (IMaGe NOISE) R package contains tools for the measurement, analysis and characterization of raw image noise from digital CMOS sensor cameras. The focus of the package is automatize as possible most of the involved procedures so you can focus on the interpretation and analysis of its results.

Installation
------------

This package is not in CRAN yet. However, you can use the `devtools` package to install the development version from Github:

``` r
devtools::install_github('oscardelama/imgnoiser', build_vignettes = FALSE))
```

Usage
-----

In [the wiki pages](https://github.com/oscardelama/imgnoiser/wiki) you will find detailed documentation and examples of the general workflow using `imgnoiser`.

------------------------------------------------------------------------

-   [The imgnoiser R Package](https://github.com/oscardelama/imgnoiser/wiki)
-   [Introduction to the `vvm` Class](https://github.com/oscardelama/imgnoiser/wiki/vvm-Class-Introduction)
-   [Introduction to the `hvdvm` Class](https://github.com/oscardelama/imgnoiser/wiki/hvdvm-Class-Introduction)
-   [`hvdvm` and `vvm` Class Usage](https://github.com/oscardelama/imgnoiser/wiki/hvdvm-and-vvm-Class-Usage)
-   [Collecting Image Samples](https://github.com/oscardelama/imgnoiser/wiki/Collecting-Image-Samples)

------------------------------------------------------------------------

These wiki pages are based on the package vignettes. If you install the package, you will be able to access this documentation from your R session using:

``` r
browseVignettes('imgnoiser')
```

License
-------

This package is free and open source software, licensed under GPL.
