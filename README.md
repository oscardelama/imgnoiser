<!-- README.md is generated from README.Rmd. Please edit that file -->
The :camera: `imgnoiser` R Package
==================================

[![Build Status](https://travis-ci.org/oscardelama/imgnoiser.png?branch=master)](https://travis-ci.org/oscardelama/imgnoiser)

The `imgnoiser` (IMaGe NOISE) R package contains tools for the measurement, analysis and characterization of raw image noise from digital CMOS sensor cameras. The focus of the package is automatize as possible most of the involved procedures so you can focus on the interpretation and analysis of its results.

Installation
------------

This package is not in CRAN yet. However, you can use the `devtools` package to install the development version from Github:

``` r
devtools::install_github('oscardelama/imgnoiser')
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

Documentation
-------------

Most software, including `imgnoiser` is developed in an iterative way: you decide to include some functionality or service in the product, you design the interface, develop the service, and during the tests you find some rough edges to polish and unhandled situations, so you iterate all over again until you are satisfied. Then you start to document the new functionality. In this context, most of the package documentation is complete and accurate, but recently added features are not complete or does not exists yet.

I have found an Issue
---------------------

The package is continuously tested, but the tests neither include every package feature nor all their possible usage combinations. Notice for example that a function with 4 parameters and an average of 3 substantially different possible values per parameter, requires 4^3 = 64 tests. If in addition to that, you take into account that this function output is the input of another similar function (4 parameters, 3 values per parameter), now we need 64\*64 = 4,096 tests just to be 100% sure this two functions work, individually and together, as expected. All of this explains the real software tester is the final user, and that is the reason for periodic software updates.

However, all of this do not dismiss a developer to make its best reasonable effort to ensure the quality of its product. To be honest, nowadays, the Travis CI badge above does not reflect the quality assurance I wish to reach when this product is more mature and stable in its features and interfaces.

In the iterative development effort, most of the package functionality is reasonable well tested but yet not stressed in real life intensive use; some recently added code is still in test phase.

If you have found an issue please [file it here](https://github.com/oscardelama/imgnoiser/issues).

License
-------

This package is free and open source software, licensed under GPL.
