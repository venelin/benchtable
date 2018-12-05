
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis build status](https://travis-ci.org/venelin/benchtable.svg?branch=master)](https://travis-ci.org/venelin/benchtable) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/benchtable?color=blue)](https://cran.r-project.org/package=benchtable)

benchtable
==========

This R-package executes a user-defined function on each line of a data.table splitting the job into multiple parallel R-processes which eventually are run on a cluster using the lsf (bsub) or qsub command.

Installation
------------

``` r
devtools::install.github('venelin/benchtable')
```

Example
-------

Please, read the [Get started guide](https://venelin.github.io/benchtable/articles/benchtable.html).
