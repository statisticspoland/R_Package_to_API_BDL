# bdl 1.0.0

<!-- badges: start -->
[![Travis build status](https://travis-ci.org/statisticspoland/R_Package_to_API_BDL.svg?branch=master)](https://travis-ci.org/statisticspoland/R_Package_to_API_BDL)
[![CRAN status](https://www.r-pkg.org/badges/version/bdl)](https://CRAN.R-project.org/package=bdl)
<!-- badges: end -->

## Overview

The **bdl** package is an interface to Local Data Bank (Bank Danych Lokalnych - bdl) 
[API](https://api.stat.gov.pl/Home/BdlApi).
It is a set of tools that includes: quick plots and maps generation, with the usage of the datasets from the data bank. 

The information about how to use this tool is included in the built-in R help documentation and on the package webpage [https://statisticspoland.github.io/R_Package_to_API_BDL/](https://statisticspoland.github.io/R_Package_to_API_BDL/). 

Try

```r
help(p = "bdl")
```

More info about the metadata can be found at https://bdl.stat.gov.pl/BDL/start.

## Installation

To install this package run these commands:

**Windows and Mac**

```r
install.packages("remotes", type = "binary")
remotes::install_github("statisticspoland/R_Package_to_API_BDL", upgrade = "always", type = "binary")
```

**Linux**

```r
install.packages("remotes")
remotes::install_github("statisticspoland/R_Package_to_API_BDL")
```

To use the `generate_map()` function, you have to download and load map data to the global environment.
Map data can be downloaded here at [https://github.com/statisticspoland/R_Package_to_API_BDL/releases/download/1.0.0/bdl.maps.RData](https://github.com/statisticspoland/R_Package_to_API_BDL/releases/download/1.0.0/bdl.maps.RData)

If installation through **remotes** do not work, try installing the package manually by 
downloading the source package at [https://github.com/statisticspoland/R_Package_to_API_BDL/releases/download/1.0.0/bdl_1.0.0.zip](https://github.com/statisticspoland/R_Package_to_API_BDL/releases/download/1.0.0/bdl_1.0.0.zip) and executing

```r
install.packages("path_to_file/bdl_1.0.0.zip", repos = NULL, type = "source")
```
