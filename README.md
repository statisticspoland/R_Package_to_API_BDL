# bdl 1.0.0

## Overview
The 'bdl' package is an interface to Local Data Bank(Bank Danych Lokalnych - bdl) 
[API](https://bdl.stat.gov.pl/BDL) with set of useful tools like quick plotting 
and generating maps using data from data bank. 

Information about how to use this tool is included in built in R help documentation.

More info about metadata can be found at:
* https://bdl.stat.gov.pl/BDL/start

## Installation
To install this package run these commands:

```
install.packages("remotes", type = "binary")
remotes::install_github("kaniakrzysztof/bdl", upgrade = "always", type = "binary")
```

If installation through remotes didnt work try to install package manually by 
downloading source package: https://github.com/statisticspoland/R_Package_to_API_BDL/releases/download/1.0.0/bdl_1.0.0.zip
