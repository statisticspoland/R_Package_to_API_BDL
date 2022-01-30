Sys.setenv("R_TESTS" = "")
library(testthat)
library(bdl)

test_check("bdl")
