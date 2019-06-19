#' @name bdl
#' 
#' 
#' @section Package options:
#' \describe{
#' \item{`bdl.api_private_key`}{String with BDL API key which you can get
#' \link{https://bdl.stat.gov.pl/api/v1/client?theme=Default} 
#' Example: \code{options(bdl.api_private_key = "3sd1dea1-f89d-488c-c441-08d64fbd20e9")}}
#' 
#' }
#' 
"_PACKAGE"
#' @docType package
#' @import methods
NULL

# global var note fix
if(getRversion() >= "2.15.1") utils::globalVariables(c("val"))
