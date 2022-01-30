#' @name bdl
#' 
#' 
#' @section Package options:
#' \describe{
#' \item{`bdl.api_private_key`}{String with BDL API key which you can get at
#' \url{https://api.stat.gov.pl/Home/BdlApi?lang=en} 
#' Example: \code{options(bdl.api_private_key = "11111111-2222-3333-4444-555555555555")}}
#' 
#' }
#' 
"_PACKAGE"
#' @docType package
#' @import methods
#' @import utils
#' @importFrom  stats sd
#' @importFrom  stats var
#' @import dplyr
#' @importFrom purrr reduce
#' @import tibble
#' @importFrom jsonlite fromJSON
#' @importFrom magrittr %>%
#' @import httr
#' @importFrom tidyr unnest
#' @importFrom tidyr drop_na
#' @import ggplot2
#' @import ggpubr
#' @importFrom randomcoloR distinctColorPalette
#' @import tmap
#' @import tmaptools
#' @import sf
NULL
# global var note fix
if(getRversion() >= "2.15.1") utils::globalVariables(c("val","bdl.maps","values"))
