#' @title Get data by variable Id for localities from BDL API
#' @description Retrieve data for a given variables for multiple unit localities
#'  from BDL with specified format.
#' @details
#'   Data to retrieve from
#'   \href{https://bdl.stat.gov.pl}{The
#'   BDL Web Services} can be filtered with arguments. To get JSON data from specified
#'   directory with custom filters use \code{\link{get_request}} directly.
#'
#'   To use a proxy to connect, a \code{\link[httr]{use_proxy}} can be
#'   passed to \code{\link[httr]{GET}}. For example
#'   \code{get_request(id, filters,
#'   config = httr::use_proxy(url, port, username, password))}.
#'
#' @param varId A single variable id or vector of multiple variable id's. If multiple id's are used, some columns
#'   are not available.. Use \code{\link{search_variables}} or \code{\link{get_variables}} to find variable id code.
#' @param unitParentId A 12 character NUTS id code of interested unit. Use \code{\link{search_units}} or
#'   \code{\link{get_units}} to find unit id code. If \code{NULL} (default) and \code{unitLevel} not
#'   set up, returns all available units for variable.
#' @param year A vector of years. If \code{NULL} (default) returns data for all available years.
#' @param lang  A language of returned data, "pl" (default), "en"
#' @param ... Other arguments passed on to \code{\link[httr]{GET}}. For example
#'   a proxy parameters, see details.
#'
#' @return A dataset as a tibble.
#' @export
#' @examples
#'    # get_data_by_variable_locality(varId = "415", unitParentId = "011212006063")
#'    # get_data_by_variable_locality("420", year = "2008", unitParentId = "070000000000")
#'    
#'    # Multi variable download
#'    # get_data_by_variable_locality(varId =c("415","430"), unitParentId = "011212006063")
#' @keywords utilities database
get_data_by_variable_locality <- function(varId, unitParentId, year = NULL,
                                          lang = c("pl","en"), ...) {
  
  if(length(unitParentId) > 1 || any(nchar_length(unitParentId) != 12)) {
    stop("Unit id should be 12 characters NUTS id code.")
  }
  if(any(is.na(varId)) || any(nchar_length(varId) == 0)){
    stop("Variable id should be non-zero length string.")
  }
  
  dir <- "data/Localities/By-Variable"
  lang <- match.arg(lang)
  
  
  if (length(varId) ==  1) {
    filters <- list(year = year, "unit-Parent-Id" = unitParentId, lang = lang)
    
    df <- page_download(dir, varId, filters, ...)
    df <- add_attribute_labels(df, lang)
    df <- add_measure_columns(varId, df, lang)
    
  } else {
    varId <- as.list(varId)
    
    helper <- function(x) {
      temp <- try(get_data_by_variable_locality(x, unitParentId = unitParentId, year = year, lang = lang), silent = T)
      if(is.error(temp)){
        warning(paste("Filters returned empty data for variable", x, "and it will be skipped."), call. = F)
        return(NULL)
      }
      
      temp <- add_attribute_labels(temp, lang)
      
      colname <- paste0("attrId_", x, sep = "")
      names(temp)[names(temp) == "attrId"] <- colname
      
      colname <- paste0("attributeDescription_", x, sep = "")
      names(temp)[names(temp) == "attributeDescription"] <- colname
      
      colname <- paste0("measureUnitId_", x, sep = "")
      names(temp)[names(temp) == "measureUnitId"] <- colname
      
      colname <- paste0("measureName_", x, sep = "")
      names(temp)[names(temp) == "measureName"] <- colname

      colname <- paste0("val_", x, sep = "")
      names(temp)[names(temp) == "val"] <- colname
      temp
    }
    
    df <- lapply(varId, helper)
    df <- df[lengths(df) != 0]
    if(length(df) > 0){
      df <- purrr::reduce(df, dplyr::left_join) 
    } else {
      stop("Filters returned empty set for every variable you provided.")
    }
    
  }
  
  df <- df %>%
    select(one_of("id", "name", "year"), starts_with("val"),starts_with("measure"), everything())
  
  df <- tibble::as_tibble(df)
  class(df) <- c("bdl", class(df))
  
  df
}
