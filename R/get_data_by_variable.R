#' @title Get data by variable Id from BDL API
#' @description Retrieve data for a given variable for multiple units
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
#' @param varId A single variable Id or vector of multilpe variable id's. If multiple id's are used, some columns
#'   are not available. Use \code{\link{search_variables}} or \code{\link{get_variables}} to find variable id code.
#' @param unitParentId A 12 character NUTS id code of parent unit. Use \code{\link{search_units}} or
#'   \code{\link{get_units}} to find unit id code. If \code{NULL} (default) and \code{unitLevel} not
#'   set up, returns all available units for variable.
#' @param unitLevel A number from 0 to 6, filters the returned unit by its level.
#'    If \code{NULL} (default) no level filters apply. Use \code{\link{get_levels}} to find more info.
#' @param aggregateId An aggregate id. Use \code{\link{get_aggregates}} for more info.
#' @param year A vector of years. If \code{NULL} (default) returns data for all available years.
#' @param lang  A language of returned data, "pl" (default), "en"
#' @param ... Other arguments passed on to \code{\link[httr]{GET}}. For example
#'   a proxy parameters, see details.
#'
#' @return A dataset as a tibble.
#' @export
#' @examples
#'  \dontrun{
#'    df <- get_data_by_variable(varId = "3643", unitParentId = "010000000000")
#'    df <- get_data_by_variable("420", year = "2000", unitLevel = 6)
#'    
#'    # Multi variable download
#'    df <- get_data_by_variable(varId =c("3643","420"), unitParentId = "010000000000")
#' }
#' @keywords utilities database
get_data_by_variable <- function(varId, unitParentId = NULL, unitLevel = NULL,
                                year = NULL, aggregateId = NULL, lang = c("pl","en"), 
                                ...) {

  if(any(is.na(varId)) || any(nchar_length(varId) == 0)){
    stop("Variable id should be non-zero length string.")
  }
  
  if (!is.null(unitParentId) && nchar_length(unitParentId) != 12) {
    stop("Unit id should be 12 characters NUTS id code.")
  }

  dir <- "data/By-Variable"
  lang <- match.arg(lang)
  
  if (length(varId) ==  1) {
    filters <- list(year = year, "unit-Parent-Id" = unitParentId,
                    "unit-Level" = unitLevel, "aggregate-Id" = aggregateId, lang = lang)
    
    df <- page_download(dir, varId, filters, ...)
  } else {
    varId <- as.list(varId)
    
    helper <- function(x) {
      temp <- get_data_by_variable(x, unitParentId = unitParentId, unitLevel = unitLevel, 
                                   year = year, aggregateId = aggregateId, lang = lang)
      colname <- paste0("val_", x, sep = "")
      names(temp)[names(temp) == "val"] <- colname
      temp
    }
    
    df <- lapply(varId, helper)
    
    helper = function(x) dplyr::select(x,-dplyr::one_of(c("attrId")))
    df <- lapply(df, helper)
    

    df <- purrr::reduce(df, dplyr::left_join)
  }
  

  
  df <- tibble::as_tibble(df)
  class(df) <- c("bdl", class(df))
  df
}
