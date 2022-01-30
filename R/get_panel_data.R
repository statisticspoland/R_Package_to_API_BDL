#' @title Get panel data by unit and variable Id's from BDL API
#' @description Retrieve data for given units from BDL with specified format.
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
#' @param unitId A single 12 character NUTS id code or vector of  multiple unit id 
#'   codes. If multiple unit codes are used, some columns are not available.
#'   Use \code{\link{search_units}} or \code{\link{get_units}} to find unit id code.
#' @param varId A single Id or vector of variable Id's.Use \code{\link{search_variables}} or
#'   \code{\link{get_variables}} to find variable id code.
#' @param year A vector of years. If \code{NULL} (default) returns data for all available years.
#' @param ggplot  Output in a long format suitable for ggplot2. Allows to plot results directly with ggplot function.
#' @param ... Other arguments passed on to \code{\link[httr]{GET}}. For example
#'   a proxy parameters, see details.
#'
#' @return A dataset as a tibble.
#' @export
#' @examples
#'    # get_panel_data(unitId = "030210101000", varId =  "60270")
#'    
#'    # get_panel_data(unitId = "030210101000", varId =  c("60270", "461668"))
#'    
#'    # get_panel_data(unitId = c("030210101000", "030210105000", "030210106000"), 
#'    #                varId =  c("60270", "461668"), year = c(2013:2016))
#'                           
#'    # get_panel_data(unitId = c("030210101000", "030210105000", "030210106000"), 
#'    #                varId =  c("60270", "461668"), ggplot = TRUE)
#' @keywords utilities database
get_panel_data <- function(unitId, varId, year = NULL, ggplot = FALSE, ...) {
  
  if (any(nchar_length(unitId) != 12)) {
    stop("Unit id should be length of 12.")
  }
  if(any(is.na(varId)) || any(nchar_length(varId) == 0)){
    stop("Variable id should be non-zero length string.")
  }

  dir <- "data/By-Unit"

    unitId <- as.list(unitId)
    varId <- as.list(varId)
    
    helper <- function(x) {
      temp <- get_data_by_unit(x, varId = varId, year = year)
      colname <- paste0("", x, sep = "")
      #names(temp)[names(temp) == "val"] <- colname
      temp <- dplyr::select(temp,-dplyr::one_of(c("attrId", "measureUnitId", "lastUpdate")))
      temp <- tidyr::pivot_wider(temp, names_from = "id", values_from = "val")
      temp <- cbind(colname, temp)
      colnames(temp)[1] <- "unit"
      temp
    }
    
    df <- lapply(unitId, helper)
    
    df <- purrr::reduce(df, rbind)
    
    if (ggplot == TRUE) {
      tdf <- dplyr::select(df,-dplyr::one_of(c("unit", "year", "attributeDescription")))
      variable_col_names <- colnames(tdf)
      
      
      df <- tidyr::pivot_longer(df, cols = variable_col_names, names_to = "variables", values_to = "values")
      df$year <- paste0(df$year, "-01-01")
      df$year <- as.Date(df$year)
    }
    
  df <- tibble::as_tibble(df)
  class(df) <- c("bdl", class(df))
  df
}
