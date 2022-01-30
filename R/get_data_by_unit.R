#' @title Get data by unit Id's from BDL API
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
#' @param varId A vector of variable Id's.Use \code{\link{search_variables}} or
#'   \code{\link{get_variables}} to find variable id code.
#' @param year A vector of years. If \code{NULL} (default) returns data for all available years.
#' @param type A type of variables returned, "code" (default), "label"
#' @param aggregateId An aggregate id. Use \code{\link{get_aggregates}} for more info.
#' @param lang  A language of returned data, "pl" (default), "en"
#' @param ... Other arguments passed on to \code{\link[httr]{GET}}. For example
#'   a proxy parameters, see details.
#'
#' @return A dataset as a tibble.
#' @export
#' @examples
#'    # get_data_by_unit(unitId = "023200000000", varId =  "3643")
#'    # get_data_by_unit(unitId = "023200000000", varId =  c("3643", "2137", "148190"), 
#'    #                  type = "label")
#'    
#'    # Multi variable download
#'    # get_data_by_unit(unitId = c("023200000000", "020800000000"), 
#'    #                  varId =  c("3643", "2137", "148190"))
#' @keywords utilities database
get_data_by_unit <- function(unitId, varId, year = NULL,
  type = c("code", "label"), aggregateId = NULL, lang = c("pl","en"), ...) {
  
  if (any(nchar_length(unitId) != 12)) {
    stop("Unit id should be length of 12.")
  }
  if(any(is.na(varId)) || any(nchar_length(varId) == 0)){
    stop("Variable id should be non-zero length string.")
  }

  dir <- "data/By-Unit"
  type <- match.arg(type)
  lang <- match.arg(lang)
  
  
  if (length(unitId) ==  1) {
    
    filters <- list(year = year, "var-Id" = varId, "aggregate-Id" = aggregateId, lang = lang)
    df <- page_download(dir, unitId, filters, ...)
    df <- add_attribute_labels(df, lang)
    
  } else {
    unitId <- as.list(unitId)
    
    helper <- function(x) {
      temp <- try(get_data_by_unit(x, varId = varId, aggregateId = aggregateId, year = year, lang = lang), silent = T)
      if(is.error(temp)){
        warning(paste("Filters returned empty data for unit", x, "and it will be skipped."), call. = F)
        return(NULL)
      }
      
      colname <- paste0("val_", x, sep = "")
      names(temp)[names(temp) == "val"] <- colname
      
      temp <- add_attribute_labels(temp, lang)

      colname <- paste0("lastUpdate_", x, sep = "")
      names(temp)[names(temp) == "lastUpdate"] <- colname
      
      colname <- paste0("attrId_", x, sep = "")
      names(temp)[names(temp) == "attrId"] <- colname

      colname <- paste0("attributeDescription_", x, sep = "")
      names(temp)[names(temp) == "attributeDescription"] <- colname

      temp
    }
    
    df <- lapply(unitId, helper)
    df <- df[lengths(df) != 0]
    if(length(df) > 0){
      df <- purrr::reduce(df, dplyr::left_join)
    } else {
      stop("Filters returned empty set for every unit you provided.")
    }
    
  }

  df <- df %>% select(one_of("id", "year"), starts_with("val"), 
                      starts_with("measure"), starts_with("attr"),everything())
  
  
  if (type == "label") {
    variables <- unique(df$id)

    variable_labels <- lapply(variables, get_var_label, lang = lang)
    names(variable_labels) <- variables

    measure_labels <- lapply(variables, get_measure_label, lang = lang)
    names(measure_labels) <- variables

    df <- df %>%
      dplyr::mutate(variableName = as.character(variable_labels[as.character(df$id)])) %>%
      dplyr::mutate(measureName = as.character(measure_labels[as.character(df$id)])) %>%
      select(one_of("id", "year"), starts_with("val"), "variableName", starts_with("measure"), starts_with("attr"),everything())
  }

  df$id <- as.character(df$id)
  df <- tibble::as_tibble(df)
  class(df) <- c("bdl", class(df))
  df
}
