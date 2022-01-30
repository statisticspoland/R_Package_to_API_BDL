#' @title Get data by unit locality Id from BDL API
#' @description Retrieve data for a given unit localities from BDL with specified format.
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
#' @param unitId A 12 character NUTS unit locality id with 7 characters locality individual id, 
#'   separated by dash or vector of  multiple unit id codes. If multiple unit codes are used, 
#'   some columns are not available. Use \code{\link{search_unit_localities}} 
#'   or \code{\link{get_unit_localities}} to find unit id code.
#' @param varId A vector of variable Id's.Use \code{\link{search_variables}} or
#'   \code{\link{get_variables}} to find variable id code.
#' @param year A vector of years. If \code{NULL} (default) returns data for all available years.
#' @param type  A type of variables returned, "code" (default), "label"
#' @param lang  A language of returned data, "pl" (default), "en"
#' @param ... Other arguments passed on to \code{\link[httr]{GET}}. For example
#'   a proxy parameters, see details.
#'
#' @return A dataset as a tibble.
#' @export
#' @examples
#'    # get_data_by_unit_locality(unitId = "030210106062-0189782", varId =  "415", type = "label")
#'    
#'    # Multi variable download
#'    # get_data_by_unit_locality(unitId = c("030210106062-0189782", "030210106062-0189753"), 
#'    #                           varId = "415")
#' @keywords utilities database
get_data_by_unit_locality <- function(unitId, varId, year = NULL,
                             type = c("code", "label"), lang = c("pl","en"), ...) {

 if (any(nchar_length(unitId) != 20) || any(substr(unitId, 13, 13) != "-")) {
    stop("Unit id should be 12 characters NUTS id code with 7 characters locality individual id, separated by dash.")
 }
   if(any(is.na(varId)) || any(nchar_length(varId) == 0)){
      stop("Variable id should be non-zero length string.")
   }
 dir <- "data/Localities/By-Unit"
 type <- match.arg(type)
 lang <- match.arg(lang)
 # filters <- list(year = year, "var-Id" = varId)
 # 
 # df <- page_download(dir, unitId, filters, ...)
 # 
 # if (type == "label") {
 #   variables <- unique(df$id)
 # 
 #   variables_labels <- lapply(variables, get_var_label, lang = lang)
 #   names(variables_labels) <- variables
 # 
 #   measures_labels <- lapply(variables, get_measure_label, lang = lang)
 #   names(measures_labels) <- variables
 # 
 #   df <- df %>%
 #     dplyr::mutate(variableName = as.character(variables_labels[as.character(df$id)])) %>%
 #     dplyr::mutate(measureName = as.character(measures_labels[as.character(df$id)]))
 # }
 
 if (length(unitId) ==  1) {
   
   filters <- list(year = year, "var-Id" = varId, lang = lang)
   df <- page_download(dir, unitId, filters, ...)
   df <- add_attribute_labels(df, lang)
   
 } else {
   unitId <- as.list(unitId)
   
   helper <- function(x) {
      temp <- try(get_data_by_unit_locality(x, varId = varId, year = year, lang = lang), silent = T)
      if(is.error(temp)){
         warning(paste("Filters returned empty data for unit", x, "and it will be skipped."), call. = F)
         return(NULL)
      }
      
      colname <- paste0("val_", x, sep = "")
      names(temp)[names(temp) == "val"] <- colname
      
      temp <- add_attribute_labels(temp, lang)
      
      # temp <- dplyr::select(temp,-dplyr::one_of(c("measureUnitId")))
      
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
    select(one_of("id", "year"), starts_with("val"), "variableName", starts_with("measure"), 
           starts_with("attr"),everything())
 }
 
 df <- tibble::as_tibble(df)
 class(df) <- c("bdl", class(df))
 df
}
