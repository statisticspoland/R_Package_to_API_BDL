#' @title Generate quick pie plot 
#' @description Generate pie plot for variable/multiple units
#' @details
#'   Generate quickly `ggplot2` plot, using BDL data.
#'   
#'   Pie plot one variable value for multiple units on single year.
#'   
#'   
#'   To use a proxy to connect, a \code{\link[httr]{use_proxy}} can be
#'   passed to \code{\link[httr]{GET}}. For example
#'   \code{get_request(id, filters,
#'   config = httr::use_proxy(url, port, username, password))}.
#' @param data_type A type of data used for generating plot, "variable"(default), "variable","variable.locality"
#' @param varId A variable Id. Use \code{\link{search_variables}} or
#'   \code{\link{get_variables}} to find variable id code.
#' @param year A single year. If \code{NULL} (default) returns data for all available years.
#' @param unitParentId A 12 character NUTS id code of interested unit. Use \code{\link{search_units}} or
#'   \code{\link{get_units}} to find unit id code.
#' @param unitLevel A number from 0 to 6, filters the returned unit by its level. 
#'   (Used only with data_type equal "variable")
#'   If \code{NULL} (default) no level filters apply. Use \code{\link{get_levels}} to find more info.
#' @param aggregateId An aggregate id. Use \code{\link{get_aggregates}} for more info.
#' @param label Logical; if TRUE (default) adds labels.
#' @param lang A language of returned data, "pl" (default), "en"
#' @param ... Other arguments passed on to \code{\link[httr]{GET}}. For example
#'   a proxy parameters, see details.
#'
#' @return A ggplot2 plot.
#' 
#' @export
#'
#' @examples
#'    # pie_plot(data_type ="variable" ,"1", "2018",unitParentId="042214300000", unitLevel = "6")
pie_plot <- function(data_type = c("variable","variable.locality"), 
                     varId, year, unitParentId = NULL, unitLevel = NULL, 
                     aggregateId = NULL, label = T, lang = c("pl","en"), ...) {
  data_type <- match.arg(data_type)
  df <- NULL
  if (length(varId) == 1 && length(year) == 1) {
    if (data_type == "variable") {
      
    df <- get_data_by_variable(varId = varId, unitParentId = unitParentId, unitLevel = unitLevel, 
                               year = year, aggregateId = aggregateId, lang = lang, ...)
    } else if (data_type == "variable.locality") {
      
      df <- get_data_by_variable_locality(varId = varId, unitParentId = unitParentId, 
                                          year = year, lang = lang, ...)
      
    }  
  } else {
    stop("You can pie plot only 1 variable with multiple units on 1 year.")
  }
  
  title <- get_var_label(varId, lang = lang)
  
  if(!is.null(aggregateId) && data_type == "variable"){
    dir <- "aggregates"
    filters <- list(lang = lang)
    
    agg_lab <- get_request(dir, id = aggregateId, filters, ...)
    title <- paste0(title, " \nAggregat: ", agg_lab$name)
  }

  if(label){
    df$label <- df$val
  }else{
    df$label <- ""
  }
  
  plot <- ggpubr::ggpie(df, "val", fill = "name", label = "label",
                        color = "black", title = title, palette = randomcoloR::distinctColorPalette(nrow(df)))
  plot <- plot + ggplot2::aes("", val, fill = "name", label = "label")
  
  
  
  plot <- ggpubr::ggpar(plot, legend = "right", legend.title = toString(year), ticks = F)
  

  print(plot)
}