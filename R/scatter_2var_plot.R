#' @title Generate quick scatter correlation plot 
#' @description Generate scatter correlation plot for 2 variables
#' @details
#'   Generate quickly `ggplot2` scatter correlation plot, using BDL data.
#'   
#'   Scatter plot 2 variables for given units with regression line, confidence interval and
#'   correlation coefficient.
#'   
#'   To use a proxy to connect, a \code{\link[httr]{use_proxy}} can be
#'   passed to \code{\link[httr]{GET}}. For example
#'   \code{get_request(id, filters,
#'   config = httr::use_proxy(url, port, username, password))}.
#'   
#' @param data_type A type of data used for generating plot, "variable"(default), "variable.locality"
#' @param varId A vector of 2 variable Id's. Use \code{\link{search_variables}} or
#'   \code{\link{get_variables}} to find variable id code.
#' @param year A vector of years. If \code{NULL} (default) returns data for all available years.
#' @param unitParentId A 12 character NUTS id code of interested unit. 
#'   (Used only with data_type equal "variable" or "variable.locality")Use \code{\link{search_units}} or
#'   \code{\link{get_units}} to find unit id code.
#' @param unitLevel A number from 0 to 6, filters the returned unit by its level. 
#'   (Used only with data_type equal "variable")
#'   If \code{NULL} (default) no level filters apply. Use \code{\link{get_levels}} to find more info.
#' @param aggregateId An aggregate id. Use \code{\link{get_aggregates}} for more info.
#' @param lang A language of returned data, "pl" (default), "en"
#' @param ... Other arguments passed on to \code{\link[httr]{GET}}. For example
#'   a proxy parameters, see details.
#'
#' @return A ggplot2 plot.
#' 
#' @export
#'
#' @examples
#'    # scatter_2var_plot(data_type = "variable" ,c("415", "60559"), unitLevel = "2")
scatter_2var_plot <- function(data_type = c("variable", "variable.locality"), 
                              varId, year = NULL, unitParentId = NULL, unitLevel = NULL, 
                              aggregateId = NULL, lang = c("pl","en"), ...) {
  data_type <- match.arg(data_type)
  
  df <- NULL
  if (length(varId) == 2) {
    if (data_type == "variable") {
      
      df <- get_data_by_variable(varId = varId, unitParentId = unitParentId, unitLevel = unitLevel, 
                                 year = year, aggregateId = aggregateId, lang = lang, ...)
    } else if (data_type == "variable.locality") {
      
      df <- get_data_by_variable_locality(varId = varId, unitParentId = unitParentId, 
                                          year = year, lang = lang, ...)
      
    }  
  } else {
    stop("You can scatter plot only 2 variables.")
  }
  
  x_name <- paste0("val_", varId[1])
  x_label <- get_var_label(varId[1], lang = lang)

  
  y_name <- paste0("val_", varId[2])
  y_label <- get_var_label(varId[2], lang = lang)
  
  if(!all(c(x_name, y_name) %in% names(df))){
    stop("One of variables returned empty data.")
  }

  df <- tidyr::drop_na(df)
  plot <- ggpubr::ggscatter(df, x = x_name, y = y_name, xlab = x_label, ylab = y_label,
            color = "name", size = 3, # Points color, shape and size
            add = "reg.line",  # Add regressin line
            add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
            conf.int = TRUE, # Add confidence interval
            cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
            cor.coeff.args = list(method = "pearson", label.x = 3, label.sep = "\n")
  )
  
  title <- ""
  
  if(!is.null(aggregateId) && data_type == "variable"){
    dir <- "aggregates"
    filters <- list(lang = lang)
    
    agg_lab <- get_request(dir, id = aggregateId, filters, ...)
    title <- paste0("Aggregat: ", agg_lab$name)
  }
  
  plot <- ggpubr::ggpar(plot, legend.title = paste0(min(df$year), "-", max(df$year)), title = title)
  
  print(plot)
}