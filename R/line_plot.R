#' @title Generate quick line plot 
#' @description Generate line plot for one unit/multiple variables or variable/multiple units
#' @details
#'   Generate quickly `ggplot2` plot, using BDL data.
#'   
#'   Plot multiple variable values for one unit or one variable value for multiple units.
#'   
#'   
#'   To use a proxy to connect, a \code{\link[httr]{use_proxy}} can be
#'   passed to \code{\link[httr]{GET}}. For example
#'   \code{get_request(id, filters,
#'   config = httr::use_proxy(url, port, username, password))}.
#' @param data_type A type of data used for generating plot, "unit"(default), "unit.locality","variable","variable.locality"
#' @param unitId A 12 character NUTS unit id or locality  12 character id with 7 characters locality individual id, separated by dash.
#' @param varId A vector of variable Id's (data_type equal "unit" or "unit.locality) 
#'   or single variable (data_type equal "variable" or "variable.locality"). Use \code{\link{search_variables}} or
#'   \code{\link{get_variables}} to find variable id code.
#' @param year A vector of years. If \code{NULL} (default) returns data for all available years.
#' @param aggregateId An aggregate id. Use \code{\link{get_aggregates}} for more info.
#' @param lang A language of returned data, "pl" (default), "en"
#' @param unitParentId A 12 character NUTS id code of interested unit. (Used only with data_type equal "variable" or "variable.locality")
#'    Use \code{\link{search_units}} or
#'    \code{\link{get_units}} to find unit id code.
#' @param unitLevel A number from 0 to 6, filters the returned unit by its level. (Used only with data_type equal "variable")
#'    If \code{NULL} (default) no level filters apply. Use \code{\link{get_levels}} to find more info.
#' @param ... Other arguments passed on to \code{\link[httr]{GET}}. For example
#'   a proxy parameters, see details.
#'   
#' @return A ggplot2 plot.
#' 
#' @export
#'
#' @examples
#'   # line_plot(data_type = "unit", unitId = "000000000000", varId = c("415","420"))
line_plot <- function(data_type = c("unit","unit.locality","variable","variable.locality"), 
                      unitId = NULL, varId = NULL, year = NULL, aggregateId = NULL, lang = NULL, 
                      unitParentId = NULL, unitLevel = NULL, ...) {
  
  data_type <- match.arg(data_type)
  if (data_type == "unit") {
    
    if (length(unitId) == 1) {
      
      df <- get_data_by_unit(unitId = unitId, varId = varId, year = year, type = "label", aggregateId = aggregateId, lang = lang, ...)
      df$combined <- paste0(df$variableName, " [", df$measureName, "]")
      
      title <- unit_info(unitId, lang = lang)
      plot <- ggpubr::ggline(df, x = "year", y = "val", color = "combined", group ="combined", title = title$name)
      
    } else {
      stop("You can line plot only 1 unit with multiple variables.")
    }
    
  } else if (data_type == "unit.locality") {
    
    if (length(unitId) == 1) {
      
      df <- get_data_by_unit_locality(unitId = unitId, varId = varId, year = year, type = "label", lang = lang, ...)
      
      df$combined <- paste0(df$variableName, " [", df$measureName, "]")
      
      title <- unit_locality_info(unitId, lang = lang)
      plot <- ggpubr::ggline(df, x = "year", y = "val", color = "combined", group ="combined", title = title$name)


    } else {
      stop("You can line plot only 1 unit with multiple variables.")
    }
  } else if (data_type == "variable") {
    
    if (length(varId) == 1) {
      
      df <- get_data_by_variable(varId = varId, unitParentId = unitParentId, unitLevel = unitLevel, 
                                 year = year, aggregateId = aggregateId, lang = lang, ...)
      title <- get_var_label(varId, lang = lang)
      title <- paste0(title, " [", get_measure_label(varId, lang = lang),"]")
      plot <- ggpubr::ggline(df, x ="year", y ="val", color = "name", group ="name", title = ifelse(is.null(unitParentId), title, paste0(title, " - Jednostka nadrz\u0119dna: ", toString(unitParentId))))
      
    } else {
      stop("You can line plot only 1 variable with multiple units.")
    }
    
  } else if (data_type == "variable.locality") {
    
    if (length(varId) == 1) {
      
      df <- get_data_by_variable_locality(varId = varId, unitParentId = unitParentId, year = year, lang = lang, ...)
      title <- get_var_label(varId, lang = lang)
      title <- paste0(title, " [", get_measure_label(varId, lang = lang), "]")
      plot <- ggpubr::ggline(df, x ="year", y ="val", color = "name", group ="name", title = ifelse(is.null(unitParentId), title, paste0(title, " - Jednostka nadrz\u0119dna: ", toString(unitParentId))))
      
    } else {
    stop("You can line plot only 1 variable with multiple units.")
    }
    
  } else {
    stop("Wrong data_type parameter.")
  }
  
  title <- plot$plot_env$title
  if(!is.null(aggregateId) && (data_type == "variable" || data_type == "unit")){
    dir <- "aggregates"
    filters <- list(lang = lang)
    
    agg_lab <- get_request(dir, id = aggregateId, filters, ...)
    title <- paste0(plot$plot_env$title, ", Aggregat: ", agg_lab$name)
  }
  
  plot <- ggpubr::ggpar(plot, xlab = F, ylab = F, legend.title = "", title = title)
  
  plot <- plot + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5))
  print(plot)
}