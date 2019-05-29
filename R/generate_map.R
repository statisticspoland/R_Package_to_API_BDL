#' @title Generate quick map 
#' @description Generate given  NUTS level map with data from given variable
#' @details
#'   Generate quickly map for given NUTS level, using BDL data. Default level is 2.
#'   Provide unit parent id to narrow the map for specific regions.
#'   
#'   Generating lower (leves 5 and 6) level maps can take some time.
#'   
#'   To use a proxy to connect, a \code{\link[httr]{use_proxy}} can be
#'   passed to \code{\link[httr]{GET}}. For example
#'   \code{get_request(id, filters,
#'   config = httr::use_proxy(url, port, username, password))}.
#'   
#' @param varId A single variable Id.
#'   Use \code{\link{search_variables}} or \code{\link{get_variables}} to find variable id code.
#' @param year A single year.
#' @param unitLevel A map and data NUTS level - number from 0 to 6. Use \code{\link{get_levels}} to find more info.
#' @param unitParentId A 12 character NUTS id code of interested unit. Use \code{\link{search_units}} or
#'   \code{\link{get_units}} to find unit id code.
#' @param aggregateId An aggregate id. Use \code{\link{get_aggregates}} for more info.
#' @param lang  A language of returned data, "pl" (default), "en"
#' @param ... Other arguments passed on to \code{\link[httr]{GET}}. For example
#'   a proxy parameters, see details.
#'   
#' @return
#'   A tmap map.
#'   
#' @export
#'
#' @examples
#'  bdl_map <- generate_map(varId = "60559", year = "2017")
generate_map <- function(varId, year, unitLevel = 2, unitParentId = NULL, aggregateId = NULL, lang = c("pl","en"), ...) {
  if (length(varId) == 1 && length(year) == 1) {
    if(is.null(unitLevel) || !(unitLevel >= 1 && unitLevel <=6)){
      stop("Wrong unitLevel selected.")
    }
    
    lang <- match.arg(lang)
    
    df <- get_data_by_variable(varId = varId, year = year, unitParentId = unitParentId,
                               unitLevel = unitLevel, aggregateId = aggregateId, lang = lang, ...)
    df <- dplyr::select(df, -"name")
    
    
    if(unitLevel == 1){
      selected_map <- bdl::level1_map
    }
    if(unitLevel == 2){
      selected_map <- bdl::level2_map
    }
    if(unitLevel == 3){
      selected_map <- bdl::level3_map
    }
    if(unitLevel == 4){
      selected_map <- bdl::level4_map
    }
    if(unitLevel == 5){
      selected_map <- bdl::level5_map
    }
    if(unitLevel == 6){
      selected_map <- bdl::level6_map
    } 
    
    
    shape <- dplyr::inner_join(selected_map, df, by = "id")
    
    if(!inherits(shape, "sf")) class(shape) <- c("sf")
    

    label <- paste0(get_var_label(varId)," - ",year)
    
    is.error <- function(x) inherits(x, "try-error")

    map <- try(tmap::tmap_leaflet(tmap::tm_shape(shape) +
          tmap::tm_polygons("val", id="name", style ="cont", palette = "seq", title = "") +
          tmap::tm_layout(aes.palette = list(seq = "Blues"), title = label))
    , silent = T)
    
    if(is.error(map)) {
      return(generate_map(varId, year, unitLevel, unitParentId, aggregateId, lang, ...))
    }else {
      return(map)
    }
    
    
    # tmap::tmap_mode("view")

    # error <- try(print(map), silent = T)
    
    # if(is.error(error)) {
    #   print("error trigg")
    #   return(generate_map(varId, year, unitLevel, unitParentId, aggregateId, lang, ...))
    # }else{
    #   print("non error")
    #   return(map)
    # }

    
    
  } else {
    stop("You can generate map for single variable on single year.")
  }
}