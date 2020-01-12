#' @title Generate quick map 
#' @description Generate given NUTS level map with data from given variable
#' @details
#'   Generate quickly map for given NUTS level, using BDL data. Default level is 2.
#'   
#'   Maps available for year: 2010-2018
#'   
#'   Provide unit parent id to narrow the map for specific regions.
#'   
#'   Generating lower (levels 5 and 6) level maps can take some time.
#'   
#'   This function requires external map data "bdl.maps" loaded to global environment. 
#'   You can get data here:
#'   \href{https://github.com/statisticspoland/R_Package_to_API_BDL/releases/download/1.0.0/bdl.maps.RData}{Map 
#'   download}.
#'   Download data and double-click to load it to environment.
#' 
#'   To use a proxy to connect, a \code{\link[httr]{use_proxy}} can be
#'   passed to \code{\link[httr]{GET}}. For example
#'   \code{get_request(id, filters,
#'   config = httr::use_proxy(url, port, username, password))}.
#'   
#' @param varId A single variable Id.
#'   Use \code{\link{search_variables}} or \code{\link{get_variables}} to find variable id code.
#' @param year A single year from 2010-2018 range.
#' @param unitLevel A map and data NUTS level - number from 1 to 6. Use \code{\link{get_levels}} to find more info.
#' @param unitParentId A 12 character NUTS id code of interested unit. Use \code{\link{search_units}} or
#'   \code{\link{get_units}} to find unit id code.
#' @param aggregateId An aggregate id. Use \code{\link{get_aggregates}} for more info.
#' @param palette A palette name or a vector of colors. See tmaptools::palette_explorer() for the named palettes.
#'  Use a "-" as prefix to reverse the palette.
#' @param n Preferred number of classes. Default is 10.
#' @param borderLevel Adds contours of units on specified level - number from 1 to 6. 
#' Use \code{\link{get_levels}} to find more info.
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
#' \donttest{
#'  generate_map(varId = "60559", year = "2017")
#'  }
generate_map <- function(varId, year, unitLevel = 2, unitParentId = NULL, aggregateId = NULL, palette = "Blues", 
                         n = 10, borderLevel = NULL, lang = c("pl","en"), ...) {
  if(!exists("bdl.maps") || !is.list(bdl.maps)){
    stop(paste0("This function requires external map data \"bdl.maps\" loaded to global environment. \n\n",
                "You can get data here: \n",
                "https://github.com/statisticspoland/R_Package_to_API_BDL/releases/download/1.0.0/bdl.maps.RData \n\n",
                "Download data and double-click to load it to environment."))
      
  }
  
  if (length(varId) == 1 && length(year) == 1 && (year >= 2010 && year <=2018)) {
    if(is.null(unitLevel) || !(unitLevel >= 1 && unitLevel <=6)){
      stop("Wrong unitLevel selected.")
    }
    
    lang <- match.arg(lang)
    
    df <- get_data_by_variable(varId = varId, year = year, unitParentId = unitParentId,
                               unitLevel = unitLevel, aggregateId = aggregateId, lang = lang, ...)
    df <- dplyr::select(df, -"name")
    
    
    if(unitLevel == 1){
      selected_map <- bdl.maps$level1_map
      shape <- dplyr::inner_join(selected_map, df, by = "id")
    }
    if(unitLevel == 2){
      selected_map <- bdl.maps$level2_map
      shape <- dplyr::inner_join(selected_map, df, by = "id")
    }
    if(unitLevel == 3){
      selected_map <- bdl.maps$level3_map
      shape <- dplyr::inner_join(selected_map, df, by = "id")
    }
    if(unitLevel == 4){
      selected_map <- bdl.maps$level4_map
      shape <- dplyr::inner_join(selected_map, df, by = "id")
    }
    if(unitLevel == 5){
      tempyear<- year
      shape <- dplyr::inner_join(bdl.maps$level5_map_units, df, by = "id")
      geo <- as.data.frame(shape) 
      
      tempchanges <- bdl.maps$level5_map_changes
      tempchanges <- as.data.frame(dplyr::filter(tempchanges, year <= tempyear))
      
      for(val in 1:nrow(geo)) {
        if(geo[val,1] %in% tempchanges$id){
          geo[val,2] <- tempchanges[tempchanges$id == geo[val,1],2]
          geo[val,6] <- tempchanges[tempchanges$id == geo[val,1],4]
        }
      }
      
      
      sf::st_geometry(geo) <- sf::st_sfc(geo$geometry)
      sf::st_geometry(shape) <- sf::st_geometry(geo)
      
      # shape <- suppressWarnings(tmaptools::set_projection(shape, projection = tmaptools::get_proj4(tmaptools::get_projection(bdl.maps$level5_map_units)), 
      #                                            current.projection = tmaptools::get_proj4(tmaptools::get_projection(bdl.maps$level5_map_units))))
      # 

    }
    if(unitLevel == 6){
      tempyear<- year
      shape <- dplyr::inner_join(bdl.maps$level6_map_units, df, by = "id")
      geo <- as.data.frame(shape) 
      
      tempchanges <- bdl.maps$level6_map_changes
      tempchanges <- as.data.frame(dplyr::filter(tempchanges, year <= tempyear))
      
      
      for(val in 1:nrow(geo)) {
        if(geo[val,1] %in% tempchanges$id){
          geo[val,2] <- tempchanges[tempchanges$id == geo[val,1],2]
          geo[val,6] <- tempchanges[tempchanges$id == geo[val,1],4]
          
        }
      }
      sf::st_geometry(geo) <- sf::st_sfc(geo$geometry)
      sf::st_geometry(shape) <- sf::st_geometry(geo)
      
      
      # shape <- tmaptools::set_projection(shape, projection = tmaptools::get_proj4(tmaptools::get_projection(bdl.maps$level6_map_units)), 
      #                                                     current.projection = tmaptools::get_proj4(tmaptools::get_projection(bdl.maps$level6_map_units)))
      # 
      
    } 
    shape <- lwgeom::st_make_valid(shape)
    is.error <- function(x) inherits(x, "try-error")
    

    if(!inherits(shape, "sf")) class(shape) <- c("sf")
    
      label <- paste0(get_var_label(varId, lang = lang)," - ",year)
      
    map <- tmap::tm_shape(shape) +
        tmap::tm_polygons(col = "val", id = "val", style ="cont", palette = palette, n = n, 
                          # contrast = c(-0.1, 1),
                          title = get_measure_label(varId = varId), 
                          popup.vars = c(" " = "name")) +
      tmap::tm_layout(title = label)
    
    border_shape <- NULL
    
    if(!is.null(borderLevel) && borderLevel == 1) border_shape <- bdl.maps$level1_map
    if(!is.null(borderLevel) && borderLevel == 2) border_shape <- bdl.maps$level2_map
    if(!is.null(borderLevel) && borderLevel == 3) border_shape <- bdl.maps$level3_map
    if(!is.null(borderLevel) && borderLevel == 4) border_shape <- bdl.maps$level4_map
    if(!is.null(borderLevel) && borderLevel == 5) border_shape <- bdl.maps$level5_map_units
    if(!is.null(borderLevel) && borderLevel == 6) border_shape <- bdl.maps$level6_map_units
    
      

    if(!is.null(borderLevel) & !is.null(border_shape)){
      border_shape <- lwgeom::st_make_valid(border_shape)
      map <- map + (tmap::tm_shape(border_shape) +
        tmap::tm_borders(lwd = 1.8))
    }
    
    map <- tmap::tmap_leaflet(map)
    map
    
  } else {
    stop("You can generate map for single variable on single year within 2010-2018.")
  }
}