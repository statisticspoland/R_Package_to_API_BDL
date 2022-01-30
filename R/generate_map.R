#' @title Generate quick map 
#' @description Generate given NUTS level map with data from given variable
#' @details
#'   Generate quickly map for given NUTS level, using BDL data. Default level is 2.
#'   
#'   Maps available for year: 2010-2020
#'   
#'   Provide unit parent id to narrow the map for specific regions.
#'   
#'   Generating lower (levels 5 and 6) level maps can take some time.
#'   
#'   This function requires external map data "bdl.maps" loaded to global environment. 
#'   You can get data here:
#'   \href{https://github.com/statisticspoland/R_Package_to_API_BDL/releases/tag/1.0.4}{Map 
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
#' @param year A single year from 2010-2021 range.
#' @param unitLevel A map and data NUTS level - number from 1 to 6. Use \code{\link{get_levels}} to find more info.
#' @param unitParentId A 12 character NUTS id code of interested unit. Use \code{\link{search_units}} or
#'   \code{\link{get_units}} to find unit id code.
#' @param aggregateId An aggregate id. Use \code{\link{get_aggregates}} for more info.
#' @param palette A palette name or a vector of colors. See tmaptools::palette_explorer() for the named palettes.
#'  Use a "-" as prefix to reverse the palette.
#' @param style Method to process the color scale. Options available are "sd", "equal", "pretty", 
#' "quantile", "kmeans", "hclust", "bclust", "fisher", "jenks", and "log10_pretty".
#' @param n Preferred number of classes. Default is 10.
#' @param names Logical that determines whether the unit names are shown.
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
#'  # generate_map(varId = "60559", year = "2017")
generate_map <- function(varId, year, unitLevel = 2, unitParentId = NULL, aggregateId = NULL, palette = "Blues", 
                         style = NULL, n = 10, names = FALSE, borderLevel = NULL, lang = c("pl","en"), ...) {
  if (length(varId) == 1 && length(year) == 1 && (year >= 2010 && year <= 2021)) {
    if(is.null(unitLevel) || !(unitLevel >= 1 && unitLevel <=6)){
      stop("Wrong unitLevel selected.")
    }
    
    lang <- match.arg(lang)
    
    df <- get_data_by_variable(varId = varId, year = year, unitParentId = unitParentId,
                               unitLevel = unitLevel, aggregateId = aggregateId, lang = lang, ...)
    df <- dplyr::select(df, -"name")
    
    
    home <- Sys.getenv("HOME")
    file_name <- paste0("bdl.maps.", year, ".RData")
    object_name <- paste0("bdl.maps.", year)
    map_file <- paste0(home, "/", file_name)
    
    if(!exists(object_name)){
      
      file_exist <- try(suppressWarnings(load(map_file)), silent = T)

      if(is.error(file_exist)){
        download <- try(
          download.file(
            paste0("https://github.com/statisticspoland/R_Package_to_API_BDL/releases/download/1.0.4/", file_name), 
            map_file, 
            "auto"
          )
        )
        if(is.error(download)){
          stop("There was an error when downloading map files. Please check your internet connection or try again later.")
        }else{
          file_exist2 <- try(suppressWarnings(load(map_file)), silent = T)
          if(!exists(object_name)){
            stop( paste0("Loading map files has failed. Try to Restart your R session or remove any \n",
                         "bdl.maps.xxxx.RData files and download them manually from: \n",
                         "https://github.com/statisticspoland/R_Package_to_API_BDL/releases/tag/1.0.4\n",
                         "and put them into your Home direcotry: ", home))
          }
        }
      }
    }
    
    if(91 %in% df$attrId){
      df[df$attrId == 91, ]$val <- NA
    }
    
    
    selected_map <- get(object_name)
    if(toString(unitLevel) == "1") selected_map <- selected_map$level1
    if(toString(unitLevel) == "2") selected_map <- selected_map$level2
    if(toString(unitLevel) == "3") selected_map <- selected_map$level3
    if(toString(unitLevel) == "4") selected_map <- selected_map$level4
    if(toString(unitLevel) == "5") selected_map <- selected_map$level5
    if(toString(unitLevel) == "6") selected_map <- selected_map$level6
    
    
    shape <- dplyr::inner_join(selected_map, df, by = "id") 
    


    if(!inherits(shape, "sf")) class(shape) <- c("sf")
    
    label <- paste0(get_var_label(varId, lang = lang)," - ",year)
    

    
    if(is.null(style)){
      map <- tmap::tm_shape(shape) +
        tmap::tm_polygons(col = "val", 
                          id = "val",
                          palette = palette, 
                          n = n,
                          popup.vars = c(" " = "name", " " = "attributeDescription"),
                          legend.reverse = T, legend.format = list(text.separator = "-", 
                                                                   big.mark = " "), 
                          textNA = ifelse(lang == "pl", "Brak danych", "Missing")) +
        tmap::tm_layout(title = label)
    } else {
      map <- tmap::tm_shape(shape) +
        tmap::tm_polygons(col = "val", 
                          id = "val",
                          palette = palette, 
                          n = n,
                          style = style,
                          popup.vars = c(" " = "name", " " = "attributeDescription"),
                          legend.reverse = T, legend.format = list(text.separator = "-", big.mark = " "),
                          textNA = ifelse(lang == "pl", "Brak danych", "Missing")) +
        tmap::tm_layout(title = label)
    }
      
    if(names){
      map <- map + tmap::tm_text(text = "name", size = "AREA") + tmap::tm_view(text.size.variable = TRUE)
    }
    
    border_shape <- NULL
    
    all_maps <- get(object_name)
    if(!is.null(borderLevel) && borderLevel == 1) border_shape <- all_maps$level1
    if(!is.null(borderLevel) && borderLevel == 2) border_shape <- all_maps$level2
    if(!is.null(borderLevel) && borderLevel == 3) border_shape <- all_maps$level3
    if(!is.null(borderLevel) && borderLevel == 4) border_shape <- all_maps$level4
    if(!is.null(borderLevel) && borderLevel == 5) border_shape <- all_maps$level5
    if(!is.null(borderLevel) && borderLevel == 6) border_shape <- all_maps$level6
    
      

    if(!is.null(borderLevel) & !is.null(border_shape)){
      map <- map + (tmap::tm_shape(border_shape) +
        tmap::tm_borders(lwd = 1.8))
    }
    
    dots <- list(...)
    in_shiny <- F
    if(!is.null(dots$in.shiny) && length(dots$in.shiny) == 1 && dots$in.shiny == T){
      in_shiny <- T
    }

    map <- tmap::tmap_leaflet(map, in.shiny = in_shiny)
   
    map
    
  } else {
    stop("You can generate map for single variable on single year within 2010-2020.")
  }
}