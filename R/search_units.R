#' @title Search for units
#' @description Search for a given phrase in unit names.
#' @details
#'
#'   To use a proxy to connect, a \code{\link[httr]{use_proxy}} can be
#'   passed to \code{\link[httr]{GET}}. For example
#'   \code{get_request(id, filters,
#'   config = httr::use_proxy(url, port, username, password))}.
#'
#' @param name  A phrase to search.
#' @param level A number from 0 to 6, filters the returned unit by its level.
#'    If \code{NULL} (default) no level filters apply. Use \code{\link{get_levels}} to find more info.
#' @param year A vector of years. If \code{NULL} (default) returns data for all available years.
#' @param kind A type of unit. More info at: \url{https://bdl.stat.gov.pl/BDL/metadane/teryt/rodzaj}
#' @param sort A type of sorting, "id" (default), "-id", "name", "-name"
#' @param lang  A language of returned data, "pl" (default), "en"
#' @param ... Other arguments passed on to \code{\link[httr]{GET}}. For example
#'   a proxy parameters, see details.
#'
#' @return A dataset as a tibble.
#' @export
#' @examples
#'    # search_units("wro")
#'    # search_units("pol", type = "5")
#' @keywords utilities search units
search_units <- function(name, level = NULL, year = NULL, kind = NULL,
                        sort = c("id","-id","name", "-name"), lang = c("pl","en"), ...) {

  dir <- "units/Search"
  sort <- match.arg(sort)
  lang <- match.arg(lang)
  filters <- list(name = name, year = year, level = level, kind = kind, sort = sort, lang = lang)

  df <- page_download(dir, id = "", filters, ...)
  df
}
