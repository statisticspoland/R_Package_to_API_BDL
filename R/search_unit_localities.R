#' @title Search for unit localities
#' @description Search for a given phrase in unit locality names.
#' @details
#'
#'   To use a proxy to connect, a \code{\link[httr]{use_proxy}} can be
#'   passed to \code{\link[httr]{GET}}. For example
#'   \code{get_request(id, filters,
#'   config = httr::use_proxy(url, port, username, password))}.
#'
#' @param name  A phrase to search.
#' @param year A vector of years. If \code{NULL} (default) returns data for all available years.
#' @param sort A type of sorting, "id" (default), "-id", "name", "-name"
#' @param lang  A language of returned data, "pl" (default), "en"
#' @param ... Other arguments passed on to \code{\link[httr]{GET}}. For example
#'   a proxy parameters, see details.
#'
#' @return A dataset as a tibble.
#' @export
#' @examples
#'    # search_unit_localities("wro")
#' @keywords utilities search units localities
search_unit_localities <- function(name, year = NULL, sort = c("id","-id","name", "-name"),
                                  lang = c("pl","en"), ...) {

  dir <- "units/localities/Search"
  sort <- match.arg(sort)
  lang <- match.arg(lang)
  filters <- list(name = name, year = year, sort = sort, lang = lang)

  df <- page_download(dir, id = "", filters, ...)
  df
}
