#' @title Get all aggregates
#' @description Retrieve all aggregates with information.
#' @details
#'
#'   To use a proxy to connect, a \code{\link[httr]{use_proxy}} can be
#'   passed to \code{\link[httr]{GET}}. For example
#'   \code{get_request(id, filters,
#'   config = httr::use_proxy(url, port, username, password))}.
#'
#' @param sort A type of sorting, "id" (default), "-id", "name", "-name"
#' @param lang  A language of returned data, "pl" (default), "en"
#' @param ... Other arguments passed on to \code{\link[httr]{GET}}. For example
#'   a proxy parameters, see details.
#'
#' @return A dataset as a tibble.
#' @export
#' @examples
#'    # get_aggregates()
#' @keywords utilities levels
get_aggregates <- function(sort = c("id","-id","name", "-name"),
                        lang = c("pl","en"), ...) {

  dir <- "aggregates"
  sort <- match.arg(sort)
  lang <- match.arg(lang)
  filters <- list(sort = sort, lang = lang)

  df <- page_download(dir, id = "", filters, ...)
  df
}
