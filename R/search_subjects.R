#' @title Search for subject codes
#' @description Search for given phrase in subject names
#' @details
#'
#'   To use a proxy to connect, a \code{\link[httr]{use_proxy}} can be
#'   passed to \code{\link[httr]{GET}}. For example
#'   \code{get_request(id, filters,
#'   config = httr::use_proxy(url, port, username, password))}.
#'
#' @param name  A phrase to search.
#' @param sort A type of sorting, "id" (default), "-id", "name", "-name"
#' @param lang A language of returned data, "pl" (default), "en"
#' @param ... Other arguments passed on to \code{\link[httr]{GET}}. For example
#'   a proxy parameters, see details.
#'
#' @return A dataset as a tibble.
#' @export
#' @examples
#'     # search_subjects("samochody")
#'     # search_subjects("car", lang  = "en")
#' @keywords utilities seach subjects
search_subjects <- function(name, sort = c("id","-id","name", "-name"), lang = c("pl","en"), ...) {
  if(is.null(name) || nchar_length(name) <= 0){
    stop("Name cannot be empty.")
  }

  dir <- "subjects/Search"
  sort <- match.arg(sort)
  lang <- match.arg(lang)
  filters <- list(name = name, sort = sort, lang = lang)

  df <- page_download(dir, id = "", filters, ...)
  df
}
