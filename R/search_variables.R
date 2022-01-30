#' @title Search for variable codes
#' @description Search for given phrase in variable names
#' @details
#'
#'   To use a proxy to connect, a \code{\link[httr]{use_proxy}} can be
#'   passed to \code{\link[httr]{GET}}. For example
#'   \code{get_request(id, filters,
#'   config = httr::use_proxy(url, port, username, password))}.
#'
#' @param name  A phrase to search.
#' @param subjectId A subject id code. If not specified returns all top level subjects.
#'   Use \code{\link{search_subjects}} or \code{\link{get_subjects}} to get subject id.
#' @param level A number from 0 to 6, filters the returned unit by its level.
#'   If \code{NULL} (default) no level filters apply. Use \code{\link{get_levels}} to find more info.
#' @param year A vector of years. If \code{NULL} (default) returns data for all available years.
#' @param sort A type of sorting, "id" (default), "-id", "name", "-name"
#' @param lang A language of returned data, "pl" (default), "en"
#' @param ... Other arguments passed on to \code{\link[httr]{GET}}. For example
#'   a proxy parameters, see details.
#'
#' @return A dataset as a tibble.
#' @export
#' @examples
#'     # search_variables("samochody")
#'     # search_variables("cars", lang  = "en")
#' @keywords utilities search subjects
search_variables <- function(name, subjectId = NULL, level = NULL, year = NULL,
                            sort = c("id","-id","subjectId", "-subjectId"),
                            lang = c("pl","en"), ...) {

  dir <- "variables/Search"
  sort <- match.arg(sort)
  lang <- match.arg(lang)
  filters <- list(name = name, "subject-Id" = subjectId, year = year, level = level, sort = sort, lang = lang)

  df <- page_download(dir, id = "", filters, ...)
  df
}
