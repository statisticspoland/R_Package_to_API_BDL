#' @title Get variable id codes.
#' @description Retrieve variables for given subjectId.
#' @details
#'   Variables for specified subject optionally filtered by \code{level} and \code{year}.
#'
#'   To use a proxy to connect, a \code{\link[httr]{use_proxy}} can be
#'   passed to \code{\link[httr]{GET}}. For example
#'   \code{get_request(id, filters,
#'   config = httr::use_proxy(url, port, username, password))}.
#'
#' @param subjectId A subject id code. If not specified returns all top level subjects.
#'   Use \code{\link{search_subjects}} or \code{\link{get_subjects}} to get subject id.
#' @param level A number from 0 to 6, filters the returned unit by its level.
#'   If \code{NULL} (default) no level filters apply. Use \code{\link{get_levels}} to find more info.
#' @param year A vector of years. If \code{NULL} (default) returns data for all available years.
#' @param sort A type of sorting, "id" (default), "-id", "subjectId", "-subjectId"
#' @param lang  A language of returned data, "pl" (default), "en"
#' @param ... Other arguments passed on to \code{\link[httr]{GET}}. For example
#'   a proxy parameters, see details.
#'
#' @return A dataset as a tibble.
#' @export
#' @examples
#'     # get_variables("P2425")
#' @keywords utilities search variables
get_variables <- function(subjectId, level = NULL, year = NULL, sort = c("id","-id","subjectId", "-subjectId"),
                          lang = c("pl","en"), ...) {

  if (!is.character(subjectId)) {
    stop("subjectId has to be string value.")
  }

  if (nchar_length(subjectId) == 0 || is.null(subjectId)) {
    stop("subjectId cannot be empty.")
  }

  dir <- "variables"
  sort <- match.arg(sort)
  lang <- match.arg(lang)
  filters <- list("subject-Id" = subjectId, year = year, level = level, sort = sort, lang = lang)

  df <- page_download(dir, id = "", filters, ...)
  df
}
