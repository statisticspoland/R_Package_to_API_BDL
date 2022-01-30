#' @title Information about subject
#' @description Retrieve information about subject
#' @details
#'
#'   To use a proxy to connect, a \code{\link[httr]{use_proxy}} can be
#'   passed to \code{\link[httr]{GET}}. For example
#'   \code{get_request(id, filters,
#'   config = httr::use_proxy(url, port, username, password))}.
#'
#' @param subjectId A subject id code. If not specified returns all top level subjects.
#'   Use \code{\link{search_subjects}} or \code{\link{get_subjects}} to find subject codes.
#' @param lang  A language of returned data, "pl" (default), "en"
#' @param ... Other arguments passed on to \code{\link[httr]{GET}}. For example
#'   a proxy parameters, see details.
#'
#' @return A named list.
#' @export
#' @examples
#'    # subject_info("G7")
#' @keywords info subjects
subject_info <- function(subjectId, lang = c("pl","en"), ...) {

  dir <- "subjects"
  lang <- match.arg(lang)
  filters <- list(lang = lang)

    json <- get_request(dir, id = subjectId, filters, ...)
    if (is.list(json$results) && length(json$results) == 0) {
      stop("Filters returned empty set.")
    }
  json
}
