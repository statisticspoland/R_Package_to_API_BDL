#' @title Get subject id codes.
#' @description Retrieve all subjects id's or sub-subjects.
#' @details
#'   To get all top level subjects skip the \code{parentId} parameter or list
#'   sub-subjects for given parent subject.
#'
#'
#'   To use a proxy to connect, a \code{\link[httr]{use_proxy}} can be
#'   passed to \code{\link[httr]{GET}}. For example
#'   \code{get_request(id, filters,
#'   config = httr::use_proxy(url, port, username, password))}.
#'
#' @param parentId A parent subject id code. If not specified returns all top level subjects.
#'   Use \code{\link{search_subjects}} to find subject codes.
#' @param sort A type of sorting, "id" (default), "-id", "name", "-name"
#' @param lang  A language of returned data, "pl" (default), "en"
#' @param ... Other arguments passed on to \code{\link[httr]{GET}}. For example
#'   a proxy parameters, see details.
#'
#' @return A dataset as a tibble.
#' @export
#' @examples
#'    # get_subjects()
#'    # get_subjects("K3")
#'    # get_subjects("G7")
#' @keywords utilities search subjects
get_subjects <- function(parentId = "", sort = c("id","-id","name", "-name"),
                        lang = c("pl","en"), ...) {

  dir <- "subjects"
  sort <- match.arg(sort)
  lang <- match.arg(lang)
  filters <- list("parent-Id" = parentId, sort = sort, lang = lang)

  df <- page_download(dir, id = "", filters, ...)
  df
}
