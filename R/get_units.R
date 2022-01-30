#' @title Get unit NUTS codes.
#' @description Retrieve all unit codes or sub to given unit,
#' @details
#'   To get all units skip the \code{parentId} parameter. \emph{Warning!} Downloading
#'   all unit can take around 1 minute.
#'
#'   To use a proxy to connect, a \code{\link[httr]{use_proxy}} can be
#'   passed to \code{\link[httr]{GET}}. For example
#'   \code{get_request(id, filters,
#'   config = httr::use_proxy(url, port, username, password))}.
#'
#' @param parentId A 12 character NUTS id code of parent unit.
#'   Use \code{\link{search_units}} to find unit id code.
#' @param level A number from 0 to 6, filters the returned unit by its level.
#'   If \code{NULL} (default) no level filters apply. Use \code{\link{get_levels}} to find more info.
#' @param sort A type of sorting, "id" (default), "-id", "name", "-name"
#' @param lang  A language of returned data, "pl" (default), "en"
#' @param ... Other arguments passed on to \code{\link[httr]{GET}}. For example
#'   a proxy parameters, see details.
#'
#' @return A dataset as a tibble.
#' @export
#' @examples
#'     # get_units(level = 2)
#'     # get_units("010000000000")
#' @keywords utilities search units
get_units <- function(parentId = "", level = NULL, sort = c("id","-id","name", "-name"),
                      lang = c("pl","en"), ...) {

  if (!is.null(parentId) && (nchar_length(parentId) != 12 && nchar_length(parentId) != 0)) {
    stop("Unit id should be length of 12.")
  }

  dir <- "units"
  sort <- match.arg(sort)
  lang <- match.arg(lang)
  filters <- list("parent-Id" = parentId, level = level, sort = sort, lang = lang)

  df <- page_download(dir, id = "", filters, ...)
  df
}
