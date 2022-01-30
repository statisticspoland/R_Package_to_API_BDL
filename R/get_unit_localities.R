#' @title Get unit locality codes.
#' @description Retrieve unit locality codes.
#' @details
#'
#'   To use a proxy to connect, a \code{\link[httr]{use_proxy}} can be
#'   passed to \code{\link[httr]{GET}}. For example
#'   \code{get_request(id, filters,
#'   config = httr::use_proxy(url, port, username, password))}.
#'
#' @param parentId A 12 character NUTS id code of parent unit.
#'   Use \code{\link{search_units}} to find unit id code.
#' @param sort A type of sorting, "id" (default), "-id", "name", "-name"
#' @param lang  A language of returned data, "pl" (default), "en"
#' @param ... Other arguments passed on to \code{\link[httr]{GET}}. For example
#'   a proxy parameters, see details.
#'
#' @return A dataset as a tibble.
#' @export
#' @examples
#'     # get_unit_localities("030210106062")
#' @keywords utilities search units localities
get_unit_localities <- function(parentId, sort = c("id","-id","name", "-name"),
                                lang = c("pl","en"), ...) {
  if (!is.null(parentId) && (nchar_length(parentId) != 12 && nchar_length(parentId) != 0)) {
    stop("Unit id should be length of 12.")
  }

  dir <- "units/localities"
  sort <- match.arg(sort)
  lang <- match.arg(lang)
  filters <- list("parent-Id" = parentId, sort = sort, lang = lang)

  df <- page_download(dir, id = "", filters, ...)
  df
}
