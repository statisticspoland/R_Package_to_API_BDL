#' @title Information about unit
#' @description Retrieve information about unit
#' @details
#'
#'   To use a proxy to connect, a \code{\link[httr]{use_proxy}} can be
#'   passed to \code{\link[httr]{GET}}. For example
#'   \code{get_request(id, filters,
#'   config = httr::use_proxy(url, port, username, password))}.
#'
#' @param unitId A 12 character NUTS id code of interested unit. Use \code{\link{search_units}} or
#'   \code{\link{get_units}} to find unit id code.
#' @param lang  A language of returned data, "pl" (default), "en"
#' @param ... Other arguments passed on to \code{\link[httr]{GET}}. For example
#'   a proxy parameters, see details.
#'
#' @return A named list.
#' @export
#' @examples
#'    # unit_info("030210106062")
#' @keywords info units
unit_info <- function(unitId, lang = c("pl","en"), ...) {

  if (!is.null(unitId) &&nchar_length(unitId) != 12) {
    stop("Unit id should be length of 12.")
  }

  dir <- "units"
  lang <- match.arg(lang)
  filters <- list(lang = lang)

    json <- get_request(dir, id = unitId, filters, ...)
    if (is.list(json$results) && length(json$results) == 0) {
      stop("Filters returned empty set.")
    }
  json
}
