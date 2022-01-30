#' @title Information about unit locality
#' @description Retrieve information about unit locality
#' @details
#'
#'   To use a proxy to connect, a \code{\link[httr]{use_proxy}} can be
#'   passed to \code{\link[httr]{GET}}. For example
#'   \code{get_request(id, filters,
#'   config = httr::use_proxy(url, port, username, password))}.
#'
#' @param unitId A 12 character NUTS id with 7 characters locality individual id, separated by dash.
#'   Use \code{\link{search_unit_localities}} or \code{\link{get_unit_localities}} to find unit id code.
#' @param lang  A language of returned data, "pl" (default), "en"
#' @param ... Other arguments passed on to \code{\link[httr]{GET}}. For example
#'   a proxy parameters, see details.
#'
#' @return A named list.
#' @export
#' @examples
#'    # unit_locality_info("030210106062-0189782")
#' @keywords info units localities
unit_locality_info <- function(unitId, lang = c("pl","en"), ...) {

  if (!is.null(unitId) && nchar_length(unitId) != 20 && substr(unitId, 7, 7) != "-") {
    stop("Unit id should be 12 characters NUTS id code with 7 characters locality individual id, separated by dash.")
  }

  dir <- "units/localities"
  lang <- match.arg(lang)
  filters <- list(lang = lang)

    json <- get_request(dir, id = unitId, filters, ...)
    if (is.list(json$results) && length(json$results) == 0) {
      stop("Filters returned empty set.")
    }
  json
}
