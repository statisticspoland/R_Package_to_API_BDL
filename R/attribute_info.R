#' @title Information about attribute
#' @description Retrieve information about attribute.
#' @details
#'
#'   To use a proxy to connect, a \code{\link[httr]{use_proxy}} can be
#'   passed to \code{\link[httr]{GET}}. For example
#'   \code{get_request(id, filters,
#'   config = httr::use_proxy(url, port, username, password))}.
#'
#' @param attrId A single attribute id.Use \code{\link{get_attributes}} to find more info.
#' @param lang  A language of returned data, "pl" (default), "en"
#' @param ... Other arguments passed on to \code{\link[httr]{GET}}. For example
#'   a proxy parameters, see details.
#'
#' @return A named list.
#' @export
#' @examples
#'    # attribute_info("1")
#' @keywords info variables
attribute_info <- function(attrId, lang = c("pl","en"), ...) {

  dir <- "attributes"
  lang <- match.arg(lang)
  filters <- list(lang = lang)

    json <- get_request(dir, id = attrId, filters, ...)
    if (is.list(json$results) && length(json$results) == 0) {
      stop("Filters returned empty set.")
    }
  json
}
