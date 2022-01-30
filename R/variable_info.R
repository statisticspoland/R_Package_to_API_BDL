#' @title Information about variable
#' @description Retrieve information about variable.
#' @details
#'
#'   To use a proxy to connect, a \code{\link[httr]{use_proxy}} can be
#'   passed to \code{\link[httr]{GET}}. For example
#'   \code{get_request(id, filters,
#'   config = httr::use_proxy(url, port, username, password))}.
#'
#' @param varId A vector of variable Id's.Use \code{\link{search_variables}} or
#'   \code{\link{get_variables}} to find variable id code.
#' @param lang  A language of returned data, "pl" (default), "en"
#' @param ... Other arguments passed on to \code{\link[httr]{GET}}. For example
#'   a proxy parameters, see details.
#'
#' @return A named list.
#' @export
#' @examples
#'    # variable_info("420")
#' @keywords info variables
variable_info <- function(varId, lang = c("pl","en"), ...) {

  dir <- "variables"
  lang <- match.arg(lang)
  filters <- list(lang = lang)

    json <- get_request(dir, id = varId, filters, ...)
    if (is.list(json$results) && length(json$results) == 0) {
      stop("Filters returned empty set.")
    }
  json
}
