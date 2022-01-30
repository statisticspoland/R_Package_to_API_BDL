#' @title Get JSON response from BDL API
#' @description Retrieve data from BDL API in JSON format.
#' @details
#'   Data to retrieve from
#'   \href{https://bdl.stat.gov.pl}{The
#'   BDL Web Services} can be specified with filters. If no specific filters
#'   required, it's recommended to use data query like \code{\link{get_data_by_unit_locality}},
#'   than to use \code{\link{get_request}} directly.
#'
#'   To use a proxy to connect, a \code{\link[httr]{use_proxy}} can be
#'   passed to \code{\link[httr]{GET}}. For example
#'   \code{get_request(id, filters,
#'   config = httr::use_proxy(url, port, username, password))}.
#' @param dir A directory of the dataset.
#' @param id A name for the dataset of interested.
#' @param filters A named list of filters. Names of list objects are bdl
#'   filter names and values are vectors with specified filter values. If \code{NULL}
#'   (default) the whole dataset is returned. See API documentation for more on filters
#'   and limitations per query.

#' @param ... Other arguments passed on to \code{\link[httr]{GET}}. For example
#'   a proxy parameters, see details.
#'
#' @return A JSON raw data.
#' @export
#' @examples
#'    # get_request(dir = "data/By-Variable", id = "3643")
#'    # get_request(dir = "data/By-Unit", id = "023200000000", filters = list(year  = c("2000","2010"), 
#'    #             var-Id" = c("2137","148190")))
#'    # get_request(dir = "data/By-Variable", id = "3643", filters = list(year = c("2000","2010"), 
#'    #             unit-Level" = 2, lang = "en"))
#' @keywords utilities database
get_request <- function(dir, id, filters = NULL, ...) {
  url <- build_url(dir = dir, id = id, filters = filters)

  key <- getOption("bdl.api_private_key")
  if (!is.null(key) && nchar_length(key) != 0) {
      h <- c(key)
    names(h) <- "X-ClientId"
    resp <- httr::GET(url, httr::add_headers(.headers = h), httr::content_type_json(), ...)
   
  } else {
    resp <- httr::GET(url, ...)
  }

  status <- httr::status_code(resp)

  # httr::stop_for_status(resp)
  resp <- httr::content(resp, as="text", encoding="UTF-8")
  
  # check status and get json
  msg <- "."
  if (status == 200) {
    jdata <- jsonlite::fromJSON(resp)
    return(jdata)
  } else if (status == 404) {
    stop("Failure to get data. Probably invalid directory. Status code: ",
      status, msg)
  } else if (status == 429) {
    stop("Failure to get data. Probably query reached data limits. Status code: ", status, msg)
  } else if (status == 403) {
    stop("Failure to get data. Probably bad API Key. Status code: ", status, msg)
  } else if (status == 500) {
    stop("Failure to get data. Error occured. Status code: ", status, msg)
  } else {
    stop("Failure to get data. Probably invalid id. Status code: ", status, msg)
  }

}

build_url <- function (dir, id, filters) {
  path <- file.path("api/v1", dir, id)
  # fix weird bug with path building on travis
  if(substring(path, nchar(path)) == "/"){
    path <- substr(path,1,nchar(path)-1)
  }
  
  # prepare filters for query
  filters_list <- as.list(unlist(filters))
  names(filters_list) <- rep(names(filters), lapply(filters, length))
  filters_list <- c(filters_list, "page-Size" = "100", format = "json")

  url_list <- list(scheme = "https",
    hostname = "bdl.stat.gov.pl",
    path = path,
    query = filters_list)
  class(url_list) <- "url"
  url <- httr::build_url(url_list)
  url
}
