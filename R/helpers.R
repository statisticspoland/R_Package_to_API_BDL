#' @keywords internal
get_var_label <- function(varId, lang = "pl") {
  var_suffix <- get_request(dir = "variables", id = toString(varId), filters = list(lang = lang))

  var_prefix <- get_request(dir = "subjects", id = var_suffix$subjectId, filters = list(lang = lang))
  var_suffix <- tibble::as_tibble(var_suffix)
  var_suffix <- var_suffix %>%
    utils::head(1) %>%
    dplyr::select(dplyr::starts_with("n"))
  
  temp <- list()
  for(val in 1:length(var_prefix$dimensions)){
    temp[val] <- paste(var_prefix$dimensions[val], var_suffix[val], sep = ": ")
  }
  
  var_suffix <- paste(temp, collapse = ", ")
  variable_label <- paste(var_prefix$name, var_suffix, sep = " - ")
  variable_label
}
#' @keywords internal
get_measure_label <- function(varId, lang = "pl") {
  var_suffix <- get_request(dir = "variables", id = toString(varId), filters = list(lang = lang))

  var_suffix <- tibble::as_tibble(var_suffix)
  var_suffix <- var_suffix %>%
    utils::head(1)
  measure_label <- toString(var_suffix$measureUnitName)

  measure_label
}
#' @keywords internal
get_attr_label <- function(attrId, lang = "pl") {
  attr_suffix <- attribute_info(attrId, lang = lang)
  attr_label <- toString(attr_suffix$description)
  attr_label
}
#' @keywords internal
nchar_length <- function(x) {`if`(any(is.na(x)), 0, nchar(x)) }

#' @keywords internal
add_attribute_labels <- function(x, lang = "pl") {
  attributes <- unique(x$attrId)
  attribute_labels <- lapply(attributes, get_attr_label, lang = lang)
  names(attribute_labels) <- attributes
  df <- dplyr::mutate(x, attributeDescription = as.character(attribute_labels[as.character(x$attrId)]))
  df
}

#' @keywords internal
add_measure_columns <- function(varId, df, lang = "pl") {
  measure_info <- variable_info(varId, lang = lang)
  df <- df %>% dplyr::mutate(measureUnitId = as.character(measure_info$measureUnitId), measureName = as.character(measure_info$measureUnitName))
  df
}

#' @keywords internal
is.error <- function(x) inherits(x, "try-error")
