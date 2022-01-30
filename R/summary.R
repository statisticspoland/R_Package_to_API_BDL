#' @title Summarize bdl data frame
#' @description Prints brief summary with basic statistical functions like mean, standard deviation, variance,
#'          min and max for bdl data frame.
#' @param object bdl data frame to summarise
#' @param ... other arguments ignored (for compatibility with generic)
#' @examples
#' # df <- get_data_by_variable(varId = "3643")
#' # summary(df)
#' @export
summary.bdl <- function(object, ...){
  na.rm <- FALSE
  if ("id" %in% colnames(object) && "val" %in% colnames(object)) {
    dfSum <- object %>%
      dplyr::group_by_(~id) %>%
      dplyr::mutate(mean = mean(val, na.rm = na.rm),
                    std = stats::sd(val, na.rm = na.rm),
                    min = min(val, na.rm = na.rm),
                    max = max(val, na.rm = na.rm),
                    variance = stats::var(val, na.rm = na.rm)) %>%
      dplyr::select(-dplyr::one_of(c("year", "attrId", "val"))) %>%
      dplyr::distinct_(~id, .keep_all = TRUE)

    dfSum
  } else {
    stop("Wrong data frame column names. Summary.bdl is available only for single variable bdl data frames.")
  }
}
