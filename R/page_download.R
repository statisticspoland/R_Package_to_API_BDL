#' @keywords internal
page_download <- function(dir, id, filters, ...) {
  df <- tibble::tibble()
  repeat {
    json <- get_request(dir, id, filters, ...)

    if (is.list(json$results) && length(json$results) == 0) {
      stop("Filters returned empty set.")
    }

    df_t <- tibble::as_tibble(json$results)

    if ("values" %in% colnames(df_t)) {
      df_t <- df_t %>%
        # tidyr::unnest(df_t$values)
        tidyr::unnest(`values`)
    }

    df <- dplyr::bind_rows(df, df_t)

    if (is.null(filters$page)) {
      filters$page <- 1
    } else {
      filters$page <- filters$page + 1
    }
    Sys.sleep(0.65)
    if (is.null(json$links) || json$links$self == json$links$last) {
      break
    }
    
  }
  if ("values" %in% colnames(df)) {
    df <- df %>%
      dplyr::select(-dplyr::one_of(c("values")))
  }
  
  if ("hasDescription" %in% colnames(df)) {
    df <- df %>%
      dplyr::rename( "unitHasChanged" = "hasDescription")
    
    if(TRUE %in% df$unitHasChanged){
      warning("Unit metadata has changed during searched year interval. Check description using `unit_info()` or `unit_locality_info()` for localities.", call. = F)
    }
  }
  
  df
}
