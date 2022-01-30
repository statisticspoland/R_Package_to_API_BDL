#' @keywords internal
page_download <- function(dir, id, filters, ...) {
  parse_json <- function(json) {
    if (is.list(json$results) && length(json$results) == 0) {
      stop("Filters returned empty set.")
    }
    
    df_t <- tibble::as_tibble(json$results)
    
    if ("values" %in% colnames(df_t)) {
      df_t <- df_t %>% 
        tidyr::unnest(`values`)
    }
    df_t
  }

  first_page_json <- get_request(dir, id, filters, ...)
  
  pages <- if (!is.null(first_page_json$totalRecords)) {
    floor(first_page_json$totalRecords / 100)
  } else {
    0
  }

  
  df <- parse_json(first_page_json)

  pb <- progress::progress_bar$new(format = "[:bar] :current/:total (:percent)", total = pages)
  pb$tick(0)
  
  for (page_counter in seq_len(pages)) {
    Sys.sleep(0.7)
    filters$page <- page_counter
    json <- get_request(dir, id, filters, ...)
    df_t <- parse_json(json)
    pb$tick()
    df <- dplyr::bind_rows(df, df_t)
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
