# Modification of Airtabler to actually make it work
# and not the piece of trash it currently is
# FIX YOUR FUCKING CODE BERGANT

# Define the base airtable URL
air_url <- "https://api.airtable.com/v0"

# Accessory function for getting the dataframe from the API call
air_parse <- function(res_obj) {
  if (!is.null(res_obj$records)) {
    res <- res_obj$records
  } else {
    res <- res_obj
  }
  res
}

get_airtable_records <- function(base, table_name, air_api_key, record_id = NULL) {
  offset <- NULL
  search_path <- table_name

  # Initialize empty dataframe to store results
  res_df <- data.frame()

  if (!missing(record_id)) {
    search_path <- paste0(search_path, "/", record_id)
  }

  request_url <- sprintf("%s/%s/%s?", air_url, base, table_name)
  request_url <- utils::URLencode(request_url)

  repeat {
    # Append offset parameter to URL if it exists
    param_list <- list()
    if (!is.null(offset)) {
      param_list$offset <- offset
    }

    request_url <- httr::modify_url(request_url, query = param_list)

    res <- httr::GET(
      url = request_url,
      config = httr::add_headers(Authorization = paste("Bearer", air_api_key))
    )

    res_obj <- jsonlite::fromJSON(httr::content(res, as = "text"))
    ret <- air_parse(res_obj)
    ret <- cbind(ret$id, ret$fields)

    # Ensure the columns of ret match those of res_df
    if (nrow(res_df) == 0) {
      res_df <- ret
    } else {
      # Add missing columns to ret
      missing_cols <- setdiff(names(res_df), names(ret))
      for (col in missing_cols) {
        ret[[col]] <- NA
      }
      # Add missing columns to res_df
      missing_cols <- setdiff(names(ret), names(res_df))
      for (col in missing_cols) {
        res_df[[col]] <- NA
      }
      # Reorder columns to match
      ret <- ret[, names(res_df)]
      res_df <- rbind(res_df, ret)
    }

    # Check if there is an offset for the next page
    if (is.null(res_obj$offset)) {
      break
    } else {
      offset <- res_obj$offset
    }
  }

  return(res_df)
}
