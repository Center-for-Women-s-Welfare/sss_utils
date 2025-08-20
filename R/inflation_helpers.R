#' Get Inflation Factor
#'
#' Calculates the inflation factor between a dataset's base CPI value and the most recent available CPI,
#' using metadata and BLS CPI series data.
#'
#' @param data_point_name The dataset name as listed in the metadata file (e.g., "transportation_fixed")
#' @param inflation_df A wide-format CPI data frame with Series ID columns
#' @param meta_df A metadata table with dataset_name, series_id, effective_date
#' @param child_care_inflation_df Optional. Data frame with child care CPI base months by state
#' @param state_name Optional. State name (required for "child care")
#'
#' @return A numeric inflation factor (e.g., 1.1045)
#' @export
get_inflation_factor <- function(data_point_name, inflation_df, meta_df,
                                 child_care_inflation_df = NULL, state_name = NULL) {
  # --- 1. Reshape Inflation Data ---
  inflation_long <- inflation_df %>%
    rename(series_id = `Series ID`) %>%
    pivot_longer(
      cols = -series_id,
      names_to = "period_year",
      values_to = "value"
    ) %>%
    separate(period_year, into = c("period", "year"), sep = " ", remove = FALSE) %>%
    mutate(
      period = str_trim(period),
      period = str_to_title(period),
      year = as.numeric(year)
    )
  
  # --- 2. Lookup Metadata ---
  meta_row <- meta_df %>%
    filter(dataset_name == data_point_name)
  
  if (nrow(meta_row) != 1) {
    stop("Expected exactly one metadata row for dataset_name = ", data_point_name,
         ", got: ", nrow(meta_row))
  }
  
  series_id_val <- meta_row$series_id[1]
  
  # --- 3. Get Base CPI Year/Month ---
  if (data_point_name != "child care") {
    base_year <- meta_row$effective_date[1]
    base_period <- "Annual"
  } else {
    if (is.null(child_care_inflation_df) || is.null(state_name)) {
      stop("Child care inflation requires state and child_care_inflation_df.")
    }
    
    state_name_lower <- tolower(state_name)
    child_care_row <- child_care_inflation_df %>%
      mutate(state_name_clean = tolower(state_name)) %>%
      filter(state_name_clean == state_name_lower)
    
    if (nrow(child_care_row) != 1) {
      stop("Expected exactly one row for state in child_care_inflation_df, got: ", nrow(child_care_row))
    }
    
    base_year <- child_care_row$base_cpi_year[1]
    base_period <- child_care_row$base_cpi_month[1]
    if (!is.null(base_period) && !is.na(base_period)) {
      base_period <- format(as.Date(paste("1", base_period, "2000"), format = "%d %B %Y"), "%b")
    }
  }
  
  # --- 4. Get Base CPI ---
  base_cpi <- inflation_long %>%
    filter(series_id == series_id_val, year == base_year, period == base_period) %>%
    pull(value) %>%
    as.numeric()
  
  # --- 5. Get Most Recent CPI ---
  month_order <- c("January", "February", "March", "April", "May", "June", "July",
                   "August", "Sept", "October", "November", "December")
  
  most_recent_row <- inflation_long %>%
    filter(series_id == series_id_val, period %in% c(month_order, "Annual")) %>%
    mutate(
      period_num = ifelse(period == "Annual", 13, match(period, month_order)),
      year_period = year * 100 + period_num
    ) %>%
    arrange(desc(year_period)) %>%
    slice(1)
  
  most_recent_cpi <- as.numeric(most_recent_row$value)
  
  # --- 6. Return Inflation Factor ---
  return(most_recent_cpi / base_cpi)
}



#' Get All Inflation Factors for a Given State
#'
#' Returns a named list of inflation factors for all key cost categories used
#' in Self-Sufficiency Standard calculations.
#'
#' @param state_name Full state name (e.g., "Alabama")
#' @param inflation_df BLS CPI values, in wide format with `Series ID` column
#' @param meta_df Metadata with dataset_name, series_id, effective_date
#' @param child_care_inflation_df Data frame containing child care base month/year per state
#'
#' @return A named list of numeric inflation factors
#' @export
get_all_inflation_factors <- function(state_name,
                                      inflation_df,
                                      meta_df,
                                      child_care_inflation_df) {
  factors <- list()
  
  # Non-child-care categories
  categories <- c("transportation_fixed", "transportation_insurance", "health_oop", "health_premium")
  for (cat in categories) {
    factors[[cat]] <- get_inflation_factor(
      data_point_name = cat,
      inflation_df = inflation_df,
      meta_df = meta_df
    )
  }
  
  # Child care: special logic with tryCatch fallback
  factors[["child care"]] <- tryCatch(
    get_inflation_factor(
      data_point_name = "child care",
      inflation_df = inflation_df,
      meta_df = meta_df,
      child_care_inflation_df = child_care_inflation_df,
      state_name = state_name
    ),
    error = function(e) {
      message("Warning: Child care inflation factor not found for ", state_name, ". Using 1.0.")
      return(1.0)
    }
  )
  
  return(factors)
}
