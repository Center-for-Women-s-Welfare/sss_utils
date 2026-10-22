#' Parse and Validate Inflation Target Month
#'
#' Parses a config value like `"2026-04"` into components used by inflation
#' calculations and enforces allowed target months.
#'
#' @param inflation_target Character scalar in `"YYYY-MM"` format.
#' @param allowed_months Integer vector of allowed months. Defaults to January
#'   and April (`c(1L, 4L)`).
#'
#' @return A named list with:
#' \describe{
#'   \item{year}{Integer year, e.g. `2026L`}
#'   \item{month}{Integer month, e.g. `4L`}
#'   \item{month_abb}{Three-letter month abbreviation used by CPI data, e.g. `"Apr"`}
#'   \item{month_full}{Full month name, e.g. `"April"`}
#'   \item{label}{Display label, e.g. `"Apr 2026"`}
#'   \item{iso}{Normalized `"YYYY-MM"` string}
#'   \item{key}{Sortable integer key `year * 100 + month`, e.g. `202604L`}
#' }
#'
#' @examples
#' parse_inflation_target("2026-01")
#' parse_inflation_target("2026-04")
#'
#' @export
parse_inflation_target <- function(inflation_target, allowed_months = c(1L, 4L)) {
  # ---- basic type/length checks ----
  if (!is.character(inflation_target) || length(inflation_target) != 1L || is.na(inflation_target)) {
    stop(
      "`inflation_target` must be a single non-missing character string in 'YYYY-MM' format.",
      call. = FALSE
    )
  }
  
  inflation_target <- trimws(inflation_target)
  
  # ---- strict format check ----
  # Accept only exactly YYYY-MM where MM is 01..12.
  m <- regexec("^([0-9]{4})-(0[1-9]|1[0-2])$", inflation_target)
  parts <- regmatches(inflation_target, m)[[1]]
  
  if (length(parts) == 0L) {
    stop(
      "Invalid `inflation_target` format: '", inflation_target,
      "'. Expected 'YYYY-MM' (example: '2026-04').",
      call. = FALSE
    )
  }
  
  year  <- as.integer(parts[2])
  month <- as.integer(parts[3])
  
  # ---- allowed month check (policy) ----
  allowed_months <- as.integer(allowed_months)
  if (length(allowed_months) == 0L || any(is.na(allowed_months))) {
    stop("`allowed_months` must contain at least one valid month integer.", call. = FALSE)
  }
  if (any(allowed_months < 1L | allowed_months > 12L)) {
    stop("`allowed_months` values must be between 1 and 12.", call. = FALSE)
  }
  
  if (!month %in% allowed_months) {
    allowed_fmt <- sprintf("%02d", sort(unique(allowed_months)))
    stop(
      "Month '", sprintf("%02d", month), "' in `inflation_target` is not allowed. ",
      "Allowed months are: ", paste(allowed_fmt, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  # ---- labels used elsewhere in inflation logic/logging ----
  month_abb  <- month.abb[month]   # Jan, Feb, ... (matches inflation columns)
  month_full <- month.name[month]  # January, February, ...
  iso        <- sprintf("%04d-%02d", year, month)
  label      <- sprintf("%s %d", month_abb, year)
  key        <- as.integer(year * 100L + month)
  
  list(
    year = year,
    month = month,
    month_abb = month_abb,
    month_full = month_full,
    label = label,
    iso = iso,
    key = key
  )
}

# Signal a classed error so callers can catch specific failure modes
sss_abort <- function(message, class) {
  stop(structure(
    class = c(class, "sss_error", "error", "condition"),
    list(message = message, call = NULL)
  ))
}

# Interactive confirmation of a fallback month for one CPI series.
# `available_months` needs columns: year, period, period_num, key.
# Returns the chosen row of `available_months`.
prompt_inflation_fallback <- function(data_point_name,
                                      requested_label,
                                      proposed,
                                      available_months,
                                      prompt_fn = readline) {
  fmt_ym <- function(y, m) sprintf("%04d-%02d", as.integer(y), as.integer(m))
  proposed_ym <- fmt_ym(proposed$year, proposed$period_num)
  
  repeat {
    ans <- tolower(trimws(prompt_fn(sprintf(
      "No inflation data for %s (requested %s). Accept fallback to %s? [Y/n]: ",
      data_point_name, requested_label, proposed_ym
    ))))
    if (ans %in% c("", "y", "yes")) return(proposed)
    if (ans %in% c("n", "no")) break
    message("Please answer Y or N.")
  }
  
  first <- available_months[1, ]
  last  <- available_months[nrow(available_months), ]
  range_txt <- sprintf("%s to %s",
                       fmt_ym(first$year, first$period_num),
                       fmt_ym(last$year, last$period_num))
  
  repeat {
    ym <- trimws(prompt_fn(sprintf(
      "Enter fallback month for %s as YYYY-MM (available %s, 'q' to abort): ",
      data_point_name, range_txt
    )))
    if (tolower(ym) %in% c("q", "quit")) {
      sss_abort("Inflation fallback selection aborted by user.",
                "sss_inflation_fallback_aborted")
    }
    if (!grepl("^[0-9]{4}-(0[1-9]|1[0-2])$", ym)) {
      message("Format must be YYYY-MM (example: 2026-01).")
      next
    }
    key <- as.integer(substr(ym, 1, 4)) * 100L + as.integer(substr(ym, 6, 7))
    hit <- available_months[available_months$key == key, ]
    if (nrow(hit) == 1) return(hit)
    message("No CPI data for ", ym, " in this series.")
  }
}

#' Get Inflation Factor for a Specific Cost Category
#'
#' Calculates the inflation factor from a base CPI (category-specific) to a
#' target CPI month/year defined in config.
#'
#' @param data_point_name Name of the dataset in metadata (e.g. "transportation_fixed", "child care")
#' @param inflation_df BLS CPI values in wide format with a `Series ID` column
#' @param meta_df Metadata with `dataset_name`, `series_id`, and `effective_date`
#' @param child_care_inflation_df Optional data frame with child care base CPI month/year per state
#' @param state_name Required when `data_point_name == "child care"`
#' @param target Parsed target list from `parse_inflation_target()`
#' @param fallback_to_latest_if_missing_target Logical; if TRUE (default), use latest available month
#'   when requested target is unavailable; if FALSE, stop with error.
#' @param on_missing_target What to do when CPI for the requested target month
#'   is unavailable for a series. One of:
#'   \itemize{
#'     \item `"ask"` (default): interactively propose the nearest earlier
#'       available month and let the user accept it or enter another month.
#'       Errors if the session is not interactive.
#'     \item `"latest"`: warn and use the nearest earlier available month.
#'     \item `"error"`: stop.
#'   }
#' @param prompt_fn Function used to read user input (default [readline()]);
#'   mainly useful for testing.
#'
#' @return A numeric inflation factor with attributes:
#' \describe{
#'   \item{requested_target}{e.g., "Apr 2026"}
#'   \item{used_target}{e.g., "Jan 2026" (if fallback occurred)}
#'   \item{used_target_key}{integer key YYYYMM}
#'   \item{latest_available_target}{latest available month label for the series}
#'   \item{fell_back}{TRUE/FALSE}
#'   \item{series_id}{CPI series used}
#'   \item{base_period}{base period ("Annual" or month abbreviation)}
#'   \item{base_year}{base year}
#' }
#'
#' @export
get_inflation_factor <- function(data_point_name,
                                 inflation_df,
                                 meta_df,
                                 child_care_inflation_df = NULL,
                                 state_name = NULL,
                                 target,
                                 on_missing_target = c("ask", "latest", "error"),
                                 prompt_fn = readline) {
  on_missing_target <- match.arg(on_missing_target)

  # ---- validate target ----
  required_target_fields <- c("year", "month", "month_abb", "label", "iso", "key")
  if (!is.list(target) || !all(required_target_fields %in% names(target))) {
    stop(
      "`target` must be the parsed output of parse_inflation_target(). Missing fields: ",
      paste(setdiff(required_target_fields, names(target)), collapse = ", "),
      call. = FALSE
    )
  }
  
  # --- 1. Reshape inflation data ---
  inflation_long <- inflation_df %>%
    dplyr::rename(series_id = `Series ID`) %>%
    tidyr::pivot_longer(
      cols = -series_id,
      names_to = "period_year",
      values_to = "value"
    ) %>%
    tidyr::separate(period_year, into = c("period", "year"), sep = " ", remove = FALSE) %>%
    dplyr::mutate(
      period = stringr::str_trim(period),
      period = stringr::str_to_title(period),
      year = as.integer(year),
      value = suppressWarnings(as.numeric(value))
    )
  
  # normalize month variants (defensive)
  inflation_long <- inflation_long %>%
    dplyr::mutate(
      period = dplyr::case_when(
        period %in% c("Sept", "Sep") ~ "Sep",
        TRUE ~ period
      )
    )
  
  # --- 2. Look up metadata ---
  meta_row <- meta_df %>%
    dplyr::filter(dataset_name == data_point_name)
  
  if (nrow(meta_row) != 1) {
    stop(
      "Expected exactly one metadata row for dataset_name = '", data_point_name,
      "', got: ", nrow(meta_row),
      call. = FALSE
    )
  }
  
  series_id_val <- meta_row$series_id[1]
  
  # ---- 3. Determine base CPI period ----
  if (data_point_name != "child care") {
    base_year <- as.integer(meta_row$effective_date[1])
    base_period <- "Annual"
  } else {
    if (is.null(child_care_inflation_df) || is.null(state_name)) {
      stop("Child care inflation factor requires `child_care_inflation_df` and `state_name`.", call. = FALSE)
    }
    
    state_name_lower <- tolower(state_name)
    
    child_care_row <- child_care_inflation_df %>%
      dplyr::mutate(state_name_clean = tolower(.data$state_name)) %>%
      dplyr::filter(state_name_clean == state_name_lower)
    
    if (nrow(child_care_row) == 0) {
      sss_abort(
        paste0("No row for state '", state_name, "' in child_care_inflation_df."),
        "sss_child_care_base_missing"
      )
    }
    if (nrow(child_care_row) > 1) {
      stop("Expected exactly one row for state_name in child_care_inflation_df, got: ",
           nrow(child_care_row), call. = FALSE)   # duplicate data: do not swallow
    }
    
    base_year <- suppressWarnings(as.integer(child_care_row$base_cpi_year[1]))
    base_period_raw <- child_care_row$base_cpi_month[1]
    
    if (is.na(base_year) || is.null(base_period_raw) ||
        is.na(base_period_raw) || base_period_raw == "") {
      sss_abort(
        paste0("Missing child care base year/month for state: ", state_name),
        "sss_child_care_base_missing"
      )
    }
    
    # parse full month names robustly, return 3-letter abbreviation
    base_period <- format(
      as.Date(paste("1", base_period_raw, "2000"), format = "%d %B %Y"),
      "%b"
    )
    # normalize Sep spelling
    if (identical(base_period, "Sept")) base_period <- "Sep"
  }
  
  # ---- 4. Get base CPI ----
  base_cpi <- inflation_long %>%
    dplyr::filter(
      series_id == series_id_val,
      year == base_year,
      period == base_period
    ) %>%
    dplyr::pull(value) %>%
    as.numeric()
  
  base_cpi <- base_cpi[!is.na(base_cpi)]
  if (length(base_cpi) == 0) {
    stop(
      "Base CPI not found for dataset '", data_point_name,
      "' (series_id: ", series_id_val,
      ", year: ", base_year,
      ", period: ", base_period, ").",
      call. = FALSE
    )
  }
  base_cpi <- base_cpi[1]
  
  # --- 5. Collect available monthly CPI for targeting ---
  month_order <- month.abb
  month_key <- stats::setNames(seq_along(month_order), month_order)
  
  available_months <- inflation_long %>%
    dplyr::filter(
      series_id == series_id_val,
      period %in% month_order,
      !is.na(value)
    ) %>%
    dplyr::mutate(
      period_num = unname(month_key[period]),
      key = year * 100L + period_num
    ) %>%
    dplyr::arrange(key)
  
  if (nrow(available_months) == 0) {
    stop(
      "No valid monthly CPI data found for dataset '", data_point_name,
      "' (series_id: ", series_id_val, ").",
      call. = FALSE
    )
  }
  
  latest_row <- available_months %>% dplyr::slice_tail(n = 1)
  latest_key <- latest_row$key[[1]]
  latest_label <- sprintf("%s %d", latest_row$period[[1]], latest_row$year[[1]])
  
  # --- 6. Choose target row ---
  requested_target_label <- target$label
  fell_back <- FALSE
  
  target_row <- available_months %>% dplyr::filter(key == target$key)
  
  if (nrow(target_row) == 0) {
    # Nearest available month at or before the requested target
    prior <- available_months %>% dplyr::filter(key < target$key)
    
    if (nrow(prior) == 0) {
      sss_abort(
        paste0("No CPI data at or before ", requested_target_label,
               " for '", data_point_name, "' (series ", series_id_val, ")."),
        "sss_inflation_target_unavailable"
      )
    }
    proposed <- dplyr::slice_tail(prior, n = 1)
    proposed_label <- sprintf("%s %d", proposed$period[[1]], proposed$year[[1]])
    
    if (on_missing_target == "error") {
      sss_abort(
        paste0("No CPI data for ", requested_target_label, " for '", data_point_name,
               "' (series ", series_id_val, "; latest available: ", latest_label, ")."),
        "sss_inflation_target_unavailable"
      )
    } else if (on_missing_target == "latest") {
      warning("No inflation data for '", data_point_name, "' at ", requested_target_label,
              " (latest available: ", latest_label, "). Falling back to ",
              proposed_label, ".", call. = FALSE)
      target_row <- proposed
    } else {  # "ask"
      can_prompt <- interactive() || !identical(prompt_fn, readline)
      if (!can_prompt) {
        sss_abort(
          paste0("No inflation data for '", data_point_name, "' at ", requested_target_label,
                 ", and the session is not interactive. Re-run with ",
                 "on_missing_target = \"latest\" or \"error\"."),
          "sss_inflation_target_unavailable"
        )
      }
      target_row <- prompt_inflation_fallback(
        data_point_name, requested_target_label, proposed, available_months, prompt_fn
      )
      message("Using ", sprintf("%s %d", target_row$period[[1]], target_row$year[[1]]),
              " for '", data_point_name, "'.")
    }
    fell_back <- TRUE
  }
  
  target_cpi <- as.numeric(target_row$value[[1]])
  if (is.na(target_cpi)) {
    stop(
      "Target CPI is NA after selection for dataset '", data_point_name,
      "' (series_id: ", series_id_val, ").",
      call. = FALSE
    )
  }
  
  used_period <- target_row$period[[1]]
  used_year   <- target_row$year[[1]]
  used_label  <- sprintf("%s %d", used_period, used_year)
  used_key    <- as.integer(target_row$key[[1]])

  # --- 6. Calculate and return inflation factor ---
  inflation_factor <- target_cpi / base_cpi
  
  # optional sanity warning retained
  if (!is.na(inflation_factor) && inflation_factor <= 1) {
    warning(
      "Inflation factor for '", data_point_name, "' is ", signif(inflation_factor, 6),
      " (<= 1), indicating deflation or no change. Verify source data.",
      call. = FALSE
    )
  }
  
  # attach metadata for logging/auditing
  attr(inflation_factor, "requested_target") <- requested_target_label
  attr(inflation_factor, "used_target") <- used_label
  attr(inflation_factor, "used_target_key") <- used_key
  attr(inflation_factor, "latest_available_target") <- latest_label
  attr(inflation_factor, "fell_back") <- fell_back
  attr(inflation_factor, "series_id") <- series_id_val
  attr(inflation_factor, "base_period") <- base_period
  attr(inflation_factor, "base_year") <- base_year
  attr(inflation_factor, "base_cpi") <- base_cpi
  attr(inflation_factor, "target_cpi") <- target_cpi
  
  inflation_factor
}


#' Get All Inflation Factors for a Given State
#'
#' Computes inflation factors for every cost category used in Self-Sufficiency
#' Standard (SSS) calculations, inflating from each category's base CPI to the
#' target month/year set in the config (see [parse_inflation_target()]).
#'
#' @details
#' Factors are computed for `transportation_fixed`, `transportation_insurance`,
#' `health_oop`, `health_premium` and `child care`. Each category is evaluated
#' against its own CPI series, so availability of the target month can differ
#' by category.
#'
#' **Missing target month.** If CPI is unavailable for the target month in a
#' category's series, behavior is controlled by `on_missing_target`. In the
#' default `"ask"` mode the user is prompted per category to accept the
#' proposed fallback month (the nearest earlier month with data) or enter a
#' different `YYYY-MM`. See [get_inflation_factor()].
#'
#' **Child care defaults.** The child care factor is set to `1.0` (with a
#' warning and a `defaulted` attribute) only when the state's child care base
#' inputs are missing from `child_care_inflation_df` (no row for the state, or
#' no base year/month). All other problems, such as missing base CPI,
#' duplicate rows, unavailable target month or an aborted prompt, are raised as
#' errors.
#'
#' @param state_name Full state name (e.g., `"Alabama"`).
#' @param inflation_df BLS CPI values in wide format with a `Series ID` column
#'   and one column per `"Mon YYYY"` / `"Annual YYYY"` period.
#' @param meta_df Metadata with `dataset_name`, `series_id` and `effective_date`.
#' @param child_care_inflation_df Data frame with `state_name`, `base_cpi_year`
#'   and `base_cpi_month` for child care.
#' @param target Parsed target list from [parse_inflation_target()].
#' @param on_missing_target One of `"ask"` (default), `"latest"` or `"error"`.
#'   See [get_inflation_factor()].
#' @param prompt_fn Function used to read user input (default [readline()]).
#'
#' @return A named list of numeric inflation factors with names
#'   `transportation_fixed`, `transportation_insurance`, `health_oop`,
#'   `health_premium` and `child care`. Each element carries attributes for
#'   logging: `requested_target`, `used_target`, `used_target_key`,
#'   `latest_available_target`, `fell_back`, `series_id`, `base_period` and
#'   `base_year`. A defaulted child care factor instead carries
#'   `defaulted = TRUE` and `requested_target`.
#'   Note that `c()` and `unlist()` drop attributes, so read them from each
#'   list element.
#'
#' @seealso [get_inflation_factor()], [parse_inflation_target()]
#'
#' @examples
#' \dontrun{
#' target <- parse_inflation_target("2026-04")
#' factors <- get_all_inflation_factors(
#'   state_name = "Alabama",
#'   inflation_df = inflation_df,
#'   meta_df = meta_state_data_df,
#'   child_care_inflation_df = child_care_inflation_df,
#'   target = target,
#'   on_missing_target = "ask"
#' )
#' factors$transportation_fixed
#' attr(factors$health_oop, "used_target")
#' }
#'
#' @export
get_all_inflation_factors <- function(state_name,
                                      inflation_df,
                                      meta_df,
                                      child_care_inflation_df,
                                      target,
                                      on_missing_target = c("ask", "latest", "error"),
                                      prompt_fn = readline) {
  on_missing_target <- match.arg(on_missing_target)
  factors <- list()
  
  categories <- c("transportation_fixed", "transportation_insurance",
                  "health_oop", "health_premium")
  for (cat in categories) {
    factors[[cat]] <- get_inflation_factor(
      data_point_name = cat,
      inflation_df = inflation_df,
      meta_df = meta_df,
      target = target,
      on_missing_target = on_missing_target,
      prompt_fn = prompt_fn
    )
  }
  
  factors[["child care"]] <- tryCatch(
    get_inflation_factor(
      data_point_name = "child care",
      inflation_df = inflation_df,
      meta_df = meta_df,
      child_care_inflation_df = child_care_inflation_df,
      state_name = state_name,
      target = target,
      on_missing_target = on_missing_target,
      prompt_fn = prompt_fn
    ),
    # Only "child care base inputs missing" becomes 1.0; everything else propagates.
    sss_child_care_base_missing = function(e) {
      warning("Child care inflation base not found for ", state_name, " (",
              conditionMessage(e), "). Using 1.0.", call. = FALSE)
      out <- 1.0
      attr(out, "defaulted") <- TRUE
      attr(out, "requested_target") <- target$label
      out
    }
  )
  
  factors
}