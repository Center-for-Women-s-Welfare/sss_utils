#' Save Google Sheets Tabs as Clean CSV Files
#'
#' Downloads tabs from a Google Sheet and saves them as cleaned CSV files,
#' converting numeric-like values (e.g., "Inf", "NA", "") and organizing them by user-defined folders.
#'
#' @param sheet_url URL to the Google Sheet
#' @param tab_map Named list. Keys are sheet tab names, values are CSV file names
#' @param numeric_columns Named list. Keys are tab names, values are vectors of numeric column names
#' @param output_paths Either a named list (per tab) or a single string for all tabs
#' @param base_path Character. Optional base path to prepend to each output path (default = get_base_path())
#' @param dry_run Logical. If TRUE, prints what would happen without saving any files
#'
#' @return No return value; prints messages or saves files
#'
#' @export
save_sheets_to_csv <- function(sheet_url,
                               tab_map,
                               numeric_columns = list(),
                               output_paths,
                           #    base_path = get_base_path(),
                               dry_run = FALSE) {
  # Dependencies
  requireNamespace("googlesheets4")
  requireNamespace("readr")
  requireNamespace("dplyr")

  parse_numeric_columns <- function(df, numeric_cols) {
    parse_special <- function(x) {
      dplyr::case_when(
        x %in% c("Inf", "inf") ~ Inf,
        x %in% c("NA", "na", "") ~ NA_real_,
        TRUE ~ suppressWarnings(as.numeric(x))
      )
    }
    df %>% dplyr::mutate(dplyr::across(dplyr::all_of(numeric_cols), parse_special))
  }

  for (tab in names(tab_map)) {
    message("📥 Reading tab: ", tab)

    sheet_data <- googlesheets4::read_sheet(sheet_url, sheet = tab, col_types = "c") %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ dplyr::na_if(.x, "")))

    if (tab %in% names(numeric_columns)) {
      sheet_data <- parse_numeric_columns(sheet_data, numeric_columns[[tab]])
    }

    sheet_data <- sheet_data %>% dplyr::select(where(~ !all(is.na(.))))

    # Determine output path
    sub_path <- if (is.character(output_paths) && length(output_paths) == 1) {
      output_paths
    } else if (is.list(output_paths) && tab %in% names(output_paths)) {
      output_paths[[tab]]
    } else {
      stop(paste0("❌ No output path defined for tab: ", tab))
    }

    # No base_path prefixing here — output_paths is already absolute
    full_path <- sub_path
    out_file <- file.path(full_path, tab_map[[tab]])


    if (dry_run) {
      message("🔎 Would save: ", normalizePath(out_file, winslash = "/", mustWork = FALSE))
    } else {
      dir.create(full_path, recursive = TRUE, showWarnings = FALSE)
      readr::write_csv(sheet_data, out_file)
      message("✅ Saved: ", normalizePath(out_file))
    }
  }
}

# Example usage
#save_sheets_to_csv(
 # sheet_url = sheet_url,
  #tab_map = tab_map,
  #numeric_columns = numeric_columns,
  #output_paths = "sss_production/data/processed/2026/state_data",
  #dry_run = TRUE
#)
