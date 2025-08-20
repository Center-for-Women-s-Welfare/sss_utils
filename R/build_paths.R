#' Build Path in sss_production Project
#'
#' Constructs standardized paths for src/ or data/ directories in the SSS project.
#'
#' @param top_level Either "src" or "data"
#' @param year The SSS year (e.g., "2026")
#' @param subfolder A subfolder like "processing", "analysis", "loaders", "raw", or "final"
#' @param module Optional. Module name (e.g., "child_care", "housing")
#' @param state Optional. Two-letter lowercase state abbreviation (e.g., "mi")
#' @param filename Optional. File name. If NULL, returns the directory path.
#' @param check_exists Logical. If TRUE, throws an error if the resulting path does not exist.
#'
#' @return A full file or folder path as a character string.
#' @export
build_sss_path <- function(top_level,
                           year,
                           subfolder,
                           module = NULL,
                           state = NULL,
                           filename = NULL,
                           check_exists = FALSE) {
  base_path <- get_base_path()
  if (interactive()) message("Base path: ", base_path)

  path_parts <- c(
    base_path,
    "sss_production",
    top_level,
    as.character(year),
    subfolder
  )

  if (!is.null(module)) {
    path_parts <- c(path_parts, module)
  }

  if (!is.null(state)) {
    path_parts <- c(path_parts, state)
  }

  if (!is.null(filename)) {
    path_parts <- c(path_parts, filename)
  }

  full_path <- do.call(file.path, as.list(path_parts))

  # Print full path when called interactively
  if (interactive()) {
    message("Built path: ", full_path)
  }

  if (check_exists) {
    exists <- if (is.null(filename)) dir.exists(full_path) else file.exists(full_path)
    if (length(exists) != 1 || !exists) {
      stop("Path does not exist: ", full_path)
    }
  }

  return(full_path)

}

# ---- Wrappers ----

#' Build path to a processing file or directory
#'
#' Returns path to src/year/processing/module/[state]/[filename].
#' If filename is NULL, returns the directory path.
#'
#' @export
build_processing_path <- function(year = NULL,
                                  module = NULL,
                                  state = NULL,
                                  filename = NULL,
                                  check_exists = FALSE) {
  if (missing(year))   year   <- get0("sss_year", envir = parent.frame(), ifnotfound = NULL)
  if (missing(module)) module <- get0("module",   envir = parent.frame(), ifnotfound = NULL)
  if (missing(state))  state  <- get0("state",    envir = parent.frame(), ifnotfound = NULL)

  build_sss_path("src", year, "processing", module, state, filename, check_exists)
}


#' Build path to raw data file or directory
#'
#' Returns path to data/year/raw/module/[state]/[filename].
#' If filename is NULL, returns the directory path.
#'
#' @export
build_raw_data_path <- function(year = NULL,
                                module = NULL,
                                state = NULL,
                                filename = NULL,
                                check_exists = FALSE) {
  if (missing(year))   year   <- get0("sss_year", envir = parent.frame(), ifnotfound = NULL)
  if (missing(module)) module <- get0("module",   envir = parent.frame(), ifnotfound = NULL)
  if (missing(state))  state  <- get0("state",    envir = parent.frame(), ifnotfound = NULL)

  build_sss_path("data", year, "raw", module, state, filename, check_exists)
}

#' Build path to processed data file or directory
#'
#' Returns path to data/year/processed/module/[state]/[filename].
#' If filename is NULL, returns the directory path.
#'
#' @export
build_processed_data_path <- function(year = NULL,
                                      module = NULL,
                                      state = NULL,
                                      filename = NULL,
                                      check_exists = FALSE) {
  if (missing(year))   year   <- get0("sss_year", envir = parent.frame(), ifnotfound = NULL)
  if (missing(module)) module <- get0("module",   envir = parent.frame(), ifnotfound = NULL)
  if (missing(state))  state  <- get0("state",    envir = parent.frame(), ifnotfound = NULL)

  build_sss_path("data", year, "processed", module, state, filename, check_exists)
}


#' Build path to final output data file or directory
#'
#' Returns path to data/year/final/[state]/[filename].
#' If filename is NULL, returns the directory path.
#'
#' @export
build_final_data_path <- function(year = NULL,
                                  state = NULL,
                                  filename = NULL,
                                  check_exists = FALSE) {
  if (missing(year))   year   <- get0("sss_year", envir = parent.frame(), ifnotfound = NULL)
#  if (missing(module)) module <- get0("module",   envir = parent.frame(), ifnotfound = NULL)
  if (missing(state))  state  <- get0("state",    envir = parent.frame(), ifnotfound = NULL)

  build_sss_path("data", year, "final", module = NULL, state, filename, check_exists)
}

#' Build path to reference data file or directory
#'
#' Returns path to data/year/reference/module/[state]/[filename].
#' If filename is NULL, returns the directory path.
#'
#' @export
build_reference_data_path <- function(year = NULL,
                                      module = NULL,
                                      state = NULL,
                                      filename = NULL,
                                      check_exists = FALSE) {
  if (missing(year))   year   <- get0("sss_year", envir = parent.frame(), ifnotfound = NULL)
  if (missing(module)) module <- get0("module",   envir = parent.frame(), ifnotfound = NULL)
  if (missing(state))  state  <- get0("state",    envir = parent.frame(), ifnotfound = NULL)

  build_sss_path("data", year, "reference", module, state, filename, check_exists)
}

#' Build path to analysis file or directory
#'
#' Returns path to src/year/analysis/[filename].
#' If filename is NULL, returns the directory path.
#'
#' @export
build_analysis_path <- function(year = NULL,
                                filename = NULL,
                                check_exists = FALSE) {
  if (missing(year))   year   <- get0("sss_year", envir = parent.frame(), ifnotfound = NULL)
#  if (missing(module)) module <- get0("module",   envir = parent.frame(), ifnotfound = NULL)
 # if (missing(state))  state  <- get0("state",    envir = parent.frame(), ifnotfound = NULL)

  build_sss_path("src", year, "analysis", module = NULL, state = NULL, filename, check_exists)
}

#' Build path to loaders file or directory
#'
#' Returns path to src/year/loaders/[filename].
#' If filename is NULL, returns the directory path.
#'
#' @export
build_loaders_path <- function(year = NULL,
                               filename = NULL,
                               check_exists = FALSE) {
  if (missing(year)) year <- get0("sss_year", envir = parent.frame(), ifnotfound = NULL)

  build_sss_path("src", year, "loaders", module = NULL, state = NULL, filename, check_exists)
}



#' Get All Standardized SSS Directory Paths
#'
#' Constructs and returns a named list of key directory paths used in the
#' Self-Sufficiency Standard (SSS) project, including processed data, reference
#' data, analysis scripts, loaders, and child care input directories.
#'
#' The function uses `build_processed_data_path()`, `build_reference_data_path()`,
#' `build_analysis_path()`, and `build_loaders_path()` to generate paths according
#' to the standardized SSS directory structure. It also validates that all returned
#' directories exist.
#'
#' @param child_care_inputs_state Character. State identifier or folder name for
#'   child care inputs. Defaults to `"inputs"`.
#'
#' @return A named list of absolute paths to required directories:
#' \describe{
#'   \item{state_data_dir}{Processed state data directory}
#'   \item{fed_tax_dir}{Processed federal tax data directory}
#'   \item{state_tax_dir}{Processed state tax data directory}
#'   \item{ref_dir}{Reference data directory}
#'   \item{county_data_dir}{Processed county data directory}
#'   \item{analysis_dir}{Analysis scripts directory}
#'   \item{loaders_dir}{Data loader scripts directory}
#'   \item{child_care_inputs_dir}{Processed child care inputs directory}
#' }
#'
#' @examples
#' \dontrun{
#' dirs <- get_all_sss_dirs()
#' dirs$state_data_dir
#' }
#'
#' @export
get_all_sss_dirs <- function(child_care_inputs_state = "inputs") {
  dirs <- list(
    state_data_dir        = build_processed_data_path(module = "state_data",       state = NULL),
    fed_tax_dir           = build_processed_data_path(module = "taxes_federal",    state = NULL),
    state_tax_dir         = build_processed_data_path(module = "taxes_state",      state = NULL),
    ref_dir               = build_reference_data_path(state = NULL),
    county_data_dir       = build_processed_data_path(module = "county_data",      state = NULL),
    analysis_dir          = build_analysis_path(),
    loaders_dir           = build_loaders_path(),
    child_care_inputs_dir = build_processed_data_path(module = "child_care",       state = child_care_inputs_state),
    transportation_dir    = build_processed_data_path(module = "transportation",      state = NULL),
    child_care_dir        = build_processed_data_path(module = "child_care",       state = NULL)
    )

  missing <- names(dirs)[!dir.exists(unlist(dirs))]
  if (length(missing) > 0) {
    stop("Missing directories: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  dirs
}
