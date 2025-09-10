#' Build paths inside the sss_production repo (code vs data)
#'
#' - `data/...` paths resolve under the **Google Drive data base** via `sss_data_path()`.
#' - `src/...`  paths resolve under the **local code base** via `sss_code_path()`.
#'
#' These functions assume you're running from inside a local clone of
#' `sss_production` (so `sss_code_path()` can auto-detect the repo root).
#'
#' @section Environment:
#' Set these once in `~/.Renviron`:
#' \preformatted{
#' SSS_DATA_BASE="G:/Shared drives/CWW Team Drive/SSS/sss_production"
#' SSS_CODE_BASE="C:/Users/<you>/Desktop/local_dev"
#' }
#'
#' @name sss_build_paths
#' @noRd
NULL

# ---- core builder -----------------------------------------------------------

#' Build path in the sss_production project
#'
#' @param top_level Either `"src"` or `"data"`.
#' @param year Character or integer SSS year (e.g., `"2026"`).
#' @param subfolder Subfolder under the year (e.g., `"processing"`, `"raw"`, `"processed"`,
#'   `"final"`, `"reference"`, `"analysis"`, `"loaders"`).
#' @param module Optional module name (e.g., `"child_care"`, `"taxes_state"`).
#' @param state  Optional 2-letter lowercase state (e.g., `"mi"`), or other subfolder.
#' @param filename Optional file name. If `NULL`, returns the directory path.
#' @param check_exists If `TRUE`, error if the resulting path does not exist.
#'
#' @return Absolute, normalized path (character).
#' @export
build_sss_path <- function(top_level,
                           year,
                           subfolder,
                           module = NULL,
                           state = NULL,
                           filename = NULL,
                           check_exists = FALSE) {
  # which base to use?
  tl <- tolower(top_level)
  if (!tl %in% c("src", "data")) {
    stop("top_level must be 'src' or 'data'", call. = FALSE)
  }
  resolver <- if (tl == "src") sss_code_path else sss_data_path

  parts <- c(
    tl, as.character(year), subfolder,
    if (!is.null(module)) module,
    if (!is.null(state))  state,
    if (!is.null(filename)) filename
  )

  # build using the correct base helper
  full_path <- do.call(resolver, c(as.list(parts), list(must_exist = check_exists)))

  if (interactive()) message("Built path: ", full_path)
  full_path
}

# ---- convenience wrappers ---------------------------------------------------

#' Path to a processing file/dir: src/<year>/processing/<module>/<state>/<file>
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

#' Path to analysis file/dir: src/<year>/analysis/<file>
#' @export
build_analysis_path <- function(year = NULL,
                                filename = NULL,
                                check_exists = FALSE) {
  if (missing(year)) year <- get0("sss_year", envir = parent.frame(), ifnotfound = NULL)
  build_sss_path("src", year, "analysis", module = NULL, state = NULL, filename, check_exists)
}

#' Path to loaders file/dir: src/<year>/loaders/<file>
#' @export
build_loaders_path <- function(year = NULL,
                               filename = NULL,
                               check_exists = FALSE) {
  if (missing(year)) year <- get0("sss_year", envir = parent.frame(), ifnotfound = NULL)
  build_sss_path("src", year, "loaders", module = NULL, state = NULL, filename, check_exists)
}

#' Path to RAW data: data/<year>/raw/<module>/<state>/<file>
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

#' Path to PROCESSED data: data/<year>/processed/<module>/<state>/<file>
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

#' Path to FINAL outputs: data/<year>/final/<state>/<file>
#' @export
build_final_data_path <- function(year = NULL,
                                  state = NULL,
                                  filename = NULL,
                                  check_exists = FALSE) {
  if (missing(year))  year  <- get0("sss_year", envir = parent.frame(), ifnotfound = NULL)
  if (missing(state)) state <- get0("state",    envir = parent.frame(), ifnotfound = NULL)

  build_sss_path("data", year, "final", module = NULL, state, filename, check_exists)
}

#' Path to REFERENCE data: data/<year>/reference/<module>/<state>/<file>
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

#' Get all standard SSS directories (validates existence)
#' @export
get_all_sss_dirs <- function(child_care_inputs_state = "inputs") {
  dirs <- list(
    state_data_dir        = build_processed_data_path(module = "state_data",    state = NULL,  check_exists = TRUE),
    fed_tax_dir           = build_processed_data_path(module = "taxes_federal", state = NULL,  check_exists = TRUE),
    state_tax_dir         = build_processed_data_path(module = "taxes_state",   state = NULL,  check_exists = TRUE),
    ref_dir               = build_reference_data_path(                          state = NULL,  check_exists = TRUE),
    county_data_dir       = build_processed_data_path(module = "county_data",   state = NULL,  check_exists = TRUE),
    analysis_dir          = build_analysis_path(                                                check_exists = TRUE),
    loaders_dir           = build_loaders_path(                                                 check_exists = TRUE),
    child_care_inputs_dir = build_processed_data_path(module = "child_care", state = child_care_inputs_state, check_exists = TRUE),
    transportation_dir    = build_processed_data_path(module = "transportation", state = NULL, check_exists = TRUE),
    child_care_dir        = build_processed_data_path(module = "child_care",      state = NULL, check_exists = TRUE)
  )

  missing <- names(dirs)[!dir.exists(unlist(dirs))]
  if (length(missing)) stop("Missing directories: ", paste(missing, collapse = ", "), call. = FALSE)
  dirs
}
