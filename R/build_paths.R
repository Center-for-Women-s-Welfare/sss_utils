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
#'
#' #' @details
#' This file now relies on the environment-based path system defined in `paths.R`
#' (`sss_data_path()`, `sss_code_path()`), replacing older `get_base_path()` logic.

NULL

# ---- core builder -----------------------------------------------------------

#' Build a standardized path within the SSS project
#'
#' This can point either to the shared *data* tree
#'   <SSS_DATA_BASE>/sss_production/...
#' or to the local *code* repo tree
#'   <SSS_CODE_BASE>/<repo>/...
#'
#' @param top_level "src" or "data"
#' @param year SSS year, e.g. 2026
#' @param subfolder e.g. "processing", "raw", "processed", "analysis", "loaders"
#' @param module optional module name
#' @param state optional state name
#' @param filename optional filename
#' @param check_exists if TRUE, error when path/file not found
#' @param where either "data" (Drive) or "code" (local repo)
#' @param repo which repo under SSS_CODE_BASE when where = "code"
#'
#' @export
build_sss_path <- function(top_level,
                           year,
                           subfolder,
                           module = NULL,
                           state = NULL,
                           filename = NULL,
                           check_exists = FALSE,
                           where = c("data", "code"),
                           repo = "sss_production") {

  where <- match.arg(where)

  if (where == "data") {
     base_path <- sss_data_base(must_exist = TRUE)
  } else {
    base_path <- sss_code_path(repo = repo, must_exist = TRUE)
  }

  path_parts <- c(
    base_path,
    top_level,
    as.character(year),
    subfolder
  )

  if (!is.null(module))   path_parts <- c(path_parts, module)
  if (!is.null(state))    path_parts <- c(path_parts, state)
  if (!is.null(filename)) path_parts <- c(path_parts, filename)

  full_path <- do.call(file.path, as.list(path_parts))

  if (interactive()) message("Built path (", where, "): ", full_path)

  if (check_exists) {
    exists <- if (is.null(filename)) dir.exists(full_path) else file.exists(full_path)
    if (!exists) stop("Path does not exist: ", full_path)
  }

  full_path
}



# ---- convenience wrappers ---------------------------------------------------

#' Path to a processing file/dir: src/<year>/processing/<module>/<state>/<file>
#' @export
build_code_processing_path <- function(year = NULL,
                                  module = NULL,
                                  state = NULL,
                                  filename = NULL,
                                  check_exists = FALSE) {
  if (missing(year))   year   <- get0("sss_year", envir = parent.frame(), ifnotfound = NULL)
  if (missing(module)) module <- get0("module",   envir = parent.frame(), ifnotfound = NULL)
  if (missing(state))  state  <- get0("state",    envir = parent.frame(), ifnotfound = NULL)

  build_sss_path("src", year, "processing", module, state, filename, check_exists,where= "code")
}

#' Path to analysis file/dir: src/<year>/analysis/<file>
#' @export
build_code_analysis_path <- function(year = NULL,
                                filename = NULL,
                                check_exists = FALSE) {
  if (missing(year)) year <- get0("sss_year", envir = parent.frame(), ifnotfound = NULL)
  build_sss_path("src", year, "analysis", module = NULL, state = NULL, filename, check_exists, where= "code")
}

#' Path to loaders file/dir: src/<year>/loaders/<file>
#' @export
build_loaders_path <- function(year = NULL,
                               filename = NULL,
                               check_exists = FALSE) {
  if (missing(year)) year <- get0("sss_year", envir = parent.frame(), ifnotfound = NULL)
  build_sss_path("src", year, "loaders", module = NULL, state = NULL, filename, check_exists, where= "code")
}

#' Path to RAW data: data/<year>/raw/<module>/<state>/<file>
#' @export
build_data_raw_path <- function(year = NULL,
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
build_data_processed_path <- function(year = NULL,
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
build_data_final_path <- function(year = NULL,
                                  state = NULL,
                                  filename = NULL,
                                  check_exists = FALSE) {
  if (missing(year))  year  <- get0("sss_year", envir = parent.frame(), ifnotfound = NULL)
  if (missing(state)) state <- get0("state",    envir = parent.frame(), ifnotfound = NULL)

  build_sss_path("data", year, "final", module = NULL, state, filename, check_exists)
}

#' Path to REFERENCE data: data/<year>/reference/<module>/<state>/<file>
#' @export
build_data_reference_path <- function(year = NULL,
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
