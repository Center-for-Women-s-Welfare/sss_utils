#' Load SSS configuration (global + state or module)
#'
#' This helper loads configuration YAML files from the repo’s `config/<year>/`
#' structure. It supports either **state-specific** configs
#' (`config/<year>/states/config_<STATE>.yml`) or **module** configs
#' (`config/<year>/module/config_<MODULE>.yml`).
#'
#' If `year` is not supplied, the function looks under `<root>/config/` for
#' 4-digit directories (e.g. `2026`) and uses the highest one. This lets you
#' keep multiple years side-by-side and have the code automatically target the
#' newest year.
#'
#' If the current repo root does not contain the config paths and neither
#' `base_path` nor `repo` were explicitly provided, the function falls back to
#' the local `sss_production` repo via `sss_code_path(repo = "sss_production")`.
#'
#' The global config is read first and the state/module config is merged on
#' top of it (state/module wins). Common sub-lists like `weights` and `paths`
#' are flattened into the top level for convenience.
#'
#' @param state_abbr Optional. Two-letter state abbreviation (e.g. `"OR"`).
#'   Provide this when you want a state-specific config.
#' @param module Optional. Module name (e.g. `"housing"`). Provide this when
#'   there is no state-specific config and the config lives under `module/`.
#' @param year Optional. 4-digit year directory under `config/` (e.g. `"2026"`).
#'   If `NULL`, the function auto-detects the latest year present.
#' @param base_path Optional. Explicit repository root. If supplied, this wins
#'   over `repo` and auto-detection.
#' @param repo Optional. Repository name to resolve via `sss_code_path()`
#'   (e.g. `"sss_production"`). Ignored if `base_path` is supplied.
#'
#' @return A named list of configuration values with state/module values merged
#'   over the global config.
#'
#' @examples
#' \dontrun{
#'   # load Oregon config for latest year
#'   cfg_or <- load_sss_config(state_abbr = "OR")
#'
#'   # load housing module for 2026 explicitly
#'   cfg_housing <- load_sss_config(module = "housing", year = "2026")
#'
#'   # load from a specific clone
#'   cfg_wa <- load_sss_config(state_abbr = "WA",
#'                             base_path = "C:/Users/Lisa/Desktop/local_dev/sss_production")
#' }
#'
#' @export
load_sss_config <- function(state_abbr = NULL,
                            module = NULL,
                            year = NULL,
                            base_path = NULL,
                            repo = NULL) {

  # must have exactly one of state_abbr/module
  if (is.null(state_abbr) && is.null(module)) {
    stop("You must supply either `state_abbr` or `module`.")
  }

  # 1. resolve root
  if (!is.null(base_path)) {
    root <- normalizePath(base_path, winslash = "/", mustWork = TRUE)
  } else {
    root <- if (is.null(repo)) {
      sss_code_path(must_exist = TRUE)
    } else {
      sss_code_path(repo = repo, must_exist = TRUE)
    }
  }

  # 2. determine year if not given
  if (is.null(year)) {
    config_dir <- file.path(root, "config")
    subdirs <- list.dirs(config_dir, full.names = FALSE, recursive = FALSE)
    year_dirs <- subdirs[grepl("^[0-9]{4}$", subdirs)]
    if (length(year_dirs) == 0) {
      stop("No year directories (e.g. 2026) found under: ", config_dir)
    }
    # use the latest year present
    year <- max(year_dirs)
  }

  # 3. build paths
  global_config_path <- file.path(root, "config", year, "config_global.yml")

  if (!is.null(state_abbr)) {
    specific_config_path <- file.path(
      root, "config", year, "states",
      paste0("config_", state_abbr, ".yml")
    )
  } else {
    specific_config_path <- file.path(
      root, "config", year, "module",
      paste0("config_", module, ".yml")
    )
  }

  # 4. fallback to sss_production if global not found
  if (!file.exists(global_config_path) &&
      is.null(base_path) && is.null(repo)) {

    fallback_root <- sss_code_path(repo = "sss_production", must_exist = TRUE)
    global_config_path <- file.path(fallback_root, "config", year, "config_global.yml")

    if (!is.null(state_abbr)) {
      specific_config_path <- file.path(
        fallback_root, "config", year, "states",
        paste0("config_", state_abbr, ".yml")
      )
    } else {
      specific_config_path <- file.path(
        fallback_root, "config", year, "module",
        paste0("config_", module, ".yml")
      )
    }

    root <- fallback_root
  }

  # 5. existence checks
  if (!file.exists(global_config_path)) {
    stop("Global config not found: ", global_config_path)
  }
  if (!file.exists(specific_config_path)) {
    stop("Config not found: ", specific_config_path)
  }

  # 6. read + merge, specific wins
  global_config   <- yaml::read_yaml(global_config_path)
  specific_config <- yaml::read_yaml(specific_config_path)

  config <- utils::modifyList(global_config, specific_config)

  # 7. flatten convenience sublists
  if (!is.null(config$weights)) config <- c(config, config$weights)
  if (!is.null(config$paths))   config <- c(config, config$paths)

  config
}
