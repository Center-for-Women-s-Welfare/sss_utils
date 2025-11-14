#' Load SSS configuration (global + state or module)
#'
#' Loads configuration YAML files from the `config/<year>/` directory structure.
#' Supports either **state-specific** configs (`config/<year>/states/config_<STATE>.yml`)
#' or **module** configs (`config/<year>/module/config_<MODULE>.yml`).
#'
#' @param year  4-digit year directory under `config/` (e.g., `2026`).
#' @param state_abbr Optional. Two-letter state abbreviation (e.g. `"IA"`).
#' @param module Optional. Module name (e.g. `"housing"`).
#' @param base_path Optional. Explicit repo root override.
#' @param repo Optional. Repository name (e.g., `"sss_production"`) to resolve
#'   via `sss_code_path()`. Ignored if `base_path` is provided.
#'
#' @return Named list of configuration values with state/module overrides merged
#'   over the global config.
#'
#' @examples
#' \dontrun{
#'   # load 2026 Iowa config
#'   cfg <- load_sss_config(2026, state_abbr = "IA")
#'
#'   # load 2026 housing module
#'   cfg <- load_sss_config(2026, module = "housing")
#' }
#' @export
load_sss_config <- function(year,
                            state_abbr = NULL,
                            module = NULL,
                            base_path = NULL,
                            repo = NULL) {

  # input validation
  if (missing(year) || !grepl("^[0-9]{4}$", as.character(year))) {
    stop("You must provide a 4-digit `year`, e.g., load_sss_config(2026, 'IA').")
  }
  if (is.null(state_abbr) && is.null(module)) {
    stop("You must supply either `state_abbr` or `module`.")
  }

  # resolve root
  if (!is.null(base_path)) {
    root <- normalizePath(base_path, winslash = "/", mustWork = TRUE)
  } else {
    root <- if (is.null(repo)) {
      sss_code_path(must_exist = TRUE)
    } else {
      sss_code_path(repo = repo, must_exist = TRUE)
    }
  }

  # build paths
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

  # fallback logic
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

  if (!file.exists(global_config_path)) {
    stop("Global config not found: ", global_config_path)
  }
  if (!file.exists(specific_config_path)) {
    stop("Config not found: ", specific_config_path)
  }

  # read + merge
  global_config   <- yaml::read_yaml(global_config_path)
  specific_config <- yaml::read_yaml(specific_config_path)
  config <- utils::modifyList(global_config, specific_config)

  # flatten
  if (!is.null(config$weights)) config <- c(config, config$weights)
  if (!is.null(config$paths))   config <- c(config, config$paths)

  config
}
