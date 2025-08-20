#' Load SSS Configuration from YAML
#'
#' Loads a merged configuration from both the global and state YAML files.
#' Promotes nested config fields like `weights` and `paths` to top-level for convenience.
#'
#' @param state_abbr Two-letter state abbreviation, e.g., "AL"
#' @param base_path Base path for the SSS project (optional, uses `get_base_path()` if NULL)
#'
#' @return A named list containing merged config values
#' @export
load_sss_config <- function(state_abbr, base_path = NULL) {
  if (is.null(base_path)) {
    base_path <- get_base_path()
  }

  # Load global config
  global_config_path <- file.path(base_path, "sss_production/config/config_global.yml")
  global_config <- yaml::read_yaml(global_config_path)

  # Load state config
  state_config_path <- file.path(base_path, "sss_production/config/states", paste0("config_", state_abbr, ".yml"))
  if (!file.exists(state_config_path)) {
    stop("State config file not found: ", state_config_path)
  }
  state_config <- yaml::read_yaml(state_config_path)

  # Merge with state config taking precedence
  config <- modifyList(global_config, state_config)

  # Promote weights and paths (flattening one level)
  if (!is.null(config$weights)) {
    config <- c(config, config$weights)
  }
  if (!is.null(config$paths)) {
    config <- c(config, config$paths)
  }

  return(config)
}
