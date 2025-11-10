#' Load SSS configuration (global + state)
#'
#' Tries to load config from the current repo first (auto-detected),
#' and if that repo doesn't have a config/ folder, it falls back to
#' the sss_production repo under SSS_CODE_BASE.
#'
#' @param state_abbr Two-letter state code, e.g., "IA".
#' @param base_path Optional explicit repo root override. If provided, it wins.
#' @param repo Optional repo name (e.g., "sss_production") if you want to load
#'   configs from a different local repo than the current one. Ignored when
#'   `base_path` is supplied.
#' @return Named list of merged config values (state overrides global).
#' @export
load_sss_config <- function(state_abbr, base_path = NULL, repo = NULL) {
  # 1. resolve an initial root
  if (!is.null(base_path)) {
    root <- normalizePath(base_path, winslash = "/", mustWork = TRUE)
  } else {
    # if repo is given, go straight there; otherwise auto-detect current repo
    root <- if (is.null(repo)) {
      sss_code_path(must_exist = TRUE)
    } else {
      sss_code_path(repo = repo, must_exist = TRUE)
    }
  }

  # expected paths in that root
  global_config_path <- file.path(root, "config", "config_global.yml")
  state_config_path  <- file.path(root, "config", "states", paste0("config_", state_abbr, ".yml"))

  # 2. if we didn't find the global config, and the user did NOT explicitly
  #    tell us base_path or repo, try sss_production as a fallback
  if (!file.exists(global_config_path) && is.null(base_path) && is.null(repo)) {
    fallback_root <- sss_code_path(repo = "sss_production", must_exist = TRUE)
    global_config_path <- file.path(fallback_root, "config", "config_global.yml")
    state_config_path  <- file.path(fallback_root, "config", "states", paste0("config_", state_abbr, ".yml"))
  }

  if (!file.exists(global_config_path)) {
    stop("Global config not found: ", global_config_path)
  }
  if (!file.exists(state_config_path)) {
    stop("State config not found: ", state_config_path)
  }

  global_config <- yaml::read_yaml(global_config_path)
  state_config  <- yaml::read_yaml(state_config_path)

  # merge, letting state win
  config <- utils::modifyList(global_config, state_config)

  # flatten common sub-lists for convenience, same as before
  if (!is.null(config$weights)) config <- c(config, config$weights)
  if (!is.null(config$paths))   config <- c(config, config$paths)

  config
}
