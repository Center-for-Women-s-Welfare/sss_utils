#' Load SSS configuration (global + state)
#'
#' Reads config files from the *code repo* (not Google Drive).
#' By default it auto-detects the current repo root via `sss_code_path()`.
#'
#' @param state_abbr Two-letter state code, e.g., "IA".
#' @param base_path Optional explicit repo root override. If provided, it wins.
#' @param repo Optional repo name (e.g., "sss_production") if you want to load
#'   configs from a different local repo than the current one. Ignored when
#'   `base_path` is supplied.
#' @return Named list of merged config values (state overrides global).
#' @export
load_sss_config <- function(state_abbr, base_path = NULL, repo = NULL) {
  # Resolve the repo root (code base), not the data base.
  if (!is.null(base_path)) {
    root <- normalizePath(base_path, winslash = "/", mustWork = TRUE)
  } else {
    root <- if (is.null(repo)) {
      # auto-detect current repo (walk up to .git)
      sss_code_path(must_exist = TRUE)
    } else {
      # jump to a specific local repo under SSS_CODE_BASE
      sss_code_path(repo = repo, must_exist = TRUE)
    }
  }

  # Config files live under <repo>/config/...
  global_config_path <- file.path(root, "config", "config_global.yml")
  state_config_path  <- file.path(root, "config", "states", paste0("config_", state_abbr, ".yml"))

  if (!file.exists(global_config_path)) stop("Global config not found: ", global_config_path)
  if (!file.exists(state_config_path))  stop("State config not found: ",  state_config_path)

  global_config <- yaml::read_yaml(global_config_path)
  state_config  <- yaml::read_yaml(state_config_path)

  # Merge (state wins), then flatten common sub-lists for convenience
  config <- utils::modifyList(global_config, state_config)
  if (!is.null(config$weights)) config <- c(config, config$weights)
  if (!is.null(config$paths))   config <- c(config, config$paths)

  config
}
