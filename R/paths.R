#' Path utilities for SSS projects
#'
#' This file defines helpers for two bases:
#' - Data base (Google Drive): SSS_DATA_BASE (fallback: SSS_BASE)
#' - Code base (local clones): SSS_CODE_BASE
#'
#' @name sss_paths
#' @noRd
NULL


# ---- internal helpers ------------------------------------------------------

# variadic coalescer: return first non-empty character string
`%||%` <- function(...) {
  xs <- list(...)
  for (x in xs) {
    if (!is.null(x) && is.character(x) && nzchar(x)) return(x)
  }
  ""
}

normalize_abs <- function(p, mustWork = FALSE) {
  normalizePath(p, winslash = "/", mustWork = mustWork)
}

# Find nearest git repo root from a starting directory
sss_find_git_root <- function(start = getwd(), max_up = 8) {
  cur <- normalize_abs(start, mustWork = TRUE)
  for (i in seq_len(max_up)) {
    if (dir.exists(file.path(cur, ".git"))) return(cur)
    parent <- dirname(cur); if (parent == cur) break
    cur <- parent
  }
  ""
}

# ---- DATA base (Google Drive) ---------------------------------------------

#' Resolve the SSS *data* base (Google Drive root, usually `sss_production`)
#'
#' Resolution order:
#' 1. explicit `base` argument
#' 2. `getOption('sss.data')`
#' 3. `Sys.getenv('SSS_DATA_BASE')`
#' 4. `Sys.getenv('SSS_BASE')` (fallback for older setups)
#'
#' @param base Optional override of the base path.
#' @param must_exist If `TRUE`, error if the path doesn't exist.
#' @return Absolute, normalized path to the data base.
#' @export
sss_data_base <- function(base = NULL, must_exist = TRUE) {
  root <- `%||%`(
    base,
    getOption("sss.data", ""),
    Sys.getenv("SSS_DATA_BASE", unset = ""),
    Sys.getenv("SSS_BASE", unset = "")
  )
  if (!nzchar(root)) {
    stop(
      "Data base path is not set.\n",
      "Set SSS_DATA_BASE in ~/.Renviron to your Google Drive sss_production root.",
      call. = FALSE
    )
  }
  if (isTRUE(must_exist) && !dir.exists(root)) {
    stop("SSS_DATA_BASE points to a non-existent folder: ", root, call. = FALSE)
  }
  normalize_abs(root, mustWork = isTRUE(must_exist))
}

#' Build a path under the *data* base
#'
#' @inheritParams sss_data_base
#' @param ... Path components below the data base
#'   (e.g., `"data","2026","raw","Taxes","file.csv"`).
#' @return Absolute, normalized path.
#' @export
sss_data_path <- function(..., base = NULL, must_exist = FALSE) {
  p <- file.path(sss_data_base(base = base, must_exist = TRUE), ...)
  p <- normalize_abs(p, mustWork = FALSE)
  if (isTRUE(must_exist) && !file.exists(p)) stop("Not found: ", p, call. = FALSE)
  p
}

#' Quick environment check for the *data* base
#'
#' Ensures the expected `data/` folder exists under `SSS_DATA_BASE`.
#'
#' @param expect_data_dir Folder expected under the data base (default: `"data"`).
#' @return (Invisibly) `TRUE` if OK; otherwise errors with guidance.
#' @export
sss_check_environment <- function(expect_data_dir = "data") {
  root <- sss_data_base()
  if (!dir.exists(file.path(root, expect_data_dir))) {
    stop(
      "SSS_DATA_BASE is: ", root, "\n",
      "But '", expect_data_dir, "' was not found there.\n",
      "Ensure Drive is synced and SSS_DATA_BASE points to your sss_production root.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# ---- CODE base (local clones) ---------------------------------------------

#' Resolve the SSS *code* base (parent folder containing your local repos)
#'
#' Resolution order:
#' 1. explicit `base`
#' 2. `getOption('sss.code')`
#' 3. `Sys.getenv('SSS_CODE_BASE')`
#'
#' It's OK if this is unset *when* `repo = NULL` is used with `sss_code_path()`
#' (auto-detect current repo).
#'
#' @inheritParams sss_data_base
#' @return Absolute, normalized path (or `""` if unset and allowed).
#' @export
sss_code_base <- function(base = NULL, must_exist = TRUE) {
  root <- `%||%`(
    base,
    getOption("sss.code", ""),
    Sys.getenv("SSS_CODE_BASE", unset = "")
  )
  if (!nzchar(root)) return("")  # fine if caller chooses autodetect
  if (isTRUE(must_exist) && !dir.exists(root)) {
    stop("SSS_CODE_BASE points to a non-existent folder: ", root, call. = FALSE)
  }
  normalize_abs(root, mustWork = isTRUE(must_exist))
}

#' Build a path under a code repo
#'
#' If `repo` is `NULL`, the function auto-detects the current repo by walking
#' upward from `getwd()` to find a `.git` directory.
#'
#' @param repo Name of the local repo folder (e.g., `"sss_production"`, `"sss_utils"`),
#'   or `NULL` to auto-detect the current repo.
#' @inheritParams sss_data_path
#' @return Absolute, normalized path.
#' @export
sss_code_path <- function(..., repo = NULL, base = NULL, must_exist = FALSE) {
  root <- ""
  if (is.null(repo)) {
    root <- sss_find_git_root()
    if (!nzchar(root)) {
      stop(
        "Could not auto-detect a git repo from the current working directory.\n",
        "Either run this from inside a repo, or provide `repo=` and set SSS_CODE_BASE.",
        call. = FALSE
      )
    }
  } else {
    code_base <- sss_code_base(base = base, must_exist = TRUE)
    if (!nzchar(code_base)) {
      stop(
        "SSS_CODE_BASE is not set. Add e.g. SSS_CODE_BASE=\"C:/Users/You/dev\" to ~/.Renviron,\n",
        "so that your repos live at C:/Users/You/dev/<repo>.",
        call. = FALSE
      )
    }
    root <- file.path(code_base, repo)
    if (!dir.exists(root)) {
      stop("Repo folder not found at: ", normalize_abs(root), call. = FALSE)
    }
  }
  p <- normalize_abs(file.path(root, ...), mustWork = FALSE)
  if (isTRUE(must_exist) && !file.exists(p)) stop("Not found: ", p, call. = FALSE)
  p
}

# ---- Back-compat aliases (soft-deprecated) --------------------------------

#' @export
sss_base <- function(base = NULL, must_exist = TRUE) {
  .Deprecated("sss_data_base", package = "sssUtils",
              msg = "sss_base() is deprecated; use sss_data_base(). SSS_BASE remains a fallback.")
  root <- `%||%`(base, Sys.getenv("SSS_BASE", unset = ""))
  if (nzchar(root)) return(normalize_abs(root, mustWork = isTRUE(must_exist)))
  sss_data_base(must_exist = must_exist)
}

#' @export
sss_path <- function(..., base = NULL, must_exist = FALSE) {
  .Deprecated("sss_data_path", package = "sssUtils",
              msg = "sss_path() is deprecated; use sss_data_path().")
  sss_data_path(..., base = base, must_exist = must_exist)
}

#' @export
get_base_path <- function(..., base = NULL, must_exist = FALSE) {
  .Deprecated("sss_data_path", package = "sssUtils",
              msg = "get_base_path() is deprecated; use sss_data_path().")
  sss_data_path(..., base = base, must_exist = must_exist)
}
