#' Get the SSS base folder (Google Drive root)
#'
#' Resolution order:
#' 1) explicit `base=` argument
#' 2) getOption("sss.base")
#' 3) Sys.getenv("SSS_BASE")
#'
#' @param base Optional explicit root path (useful for tests/CI).
#' @param must_exist If TRUE, error if the resolved path doesn't exist.
#' @return A normalized absolute path to the SSS base directory.
#' @export
sss_base <- function(base = NULL, must_exist = TRUE) {
  root <- `%||%`(
    base,
    getOption("sss.base", ""),
    Sys.getenv("SSS_BASE", unset = "")
  )
  if (!nzchar(root)) {
    stop(
      "SSS base path is not set.\n",
      "Set SSS_BASE in your ~/.Renviron to the SSS folder in Google Drive.\n",
      "Example (Windows): SSS_BASE=\"C:/Users/YourName/Google Drive/Shared drives/CWW Team Drive/SSS\"\n",
      "Example (macOS):   SSS_BASE=\"~/Library/CloudStorage/GoogleDrive-yourEmail/Shared drives/CWW Team Drive/SSS\"",
      call. = FALSE
    )
  }
  if (isTRUE(must_exist) && !dir.exists(root)) {
    stop("SSS_BASE points to a folder that does not exist: ", root, call. = FALSE)
  }
  normalizePath(root, winslash = "/", mustWork = isTRUE(must_exist))
}

#' Build a path under the SSS base
#'
#' @param ... Path components below the base (e.g., "data","Taxes","2025","file.csv")
#' @param base Optional explicit base override (rare).
#' @param must_exist If TRUE, error if the final path does not exist.
#' @return A normalized absolute path.
#' @export
sss_path <- function(..., base = NULL, must_exist = FALSE) {
  p <- file.path(sss_base(base = base, must_exist = TRUE), ...)
  p_norm <- normalizePath(p, winslash = "/", mustWork = FALSE)
  if (isTRUE(must_exist) && !file.exists(p_norm)) {
    stop("File/folder not found: ", p_norm, call. = FALSE)
  }
  p_norm
}

#' Quick environment check for students
#' @return (invisibly) TRUE if OK; otherwise errors with guidance.
#' @export
sss_check_environment <- function() {
  root <- sss_base()
  if (!dir.exists(file.path(root, "data"))) {
    stop(
      "Your SSS_BASE is:\n  ", root,
      "\nBut there is no 'data' folder there.\n",
      "Set SSS_BASE to your sss_production root (the folder that contains 'data')."
    )
  }
  invisible(TRUE)
}


# Back-compat: soft-deprecate get_base_path() if your scripts already use it
#' @export
get_base_path <- function(..., base = NULL, must_exist = FALSE) {
  .Deprecated("sss_path", package = "sssUtils",
              msg = "get_base_path() is deprecated; use sss_path() going forward.")
  sss_path(..., base = base, must_exist = must_exist)
}

# internal helper
`%||%` <- function(x, y, z = NULL) {
  if (!is.null(x) && is.character(x) && nzchar(x)) return(x)
  if (!is.null(y) && is.character(y) && nzchar(y)) return(y)
  if (!is.null(z) && is.character(z) && nzchar(z)) return(z)
  ""
}
