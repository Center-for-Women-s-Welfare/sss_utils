#' Get Base Path
#'
#' Determines the base path based on the operating system.
#' Validates that the base path exists.
#'
#' @return A string representing the base path for the current operating system.
#' @export
get_base_path <- function() {
  os <- Sys.info()["sysname"]

  if (os == "Darwin") {  # macOS
    base_path <- "/Users/anniekucklick/Library/CloudStorage/GoogleDrive-akuckl@uw.edu/Shared drives/CWW Team Drive/SSS/"
  } else if (os == "Windows") {
    base_path <- "G:/Shared drives/CWW Team Drive/SSS/"
  } else {
    stop("Unsupported OS")
  }

  # Remove trailing slash if present
  base_path <- sub("/$", "", base_path)

  # Validate that the path exists
  if (!dir.exists(base_path)) {
    stop(paste("Base path does not exist:", base_path, "\nPlease check your configuration."))
  }

  # Optional: print for debugging
  print(paste("Base path is:", base_path))

  return(base_path)
}
