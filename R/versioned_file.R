#' Generate Versioned File Name
#'
#' Checks if a file with a given base name exists in the specified directory.
#' If it does, generates a versioned filename in the format `base_name_vX.extension`.
#'
#' @param directory A string. The directory where the file should be created.
#' @param base_name A string. The base name for the file (without versioning or extension).
#' @param extension A string. The file extension (e.g., "xlsx", "csv", "txt"). Default is "xlsx".
#' @return A string. The full path to a new versioned file that does not already exist.
#' @export
generate_file_name <- function(directory, base_name, extension = "xlsx") {
  # Remove any existing extension from the base name if present
  base_name <- sub(paste0("\\.", extension, "$"), "", base_name)

  version <- 1
  repeat {
    # Construct the file name with version and extension
    file_name <- paste0(base_name, "_v", version, ".", extension)
    full_file_path <- file.path(directory, file_name)
    if (!file.exists(full_file_path)) {
      return(full_file_path)  # Return the path of the new versioned file
    }
    version <- version + 1  # Increment version if the file already exists
  }
}
