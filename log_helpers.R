log_metadata <- function(module,
                         type = c("input", "output"),
                         file_path,
                         state_abbrev,
                         SSS_year,
                         notes = "",
                         log_dir = "logs") {
  type <- match.arg(type)

  # Ensure log directory exists
  module_log_dir <- file.path(log_dir, module)
  if (!dir.exists(module_log_dir)) dir.create(module_log_dir, recursive = TRUE)

  # Construct log entry
  log_entry <- data.frame(
    timestamp = Sys.time(),
    module = module,
    type = type,
    state = state_abbrev,
    year = SSS_year,
    file = normalizePath(file_path, winslash = "/", mustWork = FALSE),
    filename = basename(file_path),
    notes = notes,
    stringsAsFactors = FALSE
  )

  # Construct log file path for this module
  log_file <- file.path(module_log_dir, paste0("log_", module, ".csv"))

  # Write or append log entry
  if (!file.exists(log_file)) {
    write.csv(log_entry, log_file, row.names = FALSE)
  } else {
    write.table(log_entry, log_file, sep = ",", row.names = FALSE,
                col.names = FALSE, append = TRUE)
  }

  invisible(log_entry)
}
