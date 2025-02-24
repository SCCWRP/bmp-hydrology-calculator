# Helper function to validate all sheets in the file.
validate_file <- function(file_path) {
  sheets <- readxl::excel_sheets(file_path)
  sheets <- sheets[!sheets %in% "Instructions"]
  error_report <- list()
  valid_data <- list()

  for (sheet in sheets) {
    data_df <- readxl::read_excel(file_path, sheet = sheet, .name_repair = "minimal")
    errors <- c()

    # Validate "datetime" column.
    if (!"datetime" %in% names(data_df)) {
      errors <- c(errors, paste("Sheet", sheet, ": Missing column 'datetime'."))
    } else {
      if (any(is.na(data_df$datetime))) {
        errors <- c(errors, paste("Sheet", sheet, ": 'datetime' column has missing values."))
      }
      parsed_time <- as.POSIXct(data_df$datetime, tz = "UTC",
                                tryFormats = c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d"))
      if (all(is.na(parsed_time))) {
        errors <- c(errors, paste("Sheet", sheet, ": 'datetime' column is not in a valid timestamp format."))
      }
    }

    # Check for duplicate column names.
    duplicate_names <- names(data_df)[duplicated(names(data_df))]
    if (length(duplicate_names) > 0) {
      for (name in duplicate_names) {
        errors <- c(errors, paste("Sheet", sheet, ": Duplicate column name found:", name))
      }
    }

    # Validate that all other columns are numeric and have no missing values.
    other_cols <- setdiff(names(data_df), "datetime")
    for (col in other_cols) {
      if (!is.numeric(data_df[[col]])) {
        converted <- suppressWarnings(as.numeric(data_df[[col]]))
        if (all(is.na(converted))) {
          errors <- c(errors, paste("Sheet", sheet, ": Column", col, "must be numeric."))
        } else {
          data_df[[col]] <- converted
        }
      }
      if (any(is.na(data_df[[col]]))) {
        errors <- c(errors, paste("Sheet", sheet, ": Column", col, "must have no missing values."))
      }
    }

    if (length(errors) > 0) {
      error_report[[sheet]] <- errors
    } else {
      valid_data[[sheet]] <- data_df
    }
  }

  list(errors = error_report, valid_data = valid_data)
}
