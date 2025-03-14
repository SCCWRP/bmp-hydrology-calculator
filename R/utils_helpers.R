# Helper function: Validate the rainfall file and return a list of error messages
validate_rainfall_file <- function(file_path) {
  errors <- list()

  # Check that the file has exactly two sheets: "Instructions" and "rainfall_data"
  sheets <- readxl::excel_sheets(file_path)
  if (length(sheets) != 2 || !all(c("Instructions", "rainfall_data") %in% sheets)) {
    errors <- c(errors, "The uploaded Excel file must have exactly two sheets: 'Instructions' and 'rainfall_data'.")
  } else {
    # Read the "rainfall_data" sheet
    user_data <- readxl::read_excel(file_path, sheet = "rainfall_data")

    # Check that it contains exactly 2 columns: "datetime" and "rain"
    if (ncol(user_data) != 2 || !all(c("datetime", "rain") %in% names(user_data))) {
      errors <- c(errors, "The 'rainfall_data' sheet must contain exactly two columns: 'datetime' and 'rain'.")
    } else {
      # Check that "datetime" is a datetime type (POSIXct or Date)
      if (!inherits(user_data$datetime, "POSIXct") && !inherits(user_data$datetime, "Date")) {
        errors <- c(errors, "The 'datetime' column must be of datetime type.")
      }
      # Check that "rain" is numeric
      if (!is.numeric(user_data$rain)) {
        errors <- c(errors, "The 'rain' column must be numeric.")
      }
    }
  }

  return(errors)
}


# Helper function: Validate the flow file and return a list of error messages.
# Helper function: Validate the flow file and return a list of error messages.
validate_flow_file <- function(file_path) {
  errors <- list()

  # Expected data sheet names (ignore "Instructions")
  expected_data_sheets <- c("inflow1", "inflow2", "outflow", "bypass")

  # Get all sheet names from the file.
  sheets <- readxl::excel_sheets(file_path)

  # Check that all expected data sheets are present.
  if (!all(expected_data_sheets %in% sheets)) {
    errors <- c(
      errors,
      paste0("The uploaded Excel file must contain the following sheets: ",
             paste(expected_data_sheets, collapse = ", "), ".")
    )
  }

  # Validate each expected data sheet.
  for (sheet in expected_data_sheets) {
    if (sheet %in% sheets) {
      df <- readxl::read_excel(file_path, sheet = sheet)

      # Check that there are exactly 2 columns: "datetime" and "flow"
      if (ncol(df) != 2 || !all(c("datetime", "flow") %in% names(df))) {
        errors <- c(errors, paste0("Sheet '", sheet, "' must contain exactly two columns: 'datetime' and 'flow'."))
      } else {
        # For inflow2 and bypass, allow an empty dataset but still require the correct column names.
        # If the sheet is non-empty, then check the column datatypes.
        if (!(sheet %in% c("inflow2", "bypass") && nrow(df) == 0)) {
          # Check that "datetime" is of a datetime type.
          if (!inherits(df$datetime, "POSIXct") && !inherits(df$datetime, "Date")) {
            errors <- c(errors, paste0("In sheet '", sheet, "', the 'datetime' column must be of datetime type."))
          }
          # Check that "flow" is numeric.
          if (!is.numeric(df$flow)) {
            errors <- c(errors, paste0("In sheet '", sheet, "', the 'flow' column must be numeric."))
          }
        }
      }
    }
  }

  return(errors)
}




read_excel_allsheets <- function(filename) {
  sheets <- readxl::excel_sheets(filename)
  setNames(lapply(sheets, function(sheet) readxl::read_excel(filename, sheet = sheet)), sheets)
}


validate_infiltration_file <- function(file_path) {
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

# Helper to handle fatal errors: displays an error modal and stops the app.
handleFatalError <- function(errorMessage) {
  showModal(modalDialog(
    title = "Error",
    errorMessage,
    easyClose = FALSE,
    footer = modalButton("Close")
  ))

}




