# Helper function: read all sheets of an excel file
read_excel_allsheets <- function(filename) {
  sheets <- readxl::excel_sheets(filename)
  setNames(lapply(sheets, function(sheet) readxl::read_excel(filename, sheet = sheet)), sheets)
}

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

    # Check number of rows
    if (nrow(user_data) > 45000) {
      errors <- c(errors, "The 'rainfall_data' sheet must not contain more than 40,000 rows.")
    }

    # Check for required columns
    if (ncol(user_data) != 2 || !all(c("datetime", "rain") %in% names(user_data))) {
      errors <- c(errors, "The 'rainfall_data' sheet must contain exactly two columns: 'datetime' and 'rain'.")
    } else {
      datetime_col <- user_data$datetime
      rain_col <- user_data$rain

      # Check that "datetime" is of datetime type
      if (!inherits(datetime_col, "Date") && !inherits(datetime_col, "POSIXct")) {
        errors <- c(errors, "The 'datetime' column must be of datetime type (Date or POSIXct).")
      }

      # Check for duplicates in "datetime"
      if (any(duplicated(datetime_col))) {
        errors <- c(errors, "The 'datetime' column must not contain duplicate values.")
      }

      # Check that "rain" is numeric
      if (!is.numeric(rain_col)) {
        errors <- c(errors, "The 'rain' column must be numeric.")
      }

      # Check for missing values in "rain"
      if (any(is.na(rain_col))) {
        errors <- c(errors, "The 'rain' column must not contain missing values (NA).")
      }
    }
  }

  return(errors)
}


# Helper function: Validate the flow file and return a list of error messages.
validate_flow_file <- function(file_path) {
  errors <- list()

  # Expected full sheet names (including instructions)
  expected_all_sheets <- c("Instructions", "inflow1", "inflow2", "outflow", "bypass")

  # Expected data sheet names (ignore "Instructions" for some checks)
  expected_data_sheets <- c("inflow1", "inflow2", "outflow", "bypass")

  # Get all sheet names from the file.
  sheets <- readxl::excel_sheets(file_path)

  # Check that there are exactly 5 sheets with the expected names
  if (!setequal(sheets, expected_all_sheets) || length(sheets) != 5) {
    errors <- c(
      errors,
      paste0("The uploaded Excel file must contain exactly these 5 sheets with exact names: ",
             paste(expected_all_sheets, collapse = ", "), ". Found: ",
             paste(sheets, collapse = ", "), ".")
    )
  }

  # Flag to check if at least one sheet has data
  has_data <- FALSE

  # Validate each expected data sheet.
  for (sheet in expected_data_sheets) {
    if (sheet %in% sheets) {
      df <- readxl::read_excel(file_path, sheet = sheet)

      if (nrow(df) > 45000) {
        errors <- c(errors, sprintf("Sheet '%s' exceeds the maximum allowed number of rows (45,000). Found %d rows.", sheet, nrow(df)))
        next
      }

      # Check that there are exactly 2 columns: "datetime" and "flow"
      if (ncol(df) != 2 || !all(c("datetime", "flow") %in% names(df))) {
        errors <- c(errors, paste0("Sheet '", sheet, "' must contain exactly two columns: 'datetime' and 'flow'."))
      } else {
        # If the sheet is non-empty, then check the column datatypes.
        if (nrow(df) > 0) {
          has_data <- TRUE  # Mark that at least one sheet has data

          # Check that datetime column is of correct type
          if (!inherits(df$datetime, "POSIXct") && !inherits(df$datetime, "Date")) {
            errors <- c(errors, sprintf("In sheet '%s', the 'datetime' column must be of datetime type. Make sure you have correct datatype/data not NA.", sheet))
          }

          # Check that flow column is numeric
          if (!is.numeric(df$flow)) {
            errors <- c(errors, sprintf("In sheet '%s', the 'flow' column must be numeric. Make sure you have correct datatype/data not NA.", sheet))
          }

          # Check for missing values in datetime and flow columns
          if (any(is.na(df$datetime))) {
            errors <- c(errors, sprintf("Sheet '%s' has missing values in the 'datetime' column.", sheet))
          }
          if (any(is.na(df$flow))) {
            errors <- c(errors, sprintf("Sheet '%s' has missing values in the 'flow' column.", sheet))
          }
        }
      }
    }
  }

  # Add error if none of the sheets have data
  if (!has_data) {
    errors <- c(errors, "At least one of the sheets must contain data (i.e., not be empty).")
  }

  return(errors)
}



# Helper function: Validate the infiltration file and return a list of error messages.
validate_infiltration_file <- function(file_path) {
  sheets <- readxl::excel_sheets(file_path)
  sheets <- sheets[!sheets %in% "Instructions"]
  error_report <- list()
  valid_data <- list()

  for (sheet in sheets) {
    data_df <- readxl::read_excel(file_path, sheet = sheet, .name_repair = "minimal")
    errors <- c()

    if (nrow(data_df) > 45000) {
      errors <- c(errors, paste("Sheet", sheet, ": Exceeds the maximum allowed number of rows (45,000). Found", nrow(data_df), "rows."))
      error_report[[sheet]] <- errors
      next
    }

    # Check for "datetime" column
    if (!"datetime" %in% names(data_df)) {
      errors <- c(errors, paste("Sheet", sheet, ": Missing column 'datetime'."))
      error_report[[sheet]] <- errors
      next
    }

    # Coerce to character in case it's not
    datetime_vals <- as.character(data_df$datetime)

    # Check for missing values in datetime column
    if (any(is.na(datetime_vals) | trimws(datetime_vals) == "")) {
      errors <- c(errors, paste("Sheet", sheet, ": 'datetime' column has missing or blank values."))
    }

    # Try parsing datetime with tryCatch to prevent crashes
    parsed_time <- tryCatch({
      suppressWarnings(as.POSIXct(datetime_vals, tz = "UTC",
                                  tryFormats = c("%Y-%m-%d %H:%M:%S",
                                                 "%Y-%m-%d",
                                                 "%m/%d/%Y %H:%M",
                                                 "%m/%d/%Y")))
    }, error = function(e) {
      rep(NA, length(datetime_vals))
    })

    # If parsing failed entirely, return error and skip further validation
    if (all(is.na(parsed_time))) {
      errors <- c(errors, paste("Sheet", sheet, ": 'datetime' column is not in a recognizable datetime format. Make sure to check ALL values, the format is mm/dd/yy"))
      error_report[[sheet]] <- errors
      next
    } else {
      data_df$datetime <- parsed_time
    }

    # Check for duplicate column names
    duplicate_names <- names(data_df)[duplicated(names(data_df))]
    if (length(duplicate_names) > 0) {
      for (name in duplicate_names) {
        errors <- c(errors, paste("Sheet", sheet, ": Duplicate column name found:", name))
      }
    }

    # Validate all other columns (must be numeric and complete)
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

    # Save either the errors or the valid sheet
    if (length(errors) > 0) {
      error_report[[sheet]] <- errors
    } else {
      valid_data[[sheet]] <- data_df
    }
  }

  return(list(errors = error_report, valid_data = valid_data))
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
