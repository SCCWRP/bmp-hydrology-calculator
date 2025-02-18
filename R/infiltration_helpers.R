#' helpers
#'
#' @description utils function for infiltration analysis
#'
#' @return The return value, if any, from executing the utility.
#'
#' @import readxl ggplot2 zoo minpack.lm lubridate httr

exponential_decay <- function(t, y0, k) {
  y0 * exp(-k * t)
}

main <- function() {
  # Set parameter values
  REGRESSION_WINDOW <- 720
  SMOOTHING_WINDOW <- 5
  REGRESSION_THRESHOLD <- 0.999

  # Define the file path and sheet name
  file_path <- "inst/extdata/demo_infiltration_data.xlsx"
  sheet_name <- "12.14 PA"

  # Read the Excel file
  df <- read_excel(file_path, sheet = sheet_name)

  # Convert the datetime column to ISO8601 strings (assuming the column is named 'datetime')
  df$datetime <- as.character(as.POSIXct(df$datetime, tz = "UTC"))

  # Prepare the payload for the API call.
  # The API expects a JSON object with a "data" key containing the time series records,
  # plus the three parameter overrides.
  payload <- list(
    data = df,
    SMOOTHING_WINDOW = SMOOTHING_WINDOW,
    REGRESSION_WINDOW = REGRESSION_WINDOW,
    REGRESSION_THRESHOLD = REGRESSION_THRESHOLD
  )

  # Convert the payload to a JSON string.
  payload_json <- jsonlite::toJSON(payload, auto_unbox = TRUE, POSIXt = "ISO8601")

  # Define the API endpoint URL
  url <- "https://nexus.sccwrp.org/bmp_hydrology/api/infiltration"

  # Make the POST request
  response <- POST(url,
                   body = payload_json,
                   encode = "json",
                   content_type_json())

  # Check the response status
  if (status_code(response) != 200) {
    stop("API request failed with status: ", status_code(response),
         "\nResponse: ", content(response, "text"))
  }

  # Parse the JSON response
  result <- content(response, "parsed")

  # Extract the returned parameters (if needed)
  dataframe_returned  <- result$dataframe
  best_windows        <- result$best_windows
  best_params_list    <- result$best_params_list
  best_r_squared_list <- result$best_r_squared_list

  # Clean and flatten the returned dataframe
  local_df <- jsonlite::fromJSON(jsonlite::toJSON(result$dataframe), flatten = TRUE)
  local_df$datetime <- as.POSIXct(as.character(local_df$datetime),
                                  format = "%Y-%m-%dT%H:%M:%S")
  # Optionally reformat for display (if desired)
  local_df$datetime <- format(local_df$datetime, "%Y-%m-%d %H:%M:%S")

  # Clean up the column names by removing trailing suffixes
  names(local_df) <- sub("\\..*$", "", names(local_df))

  print(local_df)

  # Convert smoothed columns to numeric
  local_df$smooth_A <- as.numeric(as.character(local_df$smooth_A))
  local_df$smooth_B <- as.numeric(as.character(local_df$smooth_B))
  local_df$smooth_C <- as.numeric(as.character(local_df$smooth_C))

  # Reshape original data (smoothed columns) to long format for plotting
  df_long <- local_df %>%
    tidyr::pivot_longer(
      cols = c("Piez A", "Piez B", "Piez C"),
      names_to = "piezometer",
      values_to = "depth"
    ) %>%
    dplyr::mutate(depth = as.numeric(as.character(depth)))
  df_long$datetime <- as.POSIXct(df_long$datetime, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
  print(df_long$datetime)



  # ----- New Plotting Code -----
  # If your API also returns calculated values (extended_time, best_fit_line, etc.)
  # under a key "calc_results", we can extract them here:
  calc_results <- result$calc_results

  # Build a data frame for the best-fit line data for each piezometer.
  # Initialize best_fit_df with the expected columns:
  best_fit_df <- data.frame(
    datetime = as.POSIXct(character()),
    best_fit = numeric(),
    piezometer = character(),
    stringsAsFactors = FALSE
  )

  for(piez in names(calc_results)) {
    if(!is.null(calc_results[[piez]])) {

      # Convert extended_time to a character vector and then to POSIXct
      ext_time <- unlist(calc_results[[piez]]$extended_time)
      # Convert to POSIXct
      ext_time <- as.POSIXct(ext_time, format = "%a, %d %b %Y %H:%M:%S GMT", tz = "GMT")

      best_fit_line <- calc_results[[piez]]$best_fit_line


      temp_df <- data.frame(
        datetime = ext_time,
        best_fit = as.numeric(unlist(best_fit_line)),  # Convert to a numeric vector
        piezometer = piez,
        stringsAsFactors = FALSE
      )


      print(names(temp_df))
      best_fit_df <- rbind(best_fit_df, temp_df)
    }
  }

  print(best_fit_df)
  write.csv(best_fit_df, "best_fit_df.csv")
  # Create the plot using ggplot2:
  p <- ggplot() +
    # Plot the original smoothed data as solid lines
    geom_line(data = df_long,
              aes(x = datetime,
                  y = depth,
                  color = piezometer),
              size = 1) +
    # If best-fit results are available, overlay them as dashed lines
    { if (nrow(best_fit_df) > 0)
      geom_line(data = best_fit_df,
                aes(x = datetime, y = best_fit, color = piezometer),
                linetype = "dashed", size = 1)
      else NULL } +
    labs(title = "Exponential Decay Model Fit to Smoothed Depth Data",
         x = "Datetime", y = "Depth (cm)",
         color = "Piezometer") +
    theme_minimal()

  print(p)

  # Optionally, print calculated metrics for each piezometer if available
  if (!is.null(calc_results)) {
    for (piez in names(calc_results)) {
      if (!is.null(calc_results[[piez]])) {
        cat(sprintf("For %s:\n", piez))
        cat(sprintf("  Infiltration rate: %.2f cm/hr\n", calc_results[[piez]]$infiltration_rate))
        cat(sprintf("  Duration: %.0f hrs\n", calc_results[[piez]]$delta_x))
        cat(sprintf("  Average depth: %.2f cm\n\n", calc_results[[piez]]$y_average))
      }
    }
  }
}
