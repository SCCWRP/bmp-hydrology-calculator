

plot_excel_data <- function(path) {
  library(readxl)
  library(ggplot2)
  library(scales)
  # Read the Excel file
  data <- read_excel(path)

  # Check if necessary columns exist
  if (!all(c("datetime", "value") %in% colnames(data))) {
    stop("The Excel file must contain 'datetime' and 'value' columns.")
  }

  # Convert datetime column to POSIXct (for proper time-series plotting)
  data$datetime <- as.POSIXct(data$datetime, format="%Y-%m-%d %H:%M:%S", tz="UTC")

  # Define breaks every 6 hours for the x-axis
  x_breaks <- seq(min(data$datetime, na.rm = TRUE), max(data$datetime, na.rm = TRUE), by = "5 min")

  # Create the plot
  p <- ggplot(data, aes(x = datetime, y = value)) +
    geom_point(color = "red") +
    scale_x_datetime(breaks = x_breaks, labels = date_format("%Y-%m-%d %H:%M")) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 14),  # Increase x-axis text size
      axis.text.y = element_text(size = 14),  # Increase y-axis text size
      axis.title.x = element_text(size = 16), # Increase x-axis title size
      axis.title.y = element_text(size = 16)  # Increase y-axis title size
    ) +
    labs(x = "Datetime",
         y = "Value")

  return(p)
}

# Example usage:
# p <- plot_excel_data("path/to/your/excel_file.xlsx")
# print(p)
