#' Infiltration Instruction UI Function
#'
#' @description A Shiny module for displaying instructions for Infiltration Analysis.
#'
#' @param id Internal parameter for {shiny} namespace.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList HTML
mod_infiltration_instruction_ui <- function(id) {
  ns <- NS(id)

  # Infiltration-specific markdown text
  infiltration_markdown_text <- "

The Infiltration Analysis calculator estimates the average infiltration rate across the soil-water interface for a given storm event by analyzing changes in ponding depth. Once storm runoff inflow ceases, the decline in ponding depth due to infiltration appears as the recession limb on a timeseries plot. This portion of the curve can be fitted to a characteristic curve to determine the infiltration rate.

To use the Infiltration Analysis calculator, ponding depth data should be copied from your datasheet and pasted into the downloaded data template. Users may submit depth data from multiple sensors (e.g., pressure transducers, piezometers, etc.) for the same storm event within a single tab of the file. Additionally, data from multiple storm events can be included in a single file by using separate tabs to delineate each event.

- Download infiltration demo data
- Download infiltration template

**Data Requirements:**
The data sheet contains at least 2 worksheets: instructions and an additional worksheet for each individual infiltration event. The uploaded Excel spreadsheet must conform to the following requirements:
1. Timestamps should be in 24-hour format (mm/dd/yy hh:mm:ss).
2. For each infiltration event, the first column contains the datetimes of each water depth measurement. Enter water level data on adjacent columns from one or more sensors that share the same timestamp. Label each column with a unique name (e.g. the sensor identifier). Depth should be recorded as the numeric value only; the unit of measurement (mm, cm, in) can be selected from the dropdown menu.
3. Data from multiple storm events can also be submitted in a single file by using separate worksheets for each event. In this case, all data across tabs must use the same unit of measurement, and each tab must include a timestamp column labeled datetime. Worksheets can be renamed to reflect specific storm events.
"

  bslib::card(
    bslib::card_body(
      HTML(
        markdown::markdownToHTML(
          text = infiltration_markdown_text,
          fragment.only = TRUE
        )
      )
    ),
    bslib::card_footer(
      bslib::layout_columns(
        col_widths = c(6, 6),
        shinyWidgets::downloadBttn(
          outputId = ns("download_demo_infiltration"),
          label = "Download demo data"
        ),
        shinyWidgets::downloadBttn(
          outputId = ns("download_template_infiltration"),
          label = "Download template"
        )
      )
    )
  )
}
#' @importFrom shiny NS tagList HTML
mod_infiltration_instruction_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$download_demo_infiltration <- downloadHandler(
        filename = "demo_infiltration_data.xlsx",
        content = function(file) {
          file.copy("inst/extdata/demo_infiltration_data.xlsx", file, overwrite = TRUE)
        }
      )
      output$download_template_infiltration <- downloadHandler(
        filename = "template_infiltration.xlsx",
        content = function(file) {
          file.copy("inst/extdata/infiltration_template.xlsx", file, overwrite = TRUE)
        }
      )
    }
  )
}
