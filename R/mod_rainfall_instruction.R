#' Rainfall Instruction UI Function
#'
#' @description A Shiny module for displaying instructions for Rainfall Analysis.
#'
#' @param id Internal parameter for {shiny} namespace.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList HTML
#'
# Example snippet for bslib::card_body
my_markdown_text <- "
Hyetograph data for up to approximately one continuous month (including multiple rain events) may be uploaded in a single file. Larger/longer records must be parsed for separate uploads. The calculator determines statistics for individual events.

It is advised to copy and paste the rainfall data, whether it is in the form of 1-minute or time-of-tips data, into the downloaded template. It is important to note that the rain gauge values must be entered into the designated rain column within the template.

- Download 1-min demo data
- Download time of tips demo data
- Download rainfall template

**Data Requirements:**
There are two worksheets in the template: an instruction tab (Instructions) and a data tab (rainfall_data). The uploaded Excel spreadsheet must conform to the following requirements:

1. The data tab must contain exactly two columns, one for the measurement timestamps data and one for the associated values.
2. The timestamp should be in 24-hour format (mm/dd/yy hh:mm:ss).
3. **The column names and the tab names must not be changed from the template.**.
"


mod_rainfall_instruction_ui <- function(id) {
  ns <- NS(id)
  bslib::card(
    bslib::card_body(
      HTML(
        markdown::markdownToHTML(
          text = my_markdown_text,
          fragment.only = TRUE
        )
      )
    ),
    bslib::card_footer(
      bslib::layout_columns(
        col_widths = c(4, 4, 4),
        shinyWidgets::downloadBttn(ns("download_demo_1min_rainfall"), "Download 1-min demo data"),
        shinyWidgets::downloadBttn(ns("download_demo_timeoftips_rainfall"), "Download time of tips demo data"),
        shinyWidgets::downloadBttn(ns("download_template_rainfall"), "Download rainfall template")
      )
    )
  )
}

mod_rainfall_instruction_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    output$download_demo_1min_rainfall <- downloadHandler(
      filename = "demo_rainfall_1min_data.xlsx",
      content = function(file) {
        file.copy("inst/extdata/demo_rainfall_1min_data.xlsx", file, overwrite = TRUE)
      }
    )

    output$download_demo_timeoftips_rainfall <- downloadHandler(
      filename = "demo_rainfall_timeoftips_data.xlsx",
      content = function(file) {
        file.copy("inst/extdata/demo_rainfall_timeoftips_data.xlsx", file, overwrite = TRUE)
      }
    )

    output$download_template_rainfall <- downloadHandler(
      filename = "rainfall_template.xlsx",
      content = function(file) {
        file.copy("inst/extdata/rainfall_template.xlsx", file)
      }
    )

  })
}

