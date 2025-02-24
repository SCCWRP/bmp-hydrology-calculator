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
Using this Application
It is required that the column names and tab names in the templates remain unchanged.

**Rainfall Analysis:**
It is advised to copy and paste the rainfall data, whether it is in the form of 1-minute or time-of-tips data, into the downloaded template. It is important to note that the rain gauge values must be entered into the designated rain column within the template. A user can submit a continuous rainfall record without skipping any timestamps, containing multiple events in a single file.

- Download rainfall template
- Download 1-min demo data
- Download time of tips demo data

**Data Requirements**
The uploaded Excel spreadsheet must conform to the following requirements:
1. Flow must be reported in units of L/s, gpm, or cfs.
2. The timestamp should be in 24-hour format (mm/dd/yy hh:mm:ss).
3. Each tab must contain exactly two columns, one for the sample timestamps data and one for the associated values.
4. The column names and the tab names must not be changed from the template.
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
        shinyWidgets::downloadBttn(ns("download_rainfall_template"), "Download rainfall template")
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
        file.copy("inst/extdata/rainfall_template.xlsx", file, overwrite = TRUE)
      }
    )

  })
}

