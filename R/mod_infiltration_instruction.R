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

Infiltration data should be copied and pasted from your datasheet into the downloaded data template. This application can accommodate infiltration measurements over time from multiple infiltration points if needed. Refer to the Methods tab for an illustration of how infiltration data should be structured. A user can submit infiltration data for one or more events in a single file, as long as timestamps and values are continuous and without skipping records.

- Download infiltration demo data
- Download infiltration template

**Data Requirements:**
The uploaded Excel spreadsheet must conform to the following requirements:
1. Infiltration must be reported in unit per hour. **Data must be in the same unit across all tabs.**
2. The timestamp should be in 24-hour format (mm/dd/yy hh:mm:ss).
3. Each tab must contain a column called datetime
4. You can put the data for multiple sensors in other columns, but you need to make sure the column names are unique, and they need to contain numeric data
5. **The column names and the tab names must not be changed from the template**.
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




