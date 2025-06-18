#' Flow Instruction UI Function
#'
#' @description A Shiny module for displaying instructions for Flow Analysis.
#'
#' @param id Internal parameter for {shiny} namespace.
#'
#' @noRd
#'
# Flow-specific markdown text
flow_markdown_text <- "

Hydrograph data can be submitted for a single rain event at one time. Up to four flows in the same event can be accommodated for analysis. The available flow types are inflow 1, inflow 2, outflow, and bypass. A user does not need to submit all four types; refer to the Methods tab for an illustration of common BMP flow type configurations. If any of the data types are not applicable, leave the tab blank (as is).

- Download flow demo data
- Download flow template

**Data Requirements:**
There are 5 worksheets in the data template: instructions, inflow1, inflow2, outflow, bypass. The uploaded Excel spreadsheet must conform to the following requirements:
1. Each tab must contain exactly two columns, one for the measured timestamps data and one for the associated values.
2. The timestamp should be in 24-hour format (mm/dd/yy hh:mm:ss).
3. All tabs must use the same flow unit. Flow data must be in units of L/s, gpm, or cfs.
4. **Do not change the column names and the tab names from the template.**
"
#' @importFrom shiny NS tagList HTML
mod_flow_instruction_ui <- function(id) {
  ns <- NS(id)
  bslib::card(
    bslib::card_body(
      HTML(
        markdown::markdownToHTML(
          text = flow_markdown_text,
          fragment.only = TRUE
        )
      )
    ),
    bslib::card_footer(
      bslib::layout_columns(
        col_widths = c(6, 6),
        shinyWidgets::downloadBttn(ns("download_demo_flow"), "Download flow demo data"),
        shinyWidgets::downloadBttn(ns("download_template_flow"), "Download flow template")
      )
    )
  )
}

mod_flow_instruction_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$download_demo_flow <- downloadHandler(
        filename = "demo_flowrate_data.xlsx",
        content = function(file) {
          file.copy("inst/extdata/demo_flowrate_data.xlsx", file, overwrite = TRUE)
        }
      )
      output$download_template_flow <- downloadHandler(
        filename = "flow_template.xlsx",
        content = function(file) {
          file.copy("inst/extdata/flow_template.xlsx", file, overwrite = TRUE)
        }
      )
    }
  )
}


