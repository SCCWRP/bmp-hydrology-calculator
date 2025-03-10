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
**It is required that the column names and tab names in the templates remain unchanged.**

Flow data should be copy-pasted from a user's datasheet to the downloaded data template. Up to four flows can be accommodated for analysis. The available flow types are inflow 1, inflow 2, outflow, and bypass. Refer to the Methods tab for an illustration of the possible flow type configurations. A user does not need to submit all four types; any combination of the flow types is acceptable. A user can only submit flow data for a single rain event. If any of the data types are not applicable, leave the tab blank (as is).

- Download flow demo data
- Download flow template

**Data Requirements:**
The uploaded Excel spreadsheet must conform to the following requirements:
1. Flow must be reported in units of L/s, gpm, or cfs. All tabs must use the same flow unit.
2. The timestamp should be in 24-hour format (mm/dd/yy hh:mm:ss).
3. Each tab must contain exactly two columns, one for the sample timestamps data and one for the associated values.
4. The column names and the tab names must not be changed from the template.
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


