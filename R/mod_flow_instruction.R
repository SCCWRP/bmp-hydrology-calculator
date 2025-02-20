#' Flow Instruction UI Function
#'
#' @description A Shiny module for displaying instructions for Flow Analysis.
#'
#' @param id Internal parameter for {shiny} namespace.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList HTML
mod_flow_instruction_ui <- function(id) {
  ns <- NS(id)

  # Flow-specific markdown text
  flow_markdown_text <- "
Using this Application (Flow)
It is required that the column names and tab names in the templates remain unchanged.

**Flow Analysis:**
Flow data should be copy-pasted from a user's datasheet to the downloaded data template. Up to four flows can be accommodated for analysis. The available flow types are inflow 1, inflow 2, outflow, and bypass. Refer to the Methods tab for an illustration of the possible flow type configurations. A user does not need to submit all four types; any combination of the flow types is acceptable. A user can only submit flow data for a single rain event. If any of the data types are not applicable, leave the tab blank (as is).

- Download flow template
- Download flow demo data

**Data Requirements**
The uploaded Excel spreadsheet must conform to the following requirements:
1. Flow must be reported in units of L/s, gpm, or cfs.
2. The timestamp should be in 24-hour format (mm/dd/yy hh:mm:ss).
3. Each tab must contain exactly two columns, one for the sample timestamps data and one for the associated values.
4. The column names and the tab names must not be changed from the template.
"

  bslib::card(
    bslib::card_body(
      HTML(
        markdown::markdownToHTML(
          text = flow_markdown_text,
          fragment.only = TRUE
        )
      )
    )
  )
}
