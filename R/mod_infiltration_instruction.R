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
Using this Application (Infiltration)
It is required that the column names and tab names in the templates remain unchanged.

**Infiltration Analysis:**
Infiltration data should be copied and pasted from your datasheet into the downloaded data template. This application can accommodate infiltration measurements over time from multiple infiltration points if needed. Refer to the Methods tab for an illustration of how infiltration data should be structured. A user can submit infiltration data for one or more events in a single file, as long as timestamps and values are continuous and without skipping records.

- Download infiltration template
- Download infiltration demo data

**Data Requirements**
The uploaded Excel spreadsheet must conform to the following requirements:
1. Infiltration must be reported in consistent units (e.g., in/hr, mm/hr).
2. The timestamp should be in 24-hour format (mm/dd/yy hh:mm:ss).
3. Each tab must contain exactly two columns, one for the sample timestamps data and one for the associated values.
4. The column names and the tab names must not be changed from the template.
"

  bslib::card(
    bslib::card_body(
      HTML(
        markdown::markdownToHTML(
          text = infiltration_markdown_text,
          fragment.only = TRUE
        )
      )
    )
  )
}
