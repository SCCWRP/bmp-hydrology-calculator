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
    )

  )
}
