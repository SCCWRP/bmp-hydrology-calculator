#' Infiltration Method UI Function
#'
#' @description A Shiny module for displaying the method section for infiltration analysis.
#'
#' @param id Internal parameter for {shiny} namespace.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList HTML withMathJax
mod_infiltration_method_ui <- function(id) {
  ns <- NS(id)

  # Fetch the markdown text from GitHub or another source.
  # Adjust the URL to point to your infiltration-specific README if needed.
  markdown_text <- httr::GET("https://raw.githubusercontent.com/SCCWRP/rainfall_flow_calculator_api/master/README_infiltration.md") |>
    httr::content(as = "text", encoding = "UTF-8")
  bslib::card(
    bslib::card_body(
      markdown_text |>
        commonmark::markdown_html() |>
        HTML() |>
        withMathJax()
    )
  )
}
