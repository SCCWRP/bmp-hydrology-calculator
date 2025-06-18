#' Flow Method UI Function
#'
#' @description A Shiny module for displaying the method section for flow analysis.
#'
#' @param id Internal parameter for {shiny} namespace.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList HTML withMathJax
mod_rainfall_method_ui <- function(id) {
  ns <- NS(id)

  # Fetch the markdown text from GitHub.
  markdown_text <- httr::GET("https://raw.githubusercontent.com/SCCWRP/bmp-hydrology-calculator/refs/heads/master/README_rainfall.md") |>
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
