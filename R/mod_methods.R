#' methods UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'

markdown_text <- httr::GET("https://raw.githubusercontent.com/SCCWRP/rainfall_flow_calculator_api/master/README.md") |>
  httr::content()
mod_methods_ui <- function(id) {
  ns <- NS(id)
  bslib::page_fixed(
    markdown_text |>
      commonmark::markdown_html() |>
      HTML() |>
      withMathJax()
  )
}

#' methods Server Functions
#'
#' @noRd
mod_methods_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_methods_ui("methods_1")

## To be copied in the server
# mod_methods_server("methods_1")
