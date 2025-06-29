#' faq UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_faq_ui <- function(id) {
  ns <- NS(id)

  bslib::page_fixed(
    h3("Questions?"),
    HTML("Please email us at stormwater@sccwrp.org. You can find out more about SCCWRP's BMP Research at www.sccwrp.org"),
    tags$img(
      src = "https://ftp.sccwrp.org/pub/download/PROJECTS/SMC_BMP/sccwrp_logo.jpg",
      height = "60px"
    )
  )
}


#' faq Server Functions
#'
#' @noRd
mod_faq_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_faq_ui("faq_1")

## To be copied in the server
# mod_faq_server("faq_1")
