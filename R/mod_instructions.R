#' instructions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_instructions_ui <- function(id) {
  ns <- NS(id)

  bslib::page_fixed(

    h3("About this calculator"),
    HTML("This web application has been developed to enable consistent, transparent, easily applied calculations to generate statistics commonly use to describe rainfall and flow characteristics during stormwater best management practice (BMP) monitoring studies. The web app provides 2 types of analysis: rainfall and flow. For the rainfall analysis, the app provides the cumulative rainfall depth, rainfall duration, average rainfall intensity, and the maximum rainfall intensity over a 5-min or 10-min duration within a monitored event based on a user-uploaded hydrograph. For the flow analysis, hydrograph statistics determined by the web app include the total runoff volume, runoff duration, and peak (maximum) flow rate. If multiple hydrographs are provided for a single event, for example representing BMP inflow, outflow, and bypass, additional statistics are determined."),

  )
}

#' instructions Server Functions
#'
#' @noRd
mod_instructions_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_instructions_ui("instructions_1")

## To be copied in the server
# mod_instructions_server("instructions_1")
