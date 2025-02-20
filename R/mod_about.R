#' instructions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList


about_text <- "
This web application has been developed to enable consistent, transparent, and easily applied calculations of rainfall, flow, and infiltration characteristics during stormwater best management practice (BMP) monitoring studies. It provides three types of analyses:

1. **Rainfall Analysis**
   Generates the cumulative rainfall depth, rainfall duration, average rainfall intensity, and the maximum rainfall intensity over specified durations (e.g., 5 or 10 minutes) based on a user-uploaded hydrograph.

2. **Flow Analysis**
   Calculates hydrograph statistics including total runoff volume, runoff duration, and the peak (maximum) flow rate. If multiple hydrographs are provided for a single event (e.g., BMP inflow, outflow, and bypass), additional combined statistics are determined.

3. **Infiltration Analysis**
   To be added later

This approach ensures that essential stormwater BMP metrics (rainfall intensity, runoff volume, infiltration rates, etc.) are derived using transparent and reproducible methods, all within a unified web-based environment.

"


mod_about_ui <- function(id) {
  ns <- NS(id)

  bslib::page_fixed(
    HTML(
      markdown::markdownToHTML(
        text = about_text,
        fragment.only = TRUE
      )
    ),
    shinyWidgets::actionBttn(
      "feedback",
      "Feedback Form",
      onclick ="window.open('https://forms.office.com/Pages/ResponsePage.aspx?id=PfKopOEaHEuZAuqhUwKBkLfF7RbFiCdKlqnmjECrBFxUMTBTVFpTSlcxM1VJSkM3NUdQQTdBSU5ENi4u', '_blank')"
    ),
    shinyWidgets::actionBttn(
      "apidoc",
      "API Documentation",
      onclick ="window.open('https://nexus.sccwrp.org/bmp_hydrology/api/docs', '_blank')"
    )
  )
}

#' instructions Server Functions
#'
#' @noRd
mod_about_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    })
}
