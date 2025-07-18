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
This web application has been developed by the Southern California Coastal Water Research Project to enable consistent, transparent, and easily applied calculations of rainfall, flow, and infiltration for stormwater best management practice (BMP) monitoring studies. It provides three types of analyses:
1. **Rainfall Analysis**
   generates the cumulative rainfall depth, average rainfall intensity, and the maximum rainfall intensity over the entire storm and specified durations (e.g., 5, 10 or 60 minutes), and antecedent dry period based on a user-uploaded hyetograph.
2. **Flow Analysis**
   calculates hydrograph statistics including total runoff volume, runoff duration, and the peak (maximum) flow rate. If multiple hydrographs are provided for a single event (e.g., BMP inflow, outflow, and bypass), hydrograph statistics for each flow type are calculated.
3. **Infiltration Analysis**
   calculates infiltration rate of ponded water through a BMP based on water level depth measured over time. The calculation applies to water that moves across a soil-water interface, for example ponded runoff moving from the surface into a filtration media, or runoff stored in a subsurface BMP exfiltrating into the surrounding soil (e.g. the vadose zone).

This approach ensures that essential stormwater BMP metrics (e.g., rainfall intensity, runoff volume, infiltration rates) are derived using transparent and reproducible methods, all within a unified web-based environment.

**Who should use this calculator?**

This web application is intended for technical users. The calculators process data obtained from sensors to generate summary statistics commonly used to provide context for and/or interpret stormwater BMP performance. This web application itself does not provide any interpretation.

While creating the calculator was intended to support field monitoring of stormwater BMPs, it may be applied for other hydrologic monitoring applications where rainfall, hydrograph, or infiltration analysis may be of interest.

The user is responsible for raw data quality assurance. There are no checks of data quality built into the calculators, other than for missing data and formatting.

The documentation herein does not provide guidance on how to install sensors or collect field data. Users interested to learn more about stormwater BMP field monitoring are encouraged to review guidance from the International Stormwater BMP Database at https://bmpdatabase.org/monitoring.


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
      "Feedback Form - Rainfall and Flow Analysis",
      onclick ="window.open('https://forms.office.com/Pages/ResponsePage.aspx?id=PfKopOEaHEuZAuqhUwKBkLfF7RbFiCdKlqnmjECrBFxUMTBTVFpTSlcxM1VJSkM3NUdQQTdBSU5ENi4u', '_blank')"
    ),
    shinyWidgets::actionBttn(
      "feedback",
      "Feedback Form for Infiltration Analysis",
      onclick ="window.open('https://forms.office.com/Pages/ResponsePage.aspx?id=PfKopOEaHEuZAuqhUwKBkLfF7RbFiCdKlqnmjECrBFxUNElCTFBLWlFPQUZGUTA0QTdFMkFXTDRTNi4u', '_blank')"
    ),
    shinyWidgets::actionBttn(
      "apidoc",
      "API Documentation",
      onclick ="window.open('https://nexus.sccwrp.org/bmp_hydrology/api/docs', '_blank')"
    ),
    tags$img(
      src = "https://ftp.sccwrp.org/pub/download/PROJECTS/SMC_BMP/sccwrp_logo.jpg",
      height = "60px"
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
