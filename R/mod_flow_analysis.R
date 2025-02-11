#' flow_analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_flow_analysis_ui <- function(id) {
  ns <- NS(id)
  sidebar <- bslib::sidebar(
    width = "15%",
    open = "always",
    class = "html-fill-container",
    bslib::tooltip(
      span(strong("Step 1: Download Template", bsicons::bs_icon("question-circle"))),
      "Overwrite the template with your data. See Data Requirements on Instructions tab."
    ),
    downloadButton(ns("download_template")),
    strong("Step 2: Submit Data"),
    fileInput(
      ns("file"),
      "Choose Excel File",
      multiple = FALSE,
      accept = ".xlsx"
    ) |>
      bslib::as_fillable_container(style = "overflow-y:auto", max_height = "200px"),
    bslib::card_body(
      bslib::tooltip(
        span(strong("Step 3: Select Input Filter Parameters", bsicons::bs_icon("question-circle"))),
        "Optional: Use the range slider below the hydrograph to reduce the calculations to the time period of interest. The Composite Volume input controls the total volume in the Aliquot Volume calculation, with a minimum of 500 mL and maximum of 10,000 mL."
      ),
      shinyjs::disabled(
        numericInput(
          inputId = ns("composite_vol"),
          label = "Composite Vol. (mL)",
          value = 1000,
          min = 500,
          max = 10000,
          width = "200px"
        )
      ),
      selectInput(
        ns("flow_units"),
        "Flow Units of Submitted Data",
        c(`L/s` = "L/s", `gal/min (gpm)` = "gal/min", `ft³/s (cfs)` = "ft³/s"),
        selected = "L/s",
        width = "200px"
      ),
      shinyjs::disabled(
        actionButton(ns("submit"), "Submit")
      ),
      downloadButton(
        outputId = ns("download_results"),
        label = list(bsicons::bs_icon("download"), "Results"),
        icon = NULL
      )
    ),
    bslib::card_body(
      class = "p-0",
      fill = FALSE,
      strong("Submit New Data"),
      actionButton(ns("reset_button"), "Reload App"),
    )
  )

  bslib::page_sidebar(
    sidebar = sidebar,
    bslib::page_navbar(
      id = ns("results_nav"),
      padding = 0,
      #bslib::nav_panel("Flow-Weighting", mod_flow_weighting_ui(ns("flow_weighting")))
    )
  )
}

#' flow_analysis Server Functions
#'
#' @noRd
mod_flow_analysis_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_flow_analysis_ui("flow_analysis_1")

## To be copied in the server
# mod_flow_analysis_server("flow_analysis_1")
