#' infiltration_analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_infiltration_analysis_ui <- function(id) {
  ns <- NS(id)
  sidebar <- bslib::sidebar(
    width = "17%",
    open = "always",
    class = "html-fill-container",
    bslib::tooltip(
      span(strong("Step 1: Download Demo Data", bsicons::bs_icon("question-circle"))),
      "See demo data for the required format."
    ),
    downloadButton(ns("download_demo_infiltration")),
    bslib::tooltip(
      span(strong("Step 2: Download Template", bsicons::bs_icon("question-circle"))),
      "Overwrite the template with your data."
    ),
    downloadButton(ns("download_template_infiltration")),
    strong("Step 3: Submit Data"),
    fileInput(
      ns("file"),
      "Choose Excel File",
      multiple = FALSE,
      accept = ".xlsx"
    ) |>
      bslib::as_fillable_container(style = "overflow-y:auto", max_height = "200px"),
    bslib::card_body(
      bslib::tooltip(
        span(strong("Constants for smoothing and regression", bsicons::bs_icon("question-circle"))),
        "These numbers are for informative only. They are not adjustable by the user for now."
      ),
      shinyjs::disabled(
        numericInput(
          inputId = ns("smoothing_window"),
          label = "Smoothing Window",
          value = 5
        )
      ),
      shinyjs::disabled(
        numericInput(
          inputId = ns("regression_window"),
          label = "Regression Window",
          value = 720
        )
      ),
      shinyjs::disabled(
        numericInput(
          inputId = ns("regression_threshold"),
          label = "Regression Threshold",
          value = 0.999
        )
      )
    )
  )

  main_panel <- bslib::page_navbar(
    id = ns("main_infiltration"),
    padding = 0,
    bslib::nav_panel(
      title = "Instruction"
    ),
    bslib::nav_panel(
      title = "Method"
    ),
    bslib::nav_panel(
      title = "Result",
      bslib::layout_columns(
        col_widths = 12,
        row_heights = c(1, 1),
        bslib::card(
          full_screen = TRUE,
          bslib::card_body(
            plotOutput(ns("plot_infiltration"), height = "300px")
          )
        ),
        bslib::card(
          bslib::card_body(
            DT::dataTableOutput(ns("table_infiltration"))
          )
        )
      )
    )
  )


  bslib::page_sidebar(
    sidebar = sidebar,
    main_panel
  )

}

#' infiltration_analysis Server Functions
#'
#' @noRd
mod_infiltration_analysis_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$download_demo_infiltration <- downloadHandler(
      filename = "demo_infiltration_data.xlsx",
      content = function(file) {
        template_path <- "inst/extdata/demo_infiltration_data.xlsx"
        file.copy(template_path, file, overwrite = TRUE)
      }
    )

  })
}
