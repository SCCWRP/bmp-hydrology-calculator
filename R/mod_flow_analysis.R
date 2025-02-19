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
    width = "20%",
    open = "always",
    class = "html-fill-container",

    # Step 1: Upload Flow Data
    bslib::tooltip(
      span(strong("Step 1: Upload Flow Data", bsicons::bs_icon("question-circle"))),
      "The template is provided in the Instructions section below. (demo_data_flow.xlsx)"
    ),
    shinyWidgets::downloadBttn(
      outputId = ns("download_template"),
      label = "Download Template"
    ),
    strong("Step 1: Submit Data"),
    # Use shiny::fileInput here because shinyWidgets does not export fileInput.
    shiny::fileInput(
      inputId = ns("flow_file"),
      label = "Choose Excel File",
      multiple = FALSE,
      accept = ".xlsx"
    ),
    div("Upload complete"),

    # Step 2: Indicate Units of Flow Measurement
    strong("Step 2: Indicate Units of Flow Measurement"),
    shinyWidgets::pickerInput(
      inputId = ns("flow_units"),
      label = "Flow Units of Submitted Data",
      choices = c("L/s" = "L/s", "gal/min (gpm)" = "gal/min", "ft³/s (cfs)" = "ft³/s"),
      selected = "L/s",
      width = "200px"
    ),

    # Step 3: Select Input Filter Parameters (Optional)
    bslib::card_body(
      bslib::tooltip(
        span(strong("Step 3 (Optional): Select Input Filter Parameters", bsicons::bs_icon("question-circle"))),
        "Set the desired time range and graph title."
      ),
      shinyWidgets::airDatepickerInput(
        inputId = ns("start_date"),
        label = "Start Date",
        value = "2023-02-23",
        dateFormat = "yyyy-mm-dd"
      ),
      shinyWidgets::timeInput(
        inputId = ns("start_hour"),
        label = "Start Time"
      ),
      shinyWidgets::airDatepickerInput(
        inputId = ns("end_date"),
        label = "End Date",
        value = "2023-03-01",
        dateFormat = "yyyy-mm-dd"
      ),
      shinyWidgets::timeInput(
        inputId = ns("end_hour"),
        label = "End Time"
      ),
      textInput(
        inputId = ns("graph_title"),
        label = "Graph Title",
        placeholder = "Enter an optional title for the graph"
      ),
      shinyWidgets::actionBttn(
        inputId = ns("reset_button"),
        label = "Submit"
      )
    )
  )

  main_panel <- bslib::page_navbar(
    id = ns("main_flow"),
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
            plotOutput(ns("rainfall_plot"), height = "300px")
          ),
          bslib::card_footer(
            bslib::layout_columns(
              col_widths = c(6, 6),
              shinyWidgets::pickerInput(
                inputId = "event_selector",
                choices = c(1,2,3),
                multiple = FALSE
              ),
              shinyWidgets::actionBttn(ns("download_rainfall_plot"), "Download Plot")
            )
          )
        ),
        # --- Lower Half: Table Output ---
        bslib::card(
          bslib::card_body(
            DT::dataTableOutput(ns("rainfall_table"))
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


#' flow_analysis Server Functions
#'
#' @noRd
mod_flow_analysis_server <- function(id){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_flow_analysis_ui("flow_analysis_1")

## To be copied in the server
# mod_flow_analysis_server("flow_analysis_1")
