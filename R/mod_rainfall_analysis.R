#' rainfall_analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_rainfall_analysis_ui <- function(id) {
  ns <- NS(id)

  # Sidebar remains the same as before (with steps 1-4)
  sidebar <- bslib::sidebar(
    width = "20%",
    open = "always",
    class = "html-fill-container",

    # Step 1: Download Template
    bslib::tooltip(
      span(strong("Step 1: Download Template"), bsicons::bs_icon("question-circle")),
      "Overwrite the template with your data. See Data Requirements on the Instructions tab."
    ),
    downloadButton(ns("download_template"), "Download Template"),

    # Step 2: Upload Rainfall Data
    bslib::tooltip(
      span(strong("Step 2: Upload Rainfall Data"), bsicons::bs_icon("question-circle")),
      "Upload rainfall data (.xlsx file)."
    ),
    fileInput(
      ns("file"),
      label = "Upload rainfall data (.xlsx file)",
      multiple = FALSE,
      accept = ".xlsx"
    ),

    # Step 3: Choose a Rainfall Resolution
    bslib::tooltip(
      span(strong("Step 3: Choose a Rainfall Resolution"), bsicons::bs_icon("question-circle")),
      "Select the resolution for the rainfall data."
    ),
    selectInput(
      ns("rainfall_resolution"),
      label = "Choose a rainfall resolution",
      choices = c("0.01 inch" = 0.01, "0.1 mm" = 0.1),
      selected = 0.01
    ),

    # Step 4: Optional Graph Title
    textInput(
      ns("title"),
      label = "Input a title for the graph (optional)",
      placeholder = "Enter an optional title for the graph(s)",
      value = "",
      width = "100%"
    ),

    # Additional UI elements: notice and submit button
    bslib::card_body(
      shiny::textOutput(ns("resubmit_notice")),
      shinyjs::disabled(shinyWidgets::actionBttn(ns("submit"), "Submit"))
    )
  )

  main_panel <- bslib::page_navbar(
    id = ns("results_nav"),
    padding = 0,
    bslib::nav_panel(
      title = "Rainfall Analysis",
      bslib::layout_columns(
        col_widths = 12,
        row_heights = c(1, 1),  # Two rows of equal height
        # --- Upper Half: Plot and Control Buttons ---
        bslib::card(
          bslib::card_body(
            # Row with two buttons side by side
            bslib::layout_columns(
              col_widths = c(6, 6),
              shinyWidgets::actionBttn(ns("download_plot"), "Download Plot"),
              shinyWidgets::actionBttn(ns("choose_rain_event"), "Choose a rain event")
            ),
            # Plot output (adjust height as needed)
            plotOutput(ns("plot"), height = "300px")
          )
        ),
        # --- Lower Half: Table Output ---
        bslib::card(
          bslib::card_body(
            DT::dataTableOutput(ns("table"))
          )
        )
      )
    )
  )

  # Combine sidebar and main panel into one page layout
  bslib::page_sidebar(
    sidebar = sidebar,
    main_panel
  )
}



#' rainfall_analysis Server Functions
#'
#' @noRd
# Server module for Rainfall Analysis
mod_rainfall_analysis_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- Dummy Data for Three Rain Events ---
    events <- list(
      "Event 1" = data.frame(
        time = seq.POSIXt(from = Sys.time(), length.out = 20, by = "min"),
        rainfall = runif(20, min = 0, max = 10)
      ),
      "Event 2" = data.frame(
        time = seq.POSIXt(from = Sys.time(), length.out = 20, by = "min"),
        rainfall = runif(20, min = 5, max = 15)
      ),
      "Event 3" = data.frame(
        time = seq.POSIXt(from = Sys.time(), length.out = 20, by = "min"),
        rainfall = runif(20, min = 10, max = 20)
      )
    )

    # Reactive value to track the current event index
    current_event_index <- reactiveVal(1)

    # Reactive expression to retrieve the current event's data
    current_event_data <- reactive({
      event_names <- names(events)
      current_event <- event_names[current_event_index()]
      data <- events[[current_event]]
      data$event <- current_event  # Add the event name as a column
      data
    })

    # --- Update the current event when "Choose a rain event" is clicked ---
    observeEvent(input$choose_rain_event, {
      new_index <- current_event_index() + 1
      if (new_index > length(events)) {
        new_index <- 1  # Cycle back to the first event
      }
      current_event_index(new_index)
    })

    # --- Render the Rainfall Plot ---
    output$plot <- renderPlot({
      data <- current_event_data()
      plot(data$time, data$rainfall,
           type = "o",
           col = "blue",
           xlab = "Time",
           ylab = "Rainfall",
           main = paste("Rainfall Data for", unique(data$event)))
    })

    # --- Render the Data Table ---
    output$table <- DT::renderDataTable({
      current_event_data()
    })

    # --- Download Handler for the Plot ---
    output$download_plot <- downloadHandler(
      filename = function() {
        paste0("rainfall_plot_", Sys.Date(), ".png")
      },
      content = function(file) {
        png(file)
        data <- current_event_data()
        plot(data$time, data$rainfall,
             type = "o",
             col = "blue",
             xlab = "Time",
             ylab = "Rainfall",
             main = paste("Rainfall Data for", unique(data$event)))
        dev.off()
      }
    )

    # --- Download Handler for the Template ---
    output$download_template <- downloadHandler(
      filename = function() {
        "rainfall_template.xlsx"
      },
      content = function(file) {
        # Create a dummy Excel template using writexl
        if (!requireNamespace("writexl", quietly = TRUE)) {
          stop("Please install the 'writexl' package to enable template download.")
        }
        template <- data.frame(
          time = Sys.time() + 1:10,
          rainfall = rep(NA, 10)
        )
        writexl::write_xlsx(template, path = file)
      }
    )

    # (Additional server logic for processing fileInput or other inputs can be added here.)
  })
}



## To be copied in the UI
# mod_rainfall_analysis_ui("rainfall_analysis_1")

## To be copied in the server
# mod_rainfall_analysis_server("rainfall_analysis_1")
