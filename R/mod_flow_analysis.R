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

    # Enable the Submit button once a file is uploaded.
    observe({
      if (TRUE) {
        shinyjs::enable("submit")
      } else {
        shinyjs::disable("submit")
      }
    })

    #### 1. Create Dummy Data Only After Submit is Clicked ####
    dummy_info <- eventReactive(input$submit, {
      # Create summary table for 3 events.
      summary_table <- data.frame(
        eventid = 1:3,
        storm_date = as.POSIXct(c(Sys.time() + 3600, Sys.time() + 7200, Sys.time() + 10800)),  # Storm starts in 1h, 2h, 3h from now.
        storm_end = as.POSIXct(c(Sys.time() + 3600 + 1800, Sys.time() + 7200 + 1800, Sys.time() + 10800 + 1800)),  # Each storm lasts 30 minutes.
        total_rainfall = round(runif(3, 10, 50), 1),
        average_rainfall_intensity = round(runif(3, 1, 5), 1),
        peak_5min_rainfall_intensity = round(runif(3, 5, 20), 1),
        peak_10min_rainfall_intensity = round(runif(3, 4, 18), 1),
        peak_60_min_rainfall_intensity = round(runif(3, 2, 10), 1),
        antecedent_dry_period = round(runif(3, 24, 72), 0)
      )

      # Create time-series data for each event.
      # For each event, generate data from 1 hour before storm_date to 1 hour after storm_end.
      ts_data <- list()
      for (i in 1:3) {
        storm_start <- summary_table$storm_date[i]
        storm_end <- summary_table$storm_end[i]
        ts_start <- storm_start - 3600
        ts_end <- storm_end + 3600
        times <- seq(from = ts_start, to = ts_end, length.out = 100)
        # Generate random rainfall values (scaled using total_rainfall).
        values <- round(runif(100, min = 0, max = summary_table$total_rainfall[i] / 2), 2)
        ts_data[[as.character(i)]] <- data.frame(datetime = times, value = values)
      }
      list(summary_table = summary_table, ts_data = ts_data)
    })

    # After dummy data is created, update the event selector to pick the first event.
    observeEvent(dummy_info(), {
      shinyWidgets::updatePickerInput(session, "event_selector", selected = 1)
    })

    #### 2. Reactives for Filtering Based on the Selected Event ####

    # Reactive: Get the summary row for the selected event.
    selected_summary <- reactive({
      req(dummy_info())
      req(input$event_selector)
      dummy_info()$summary_table[dummy_info()$summary_table$eventid == as.numeric(input$event_selector), ]
    })

    # Reactive: Get the time-series data for the selected event and filter to only include data within the storm period.
    selected_ts <- reactive({
      req(dummy_info())
      req(input$event_selector)
      event_id <- as.character(input$event_selector)
      df <- dummy_info()$ts_data[[event_id]]
      sum_row <- selected_summary()
      start_time <- sum_row$storm_date
      end_time <- sum_row$storm_end
      df[df$datetime >= start_time & df$datetime <= end_time, ]
    })

    #### 3. Plot and Table Outputs ####

    # (a) Render the rainfall plot using ggplot2 with your specified theme.
    output$rainfall_plot <- renderPlot({
      req(selected_ts())
      data <- selected_ts()
      sum_row <- selected_summary()
      print("in this")
      plot_title <- paste("Rainfall Data for Event", sum_row$eventid)
      p <- ggplot2::ggplot(data, ggplot2::aes(x = datetime, y = value)) +
        ggplot2::geom_line(color = "blue") +
        ggplot2::geom_point(color = "red") +
        ggplot2::labs(
          x = "Datetime",
          y = "Rainfall",
          title = plot_title
        ) +
        ggplot2::scale_x_datetime(
          breaks = scales::breaks_pretty(n = 10),
          labels = scales::label_date(format = "%m-%d %H:%M")
        ) +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5),
          legend.position = "top",
          panel.grid.minor.x = ggplot2::element_blank(),
          panel.grid.minor.y = ggplot2::element_blank()
        )
      p
    })

    # (b) Render the summary table (all events) using the DT package.
    output$rainfall_table <- DT::renderDataTable({
      req(dummy_info())
      dummy_info()$summary_table
    }, options = list(pageLength = 10))

    # (c) Optionally, update a text output after submission.
    output$resubmit_notice <- renderText({
      req(dummy_info())
      "Data has been submitted and is now displayed."
    })

    #### 4. Download Handler for the Plot ####

    observe({
      req(selected_ts())
      output$download_rainfall_plot <- downloadHandler(
        filename = function() {
          paste0("RainfallPlot-", format(Sys.time(), "%Y-%m-%d-%H%M%S"), ".png")
        },
        content = function(file) {
          # Apply a local thematic theme using a cosmo preset from bslib.
          thematic::thematic_local_theme(
            thematic::thematic_theme(
              bg = bslib::bs_get_contrast(bslib::bs_theme(preset = "cosmo"), "secondary"),
              fg = bslib::bs_get_variables(bslib::bs_theme(preset = "cosmo"), "secondary")
            )
          )
          data <- selected_ts()
          sum_row <- selected_summary()
          plot_title <- paste("Rainfall Data for Event", sum_row$eventid)
          p <- ggplot2::ggplot(data, ggplot2::aes(x = datetime, y = value)) +
            ggplot2::geom_line(color = "blue") +
            ggplot2::geom_point(color = "red") +
            ggplot2::labs(
              x = "Datetime",
              y = "Rainfall",
              title = plot_title
            ) +
            ggplot2::scale_x_datetime(
              breaks = scales::breaks_pretty(n = 10),
              labels = scales::label_date(format = "%m-%d %H:%M")
            ) +
            ggplot2::theme(
              axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5),
              legend.position = "top",
              panel.grid.minor.x = ggplot2::element_blank(),
              panel.grid.minor.y = ggplot2::element_blank()
            )
          ggplot2::ggsave(file, plot = p, device = "png", height = 6.94, width = 9.2302)
        }
      )
    })

  })
}

## To be copied in the UI
# mod_flow_analysis_ui("flow_analysis_1")

## To be copied in the server
# mod_flow_analysis_server("flow_analysis_1")
