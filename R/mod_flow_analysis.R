#' flow_analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny NS tagList
mod_flow_analysis_ui <- function(id) {
  ns <- NS(id)

  sidebar <- bslib::sidebar(
    width = "20%",
    open = "always",
    class = "html-fill-container",

    bslib::tooltip(
      span(strong("Step 1: Download Demo Data", bsicons::bs_icon("question-circle"))),
      "Demo data for flow"
    ),
    shinyWidgets::downloadBttn(
      outputId = ns("download_demo_flow"),
    ),

    bslib::tooltip(
      span(strong("Step 2: Download Template", bsicons::bs_icon("question-circle"))),
      "Demo data for flow"
    ),
    shinyWidgets::downloadBttn(
      outputId = ns("download_template_flow"),
    ),

    strong("Step 3: Submit Data"),
    fileInput(
      inputId = ns("flow_file"),
      label = "Choose Excel File",
      multiple = FALSE,
      accept = ".xlsx"
    ),

    # Step 2: Indicate Units of Flow Measurement
    strong("Step 2: Indicate Units of Flow Measurement"),
    shinyWidgets::pickerInput(
      inputId = ns("flow_units_flow"),
      label = "Flow Units of Submitted Data",
      choices = c(
        "L/s" = "L/s",
        "gal/min (gpm)" = "gal/min",
        "ft³/s (cfs)" = "ft³/s"
      ),
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
        inputId = ns("start_date_flow"),
        label = "Start Date"
      ),
      shinyWidgets::timeInput(
        inputId = ns("start_hour_flow"),
        label = "Start Time"
      ),
      shinyWidgets::airDatepickerInput(
        inputId = ns("end_date_flow"),
        label = "End Date"
      ),
      shinyWidgets::timeInput(
        inputId = ns("end_hour_flow"),
        label = "End Time"
      ),
      textInput(
        inputId = ns("graph_title_flow"),
        label = "Graph Title",
        placeholder = "Enter an optional title for the graph"
      ),
      shinyWidgets::actionBttn(
        inputId = ns("submit_flow"),
        label = "Submit"
      )
    )
  )

  main_panel <- bslib::page_navbar(
    id = ns("main_flow"),
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
            plotOutput(ns("flow_plot"), height = "300px")
          ),
          bslib::card_footer(
            bslib::layout_columns(
              col_widths = c(6, 6),
              shinyWidgets::pickerInput(
                inputId = ns("event_selector_flow"),
                choices = c(1, 2, 3),
                multiple = FALSE
              ),
              shinyWidgets::pickerInput(
                inputId = ns("choose_graph_flow"),
                label = "Choose a flow type:",
                choices = NULL,
                selected = NULL,
                multiple = TRUE
              )
            )
          )
        ),
        bslib::card(
          bslib::card_body(
            DT::dataTableOutput(ns("flow_table"))
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
#' @import dplyr ggplo2
mod_flow_analysis_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    read_excel_allsheets <- function(filename) {
      sheets <- readxl::excel_sheets(filename)
      # Read each sheet and store them in a named list.
      setNames(lapply(sheets, function(sheet) readxl::read_excel(filename, sheet = sheet)), sheets)
    }

    observeEvent(input$flow_file, {
      req(input$flow_file)

      updateNavbarPage(session, "main_flow", selected = "Result")

      # Read the "inflow1" and "outflow" sheets from the uploaded file.
      inflow_data <- readxl::read_excel(input$flow_file$datapath, sheet = "inflow1")
      outflow_data <- readxl::read_excel(input$flow_file$datapath, sheet = "outflow")

      # Update the start date using the minimum date from the inflow sheet.
      min_inflow_date <- min(as.Date(inflow_data$datetime))
      print(min_inflow_date)
      max_inflow_date <- max(as.Date(inflow_data$datetime))
      shinyWidgets::updateAirDateInput(session, "start_date_flow",
                      value = min_inflow_date,
                      options = list(minDate = min_inflow_date, maxDate = max_inflow_date))


      # Update the end date using the maximum date from the outflow sheet.
      min_outflow_date <- min(as.Date(outflow_data$datetime))
      max_outflow_date <- max(as.Date(outflow_data$datetime))
      shinyWidgets::updateAirDateInput(session, "end_date_flow",
                      value = max_outflow_date,
                      options = list(minDate = min_outflow_date, maxDate = max_outflow_date))

      # Update the start time using the minimum time from the inflow sheet.
      start_time <- format(min(as.POSIXct(inflow_data$datetime)), "%H:%M:%S")
      shinyWidgets::updateTimeInput(session, "start_hour_flow", value = hms::as_hms(start_time))

      # Update the end time using the maximum time from the outflow sheet.
      end_time <- format(max(as.POSIXct(outflow_data$datetime)), "%H:%M:%S")
      shinyWidgets::updateTimeInput(session, "end_hour_flow", value = hms::as_hms(end_time))
    })



    # --------------------------------------------------------------------------
    # 1. Data Processing Triggered by the Submit Button
    # --------------------------------------------------------------------------
    # --------------------------------------------------------------------------
    # 1. Data Processing Triggered by the Submit Button
    # --------------------------------------------------------------------------
    data_input <- eventReactive(input$submit_flow, {
      req(input$flow_file)
      # Read all sheets from the uploaded file.
      all_data <- read_excel_allsheets(input$flow_file$datapath)
      # Remove any empty sheets.
      all_data <- Filter(function(df) nrow(df) > 0, all_data)

      # Build the start and end datetime objects from user inputs.
      start <- as.POSIXct(paste(input$start_date_flow, as.character(input$start_hour_flow)))
      end   <- as.POSIXct(paste(input$end_date_flow, as.character(input$end_hour_flow)))


      # For each sheet, convert the 'datetime' column and filter by the selected time range.
      lapply(all_data, function(df) {
        df <- df %>% dplyr::mutate(datetime = as.POSIXct(datetime))
        dplyr::filter(df, datetime >= start, datetime <= end)
      })
    })

    # Build the JSON payload for the API call.
    payload <- eventReactive(input$submit_flow, {
      req(data_input())
      # Get all sheets from the file (data_input() returns a named list)
      sheets <- data_input()

      # Define the required sheets
      required_sheets <- c("inflow1", "inflow2", "bypass", "outflow")

      # Build a payload list using only the required sheets
      payload_list <- list()
      for(sheet in required_sheets) {
        if(sheet %in% names(sheets)) {
          df <- sheets[[sheet]]
          # Select only the relevant columns, assuming the data frame has columns "datetime" and "flow"
          df <- df %>% dplyr::select(datetime, flow) %>% dplyr::arrange(datetime)
          payload_list[[sheet]] <- list(
            datetime = df$datetime,
            flow = df$flow,
            time_unit = input$flow_units_flow
          )
        }
      }

      # Convert the list to JSON. Using auto_unbox ensures that single values aren't put in arrays.
      jsonlite::toJSON(payload_list, dataframe = "columns", POSIXt = "ISO8601", auto_unbox = TRUE)
    })


    # Call the Flow API.
    response <- eventReactive(input$submit_flow, {
      req(payload())
      showModal(modalDialog("Calculating...", footer = NULL))
      res <- httr::POST(
        "https://nexus.sccwrp.org/bmp_hydrology/api/flow",
        body = payload(),
        encode = "json",
        httr::content_type_json()
      )
      content <- httr::content(res)
      removeModal()
      print(content)
      content
    })



    # Process the returned statistics.
    statistics <- eventReactive(input$submit_flow, {
      req(response())
      my_content <- response()$statistics
      my_content <- lapply(seq_along(my_content), function(i) {
        sheet_name <- names(my_content)[i]
        if (sheet_name %in% c("inflow1", "inflow2", "bypass", "outflow")) {
          df <- my_content[[i]] %>%
            tibble::as_tibble() %>%
            tidyr::unnest(cols = everything()) %>%
            dplyr::mutate(
              flow_type = sheet_name,
              runoff_duration = round(runoff_duration, 1),
              peak_flow_rate = round(peak_flow_rate, 1),
              runoff_volume = round(runoff_volume, 1)
            ) %>%
            dplyr::select(flow_type, start_time, peak_flow_rate, runoff_duration, runoff_volume, start_time, end_time) %>%
            dplyr::mutate(
              start_time = stringr::str_replace(start_time, "T", " "),
              end_time   = stringr::str_replace(end_time, "T", " ")
            )
          return(df)
        } else {
          return(NULL)
        }
      })
      my_content <- dplyr::bind_rows(my_content)
      my_content <- my_content %>%
        dplyr::mutate(flow_type = factor(flow_type, levels = c("inflow1", "inflow2", "bypass", "outflow"))) %>%
        dplyr::arrange(flow_type)
      my_content
    })

    # Combine data from all sheets for plotting.
    plot_data <- eventReactive(input$submit_flow, {
      req(data_input())
      dplyr::bind_rows(
        lapply(seq_along(data_input()), function(i) {
          df <- data_input()[[i]]
          df$flow_type <- names(data_input())[i]
          df
        })
      )
    })

    # --------------------------------------------------------------------------
    # 2. Outputs: Plot and Table
    # --------------------------------------------------------------------------

    # Render the hydrograph.
    output$flow_plot <- renderPlot({
      req(plot_data())
      df <- plot_data()
      ggplot2::ggplot(df, ggplot2::aes(x = datetime, y = flow, colour = flow_type)) +
        ggplot2::geom_line() +
        ggplot2::labs(
          x = "Datetime",
          y = paste("Flow rate (", input$flow_units_flow, ")", sep = ""),
          title = input$graph_title_flow
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text  = ggplot2::element_text(size = 12),
          axis.title = ggplot2::element_text(size = 14, face = "bold"),
          plot.title = ggplot2::element_text(size = 16, face = "bold")
        )
    })

    # Render a table of the flow statistics.
    output$flow_table <- DT::renderDT({
      req(statistics())
      data <- statistics() %>%
        dplyr::select(flow_type, peak_flow_rate, runoff_duration, runoff_volume) %>%
        dplyr::rename(
          "Type of flow" = flow_type,
          "Peak flow rate" = peak_flow_rate,
          "Duration of runoff (h)" = runoff_duration,
          "Runoff volume" = runoff_volume
        )
      DT::datatable(
        data,
        options = list(searching = FALSE),
        rownames = FALSE
      )
    })

    # --------------------------------------------------------------------------
    # 3. Download Handlers
    # --------------------------------------------------------------------------

    output$download_demo_flow <- downloadHandler(
      filename = "demo_flowrate_data.xlsx",
      content = function(file) {
        file.copy("inst/extdata/demo_flowrate_data.xlsx", file, overwrite = TRUE)
      }
    )

    output$download_template_flow <- downloadHandler(
      filename = "flow_template.xlsx",
      content = function(file) {
        file.copy("inst/extdata/flow_template.xlsx", file, overwrite = TRUE)
      }
    )

    output$download_flow_plot <- downloadHandler(
      filename = function() {
        "downloaded_flow_plot.png"
      },
      content = function(file) {
        ggplot2::ggsave(
          file,
          plot = plot_data() + ggplot2::coord_cartesian(expand = FALSE),
          device = "png"
        )
      }
    )

  })
}


