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
    strong("Step 1: Submit Data"),
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

    bslib::card_body(
      bslib::tooltip(
        span(
          strong("Step 3 (Optional): Select Input Filter Parameters", bsicons::bs_icon("question-circle"))
        ),
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
      title = "Instruction",
      mod_flow_instruction_ui("flow_instruction")
    ),
    bslib::nav_panel(
      title = "Method",
      mod_flow_method_ui("flow_method")
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
              col_widths = c(2, 6, 4),
              tags$label(
                "Choose a flow type:",
                style = "margin-top: 0.7rem; font-weight: bold;"  # Bold text
              ),
              shinyWidgets::pickerInput(
                inputId = ns("choose_graph_flow"),
                choices = NULL,
                selected = NULL,
                multiple = TRUE
              ),
              shinyWidgets::actionBttn(
                inputId = ns("download_flow_plot"),
                label = "Download Plot",
                icon = bsicons::bs_icon("download")
              )
            )
          )
        ),
        bslib::card(
          bslib::card_body(
            DT::dataTableOutput(ns("flow_table"))
          ),
          bslib::card_footer(
            bslib::layout_columns(
              col_widths = c(6, 6),
              shinyWidgets::actionBttn(
                inputId = ns("download_flow_table"),
                label = "Download Table",
                icon = bsicons::bs_icon("download")
              ),
              shinyWidgets::actionBttn(
                inputId = ns("download_flow_table_smc"),
                label = "Download Table in SMC Format",
                icon = bsicons::bs_icon("download")
              )
            )
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
#' @import dplyr ggplot2
mod_flow_analysis_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    read_excel_allsheets <- function(filename) {
      sheets <- readxl::excel_sheets(filename)
      setNames(lapply(sheets, function(sheet) readxl::read_excel(filename, sheet = sheet)), sheets)
    }

    observeEvent(input$flow_file, {
      req(input$flow_file)

      inflow_data <- readxl::read_excel(input$flow_file$datapath, sheet = "inflow1")
      outflow_data <- readxl::read_excel(input$flow_file$datapath, sheet = "outflow")

      min_inflow_date <- min(as.Date(inflow_data$datetime))
      max_inflow_date <- max(as.Date(inflow_data$datetime))
      shinyWidgets::updateAirDateInput(session, "start_date_flow",
                                       value = min_inflow_date,
                                       options = list(minDate = min_inflow_date, maxDate = max_inflow_date))

      min_outflow_date <- min(as.Date(outflow_data$datetime))
      max_outflow_date <- max(as.Date(outflow_data$datetime))
      shinyWidgets::updateAirDateInput(session, "end_date_flow",
                                       value = max_outflow_date,
                                       options = list(minDate = min_outflow_date, maxDate = max_outflow_date))

      start_time <- format(min(as.POSIXct(inflow_data$datetime)), "%H:%M:%S")
      shinyWidgets::updateTimeInput(session, "start_hour_flow", value = hms::as_hms(start_time))

      end_time <- format(max(as.POSIXct(outflow_data$datetime)), "%H:%M:%S")
      shinyWidgets::updateTimeInput(session, "end_hour_flow", value = hms::as_hms(end_time))
    })

    observeEvent(input$submit_flow, {
      updateNavbarPage(session, "main_flow", selected = "Result")
      showModal(modalDialog("Calculating...", footer = NULL))
    })

    # --------------------------------------------------------------------------
    # 1. Data Processing Triggered by the Submit Button
    # --------------------------------------------------------------------------
    data_input <- eventReactive(input$submit_flow, {
      req(input$flow_file)
      all_data <- read_excel_allsheets(input$flow_file$datapath)
      all_data <- Filter(function(df) nrow(df) > 0, all_data)
      start <- as.POSIXct(paste(input$start_date_flow, as.character(input$start_hour_flow)))
      end   <- as.POSIXct(paste(input$end_date_flow, as.character(input$end_hour_flow)))
      lapply(all_data, function(df) {
        df <- df %>% dplyr::mutate(datetime = as.POSIXct(datetime))
        dplyr::filter(df, datetime >= start, datetime <= end)
      })
    })

    payload <- eventReactive(input$submit_flow, {
      req(data_input())
      sheets <- data_input()
      required_sheets <- c("inflow1", "inflow2", "bypass", "outflow")
      payload_list <- list()
      for(sheet in required_sheets) {
        if(sheet %in% names(sheets)) {
          df <- sheets[[sheet]]
          df <- df %>% dplyr::select(datetime, flow) %>% dplyr::arrange(datetime)
          payload_list[[sheet]] <- list(
            datetime = df$datetime,
            flow = df$flow,
            time_unit = input$flow_units_flow
          )
        }
      }
      jsonlite::toJSON(payload_list, dataframe = "columns", POSIXt = "ISO8601", auto_unbox = TRUE)
    })

    response <- eventReactive(input$submit_flow, {
      req(payload())
      res <- httr::POST(
        "https://nexus.sccwrp.org/bmp_hydrology/api/flow",
        body = payload(),
        encode = "json",
        httr::content_type_json()
      )
      content <- httr::content(res)
      removeModal()
      content
    })

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
    # **New: Update the Flow Type Picker Dynamically**
    # --------------------------------------------------------------------------
    observe({
      req(plot_data())
      flow_types <- unique(plot_data()$flow_type)
      shinyWidgets::updatePickerInput(session, "choose_graph_flow",
                                      choices = flow_types,
                                      selected = flow_types)
    })

    # --------------------------------------------------------------------------
    # 2. Outputs: Plot and Table
    # --------------------------------------------------------------------------
    output$flow_plot <- renderPlot({
      req(plot_data())
      df <- plot_data()
      # Filter based on the selected flow types.
      if (!is.null(input$choose_graph_flow) && length(input$choose_graph_flow) > 0) {
        df <- df %>% dplyr::filter(flow_type %in% input$choose_graph_flow)
      }
      ggplot2::ggplot(df, ggplot2::aes(x = datetime, y = flow, colour = flow_type)) +
        ggplot2::geom_line(size = 1.5) +
        ggplot2::labs(
          x = "Datetime",
          y = paste("Flow rate (", input$flow_units_flow, ")", sep = ""),
          title = input$graph_title_flow
        )
    })

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
        rownames = FALSE,
        options = list(
          dom = 't',
          paging = FALSE,
          ordering = FALSE
        )
      )
    })

    # --------------------------------------------------------------------------
    # 3. Download Handlers
    # --------------------------------------------------------------------------
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


