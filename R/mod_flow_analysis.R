#' flow_analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
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

    bslib::tooltip(
      span(strong("Step 1: Upload data"), bsicons::bs_icon("question-circle")),
      "Expects a single .xlsx file. See Data Requirements for more info."
    ),
    fileInput(
      inputId = ns("flow_file"),
      label = NULL,
      multiple = FALSE,
      accept = ".xlsx"
    ),

    # New: Validate Data Button with expectations tooltip.
    bslib::tooltip(
      span(strong("Step 2: Validate data"), bsicons::bs_icon("question-circle")),
      "Data must be validated before proceeding."),
    shinyjs::disabled(
      shinyWidgets::actionBttn(ns("validate_flow"), "Validate data")
    ),

    strong("Step 3: Indicate units of flow measurement"),
    shinyWidgets::pickerInput(
      inputId = ns("flow_units_flow"),
      label = NULL,
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
          strong("Step 4 (Optional): Select start and end of the hydrograph", bsicons::bs_icon("question-circle"))
        ),
        "Autopopulated based on inflow1 and outflow tab."
      ),
      shinyWidgets::airDatepickerInput(
        inputId = ns("start_date_flow"),
        label = "Start date"
      ),
      shinyWidgets::timeInput(
        inputId = ns("start_hour_flow"),
        label = "Start time"
      ),
      shinyWidgets::airDatepickerInput(
        inputId = ns("end_date_flow"),
        label = "End date"
      ),
      shinyWidgets::timeInput(
        inputId = ns("end_hour_flow"),
        label = "End time"
      ),
      textInput(
        inputId = ns("graph_title_flow"),
        label = "Input a title for the graph (optional)",
        placeholder = NULL
      ),
      bslib::tooltip(
        span(strong("Step 5: Submit data"), bsicons::bs_icon("question-circle")),
        "Submit data when validation is successful."
      ),
      shinyjs::disabled(
        shinyWidgets::actionBttn(
          inputId = ns("submit_flow"),
          label = "Submit"
        )
      )
    )
  )

  main_panel <- bslib::navset_card_underline(
    id = ns("main_flow"),
    bslib::nav_panel(
      title = "Instruction",
      mod_flow_instruction_ui("flow_instruction")
    ),
    bslib::nav_panel(
      title = "Method",
      mod_flow_method_ui("flow_method")
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


    # Create a flow plot based on filtered data.
    create_flow_plot <- function(df) {
      ggplot2::ggplot(df, ggplot2::aes(x = datetime, y = flow, colour = flow_type)) +
        ggplot2::geom_line(size = 1.5) +
        ggplot2::labs(
          x = "Datetime",
          y = paste("Flow rate (", input$flow_units_flow, ")", sep = ""),
          title = input$graph_title_flow
        ) +
        ggplot2::scale_x_datetime(
          date_breaks = "2 hours",
          date_labels = "%m/%d %H:%M"
        ) +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)
        )
    }



    # When a file is uploaded, enable the Validate Data button and disable Submit.
    observeEvent(input$flow_file, {
      req(input$flow_file)
      tryCatch({
        shinyjs::enable("validate_flow")
        shinyjs::disable("submit_flow")
      }, error = function(e) {
        handleFatalError(paste("Error processing file upload:", e$message))
      })
    })

    # Validate the file and update UI inputs upon successful validation.
    observeEvent(input$validate_flow, {
      req(input$flow_file)
      tryCatch({
        errors <- validate_flow_file(input$flow_file$datapath)

        if (length(errors) > 0) {
          showModal(modalDialog(
            title = "Validation Error",
            pre(paste(errors, collapse = "\n")),
            easyClose = TRUE,
            footer = modalButton("Close")
          ))
          shinyjs::disable("submit_flow")
        } else {
          showModal(modalDialog(
            title = "Validation Successful",
            "The uploaded file has been validated successfully.",
            easyClose = TRUE,
            footer = modalButton("Close")
          ))
          shinyjs::enable("submit_flow")

          # Read inflow and outflow sheets with error handling.
          inflow_data <- tryCatch({
            readxl::read_excel(input$flow_file$datapath, sheet = "inflow1")
          }, error = function(e) {
            handleFatalError(paste("Error reading inflow1 sheet:", e$message))
          })

          outflow_data <- tryCatch({
            readxl::read_excel(input$flow_file$datapath, sheet = "outflow")
          }, error = function(e) {
            handleFatalError(paste("Error reading outflow sheet:", e$message))
          })

          tryCatch({
            # Update date inputs from inflow data.
            min_inflow_date <- min(as.Date(inflow_data$datetime))
            max_inflow_date <- max(as.Date(inflow_data$datetime))
            shinyWidgets::updateAirDateInput(session, "start_date_flow",
                                             value = min_inflow_date,
                                             options = list(minDate = min_inflow_date, maxDate = max_inflow_date))
            # Update date inputs from outflow data.
            min_outflow_date <- min(as.Date(outflow_data$datetime))
            max_outflow_date <- max(as.Date(outflow_data$datetime))
            shinyWidgets::updateAirDateInput(session, "end_date_flow",
                                             value = max_outflow_date,
                                             options = list(minDate = min_outflow_date, maxDate = max_outflow_date))

            # Update time inputs.
            start_time <- format(min(as.POSIXct(inflow_data$datetime)), "%H:%M:%S")
            shinyWidgets::updateTimeInput(session, "start_hour_flow", value = hms::as_hms(start_time))

            end_time <- format(max(as.POSIXct(outflow_data$datetime)), "%H:%M:%S")
            shinyWidgets::updateTimeInput(session, "end_hour_flow", value = hms::as_hms(end_time))
          }, error = function(e) {
            handleFatalError(paste("Error updating date/time inputs:", e$message))
          })
        }
      }, error = function(e) {
        handleFatalError(paste("Error during file validation:", e$message))
      })
    })

    # On submission, show a Calculating modal and update navigation.
    observeEvent(input$submit_flow, {
      tryCatch({
        showModal(modalDialog(
          title = "Calculating",
          "It might take a few minutes, please wait...",
          easyClose = FALSE,
          footer = modalButton("Dismiss")
        ))
        bslib::nav_remove("main_flow", target = "Result")
        bslib::nav_insert(
          "main_flow", target = "Method", select = TRUE,
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
                      style = "margin-top: 0.7rem; font-weight: bold;"
                    ),
                    shinyWidgets::pickerInput(
                      inputId = ns("choose_graph_flow"),
                      choices = NULL,
                      selected = NULL,
                      multiple = TRUE
                    ),
                    shinyWidgets::downloadBttn(ns("download_plot_flow"), "Download plot")
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
                    shinyWidgets::downloadBttn(ns("download_table_flow"), "Download table"),
                    shinyWidgets::downloadBttn(ns("download_table_smc_flow"), "Download table in SMC format")
                  )
                )
              )
            )
          )
        )
      }, error = function(e) {
        handleFatalError(paste("Error processing submit event:", e$message))
      })
    })

    # ----------------------------
    # Data Processing (Submit)
    # ----------------------------

    data_input <- eventReactive(input$submit_flow, {
      req(input$flow_file)
      tryCatch({
        all_data <- read_excel_allsheets(input$flow_file$datapath)
        all_data <- Filter(function(df) nrow(df) > 0, all_data)
        start <- as.POSIXct(paste(input$start_date_flow, as.character(input$start_hour_flow)),
                            format = "%Y-%m-%d %H:%M", tz = 'UTC')
        end   <- as.POSIXct(paste(input$end_date_flow, as.character(input$end_hour_flow)),
                            format = "%Y-%m-%d %H:%M", tz = 'UTC')

        lapply(all_data, function(df) {
          df <- df %>% dplyr::mutate(datetime = as.POSIXct(datetime), tz = 'UTC')
          dplyr::filter(df, datetime >= start, datetime <= end)

        })
      }, error = function(e) {
        handleFatalError(paste("Error processing data input:", e$message))
      })
    })

    payload <- eventReactive(input$submit_flow, {
      req(data_input())
      tryCatch({
        sheets <- data_input()
        required_sheets <- c("inflow1", "inflow2", "bypass", "outflow")
        payload_list <- list()
        for (sheet in required_sheets) {
          if (sheet %in% names(sheets)) {
            df <- sheets[[sheet]] %>% dplyr::select(datetime, flow) %>% dplyr::arrange(datetime)
            payload_list[[sheet]] <- list(
              datetime = df$datetime,
              flow = df$flow,
              time_unit = input$flow_units_flow
            )
          }
        }
        jsonlite::toJSON(payload_list, dataframe = "columns", POSIXt = "ISO8601", auto_unbox = TRUE)
      }, error = function(e) {
        handleFatalError(paste("Error preparing payload:", e$message))
      })
    })

    response <- eventReactive(input$submit_flow, {
      req(payload())
      tryCatch({
        res <- httr::POST(
          "https://nexus.sccwrp.org/bmp_hydrology/api/flow",
          body = payload(),
          encode = "json",
          httr::content_type_json()
        )
        content <- httr::content(res)
        removeModal()
        content
      }, error = function(e) {
        handleFatalError(paste("Error in API request:", e$message))
      })
    })

    statistics <- eventReactive(input$submit_flow, {
      req(response())
      tryCatch({
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
              dplyr::select(flow_type, start_time, peak_flow_rate, runoff_duration, runoff_volume, end_time) %>%
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
      }, error = function(e) {
        handleFatalError(paste("Error processing statistics:", e$message))
      })
    })

    plot_data <- eventReactive(input$submit_flow, {
      req(data_input())
      tryCatch({
        dplyr::bind_rows(
          lapply(seq_along(data_input()), function(i) {
            df <- data_input()[[i]]
            df$flow_type <- names(data_input())[i]
            df
          })
        )
      }, error = function(e) {
        handleFatalError(paste("Error preparing plot data:", e$message))
      })
    })

    # ----------------------------
    # Update Flow Type Picker
    # ----------------------------
    observe({
      req(plot_data())
      tryCatch({
        flow_types <- unique(plot_data()$flow_type)
        shinyWidgets::updatePickerInput(session, "choose_graph_flow",
                                        choices = flow_types,
                                        selected = flow_types)
      }, error = function(e) {
        handleFatalError(paste("Error updating flow type picker:", e$message))
      })
    })

    # ----------------------------
    # Outputs: Plot and Table
    # ----------------------------
    output$flow_plot <- renderPlot({
      req(plot_data())
      tryCatch({
        df <- plot_data()
        if (!is.null(input$choose_graph_flow) && length(input$choose_graph_flow) > 0) {
          df <- df %>% dplyr::filter(flow_type %in% input$choose_graph_flow)
        }
        create_flow_plot(df)
      }, error = function(e) {
        handleFatalError(paste("Error rendering plot:", e$message))
      })
    })

    output$flow_table <- DT::renderDT({
      req(statistics())
      tryCatch({
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
          options = list(dom = 't', paging = FALSE, ordering = FALSE)
        )
      }, error = function(e) {
        handleFatalError(paste("Error rendering table:", e$message))
      })
    })

    # ----------------------------
    # Download Handlers
    # ----------------------------
    output$download_plot_flow <- downloadHandler(
      filename = function() {
        paste0("flow_plot_", Sys.Date(), ".png")
      },
      content = function(file) {
        req(plot_data())
        tryCatch({
          thematic::thematic_local_theme(
            thematic::thematic_theme(
              bg = bslib::bs_get_contrast(bslib::bs_theme(preset = "cosmo"), "secondary"),
              fg = bslib::bs_get_variables(bslib::bs_theme(preset = "cosmo"), "secondary")
            )
          )
          df <- plot_data()
          if (!is.null(input$choose_graph_flow) && length(input$choose_graph_flow) > 0) {
            df <- df %>% dplyr::filter(flow_type %in% input$choose_graph_flow)
          }
          p <- create_flow_plot(df)
          ggplot2::ggsave(file, plot = p, device = "png", width = 8, height = 6)
        }, error = function(e) {
          handleFatalError(paste("Error downloading plot:", e$message))
        })
      }
    )

    output$download_table_flow <- downloadHandler(
      filename = function() {
        paste0("flow_table_", Sys.Date(), ".csv")
      },
      content = function(file) {
        tryCatch({
          df <- statistics() %>%
            dplyr::select(flow_type, peak_flow_rate, runoff_duration, runoff_volume) %>%
            dplyr::rename(
              "Type of flow" = flow_type,
              "Peak flow rate" = peak_flow_rate,
              "Duration of runoff (h)" = runoff_duration,
              "Runoff volume" = runoff_volume
            )
          write.csv(df, file, row.names = FALSE)
        }, error = function(e) {
          handleFatalError(paste("Error downloading table:", e$message))
        })
      }
    )

    output$download_table_smc_flow <- downloadHandler(
      filename = function() {
        paste0("flow_table_smc_", Sys.Date(), ".csv")
      },
      content = function(file) {
        tryCatch({
          df <- statistics() %>%
            dplyr::mutate(
              start_time = as.POSIXct(start_time, format = "%Y-%m-%d %H:%M:%S"),
              end_time = as.POSIXct(end_time, format = "%Y-%m-%d %H:%M:%S")
            ) %>%
            dplyr::mutate(
              monitoringstation = flow_type,
              datestart = as.Date(start_time),
              timestart = format(start_time, "%H:%M:%S"),
              dateend = as.Date(end_time),
              timeend = format(end_time, "%H:%M:%S"),
              volumetotal = runoff_volume,
              volumeunits = input$flow_units_flow,
              peakflowrate = peak_flow_rate,
              peakflowunits = input$flow_units_flow
            ) %>%
            dplyr::select(
              monitoringstation,
              datestart,
              timestart,
              dateend,
              timeend,
              volumetotal,
              volumeunits,
              peakflowrate,
              peakflowunits
            ) %>%
            dplyr::mutate(peakflowunits = gsub("L/s", "lps", peakflowunits))
          write.csv(df, file, row.names = FALSE)
        }, error = function(e) {
          handleFatalError(paste("Error downloading SMC table:", e$message))
        })
      }
    )
  })
}



