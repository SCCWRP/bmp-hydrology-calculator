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
        "ft³/s (cfs)" = "ft3/s"
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
      df$flow_type <- factor(df$flow_type, levels = c("inflow1", "inflow2", "bypass", "outflow"))

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

    # Initialize a reactive flag to track result generation
    result_generated <- reactiveVal(FALSE)

    # Only pop up the modal when the flow_units_flow input changes
    # and a result has already been generated.
    observeEvent(input$flow_units_flow, {
      if (result_generated()) {
        showModal(modalDialog(
          title = "Notice",
          "Changes detected for unit flow. If you had generated the result, you must hit Submit button again to generate the updated result.",
          easyClose = TRUE,
          footer = modalButton("Dismiss")
        ))
      }
    })


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
            size = "l",
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

          # ---------------------------------------------------------------------------
          # 0. Read each sheet separately (hard-coded) ---------------------------------
          # ---------------------------------------------------------------------------
          inflow1  <- tryCatch(readxl::read_excel(input$flow_file$datapath, sheet = "inflow1"),
                               error = function(e) tibble::tibble(datetime = NA)[0, ])

          inflow2  <- tryCatch(readxl::read_excel(input$flow_file$datapath, sheet = "inflow2"),
                               error = function(e) tibble::tibble(datetime = NA)[0, ])

          bypass   <- tryCatch(readxl::read_excel(input$flow_file$datapath, sheet = "bypass"),
                               error = function(e) tibble::tibble(datetime = NA)[0, ])

          outflow  <- tryCatch(readxl::read_excel(input$flow_file$datapath, sheet = "outflow"),
                               error = function(e) tibble::tibble(datetime = NA)[0, ])

          # ---------------------------------------------------------------------------
          # 1. Collect timestamps from any sheet that actually contains data ----------
          # ---------------------------------------------------------------------------
          extract_times <- function(df) {
            if (nrow(df) > 0 && "datetime" %in% names(df)) {
              as.POSIXct(df$datetime, tz = "UTC") |>           # set your TZ if known
                na.omit()
            } else {
              NULL
            }
          }

          all_times <- c(
            extract_times(inflow1),
            extract_times(inflow2),
            extract_times(bypass),
            extract_times(outflow)
          )

          if (length(all_times) == 0) {
            handleFatalError("No valid timestamps found in any of the four sheets."); return()
          }

          # ---------------------------------------------------------------------------
          # 2. Compute global minima / maxima -----------------------------------------
          # ---------------------------------------------------------------------------
          min_ts <- min(all_times)
          max_ts <- max(all_times)

          min_ts <- as.POSIXct(min_ts, origin = "1970-01-01", tz = "UTC")
          max_ts <- as.POSIXct(max_ts, origin = "1970-01-01", tz = "UTC")

          min_date <- as.Date(min_ts)
          max_date <- as.Date(max_ts)

          start_hr <- format(min_ts, "%H:%M:%S")
          end_hr <- format(max_ts, "%H:%M:%S")

          # ---------------------------------------------------------------------------
          # 3. Update widgets ----------------------------------------------------------
          # ---------------------------------------------------------------------------
          shinyWidgets::updateAirDateInput(
            session, "start_date_flow",
            value = min_date,
            options = list(minDate = min_date, maxDate = max_date)
          )

          shinyWidgets::updateAirDateInput(
            session, "end_date_flow",
            value = max_date,
            options = list(minDate = min_date, maxDate = max_date)
          )

          shinyWidgets::updateTimeInput(session, "start_hour_flow",
                                        value = hms::as_hms(start_hr))
          shinyWidgets::updateTimeInput(session, "end_hour_flow",
                                        value = hms::as_hms(end_hr))

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
        result_generated(TRUE)
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
        start <- as.POSIXct(
          paste(input$start_date_flow, as.character(input$start_hour_flow)),
          format = "%Y-%m-%d %H:%M", tz = 'UTC'
        )
        end <- as.POSIXct(
          paste(input$end_date_flow, as.character(input$end_hour_flow)),
          format = "%Y-%m-%d %H:%M", tz = 'UTC'
        )

        # Check if start is before end. If not, pop up a modal and stop further processing.
        if (start >= end) {
          showModal(modalDialog(
            title = "Date Error",
            "The start date must be before the end date.",
            easyClose = TRUE,
            footer = NULL
          ))
          stop("Start date must be before end date.")
        }

        lapply(all_data, function(df) {
          df <- df %>%
            dplyr::mutate(datetime = as.POSIXct(datetime), tz = 'UTC')
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
          httr::content_type_json(),
          config = httr::config(ssl_verifypeer = FALSE)
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
                runoff_duration = round(runoff_duration, 2),
                peak_flow_rate = round(peak_flow_rate, 2),
                runoff_volume = round(runoff_volume, 2)
              ) %>%
              dplyr::select(flow_type, start_time, peak_flow_rate, runoff_duration, runoff_volume, end_time) %>%
              dplyr::mutate(
                start_time = stringr::str_replace(start_time, "T", " "),
                end_time = stringr::str_replace(end_time, "T", " ")
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
        shinyWidgets::updatePickerInput(
          session, "choose_graph_flow",
          choices = flow_types,
          selected = flow_types
        )
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
          dplyr::select(flow_type, runoff_duration, runoff_volume, peak_flow_rate) %>%
          dplyr::rename(
            "Type of flow" = flow_type,
            "Peak flow rate" = peak_flow_rate,
            "Duration of runoff (hr)" = runoff_duration,
            "Runoff volume" = runoff_volume
          )

        # Update column headers to include units
        names(data)[names(data) == "Peak flow rate"] <- paste0("Peak flow rate (", input$flow_units_flow, ")")
        names(data)[names(data) == "Runoff volume"] <- paste0("Runoff volume (", sub("/.*", "", input$flow_units_flow), ")")

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
          ggplot2::ggsave(
            file,
            plot = p +
              scale_x_datetime(date_breaks = "1 day", date_labels = "%Y-%m-%d %H:%M") +
              theme_bw(base_size = 20),
            width = 1920,
            height = 1017,
            units = "px",
            dpi = 93
          )
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

          # Update column headers to include units
          names(df)[names(df) == "Peak flow rate"] <- paste0("Peak flow rate (", input$flow_units_flow, ")")
          names(df)[names(df) == "Runoff volume"] <- paste0("Runoff volume (", sub("/.*", "", input$flow_units_flow), ")")

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
              volumeunits = sub("/.*", "", input$flow_units_flow),  # Extracts only the unit before the slash
              peakflowrate = peak_flow_rate,
              peakflowunits = input$flow_units_flow  # Full unit for peak flow
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



