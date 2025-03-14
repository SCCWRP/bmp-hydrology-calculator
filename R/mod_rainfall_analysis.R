#' rainfall_analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @importFrom shiny NS tagList
mod_rainfall_analysis_ui <- function(id) {
  ns <- NS(id)

  sidebar <- bslib::sidebar(
    width = "20%",
    open = "always",
    class = "html-fill-container",

    # Step 1: Upload Rainfall Data
    bslib::tooltip(
      span(strong("Step 1: Upload data"), bsicons::bs_icon("question-circle")),
      "Expects a single .xlsx file. See Data Requirements for more info."
    ),
    fileInput(
      ns("rainfall_file"),
      label = NULL,
      multiple = FALSE,
      accept = ".xlsx"
    ),

    # Step 2: Validate Data
    bslib::tooltip(
      span(strong("Step 2: Validate data"), bsicons::bs_icon("question-circle")),
      "Data must be validated before proceeding."
    ),
    shinyjs::disabled(
      shinyWidgets::actionBttn(ns("validate_rainfall"), "Validate data")
    ),

    # Step 3: Choose a Rainfall Resolution
    bslib::tooltip(
      span(strong("Step 3: Choose a rainfall resolution"), bsicons::bs_icon("question-circle")),
      "Select the resolution for the rainfall data."
    ),
    selectInput(
      ns("rainfall_resolution"),
      label = NULL,
      choices = c("0.01 inch" = 0.01, "0.1 mm" = 0.1),
      selected = 0.01
    ),

    # Step 4: Optional Graph Title
    textInput(
      ns("title"),
      label = "Input a title for the graph (optional)",
      placeholder = NULL,
      value = "",
      width = "100%"
    ),

    # Step 5: Submit Button (disabled by default; enable after validation passes)
    bslib::card_body(
      bslib::tooltip(
        span(strong("Step 4: Submit data"), bsicons::bs_icon("question-circle")),
        "Submit data when validation is successful."
      ),
      shinyjs::disabled(shinyWidgets::actionBttn(ns("submit_rainfall"), "Submit"))
    )
  )
  main_panel <- bslib::navset_card_underline(
    id = ns("main_rainfall"),
    bslib::nav_panel(
      title = "Instruction",
      mod_rainfall_instruction_ui("rainfall_instruction")
    ),
    bslib::nav_panel(
      title = "Method",
      mod_rainfall_method_ui("rainfall_method")
    )
  )
  bslib::page_sidebar(
    sidebar = sidebar,
    main_panel
  )
}




# Server module for Rainfall Analysis
#' @import dplyr
mod_rainfall_analysis_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Helper to create the rainfall plot.
    create_rainfall_plot <- function(df) {
      ggplot2::ggplot(df, ggplot2::aes(x = hours, y = cumsum)) +
        ggplot2::geom_line(color = "steelblue", size = 1.5) +
        ggplot2::labs(
          x = "Elapsed hours from the first rain tip",
          y = paste("Cumulative rainfall (", rain_unit(), ")", sep = ""),
          title = input$title
        )
    }

    # ----------------------------------------------------------------------------
    # 1. Enable Validate Button When a File is Uploaded (disable Submit until validated)
    # ----------------------------------------------------------------------------
    observeEvent(input$rainfall_file, {
      tryCatch({
        if (!is.null(input$rainfall_file)) {
          shinyjs::enable("validate_rainfall")
          shinyjs::disable("submit_rainfall")
        } else {
          shinyjs::disable("validate_rainfall")
          shinyjs::disable("submit_rainfall")
        }
      }, error = function(e) {
        handleFatalError(paste("Error processing file upload:", e$message))
      })
    })

    # ----------------------------------------------------------------------------
    # 2. Validate Uploaded File
    # ----------------------------------------------------------------------------
    observeEvent(input$validate_rainfall, {
      req(input$rainfall_file)
      tryCatch({
        errors <- validate_rainfall_file(input$rainfall_file$datapath)
        if (length(errors) > 0) {
          showModal(modalDialog(
            title = "Validation Error",
            pre(paste(errors, collapse = "\n")),
            easyClose = TRUE,
            footer = modalButton("Close")
          ))
          shinyjs::disable("submit_rainfall")
        } else {
          showModal(modalDialog(
            title = "Validation Successful",
            "The uploaded file has been validated successfully.",
            easyClose = TRUE,
            footer = modalButton("Close")
          ))
          shinyjs::enable("submit_rainfall")
        }
      }, error = function(e) {
        handleFatalError(paste("Error during rainfall file validation:", e$message))
      })
    })

    # ----------------------------------------------------------------------------
    # 3. Navigate to Result on Submit
    # ----------------------------------------------------------------------------
    observeEvent(input$submit_rainfall, {
      tryCatch({
        bslib::nav_remove("main_rainfall", target = "Result")
        bslib::nav_insert(
          "main_rainfall", target = "Method", select = TRUE,
          bslib::nav_panel(
            title = "Result",
            bslib::layout_columns(
              col_widths = 12,
              row_heights = c(1, 1),
              # --- Upper Half: Plot and Control Buttons ---
              bslib::card(
                full_screen = TRUE,
                bslib::card_body(
                  plotOutput(ns("rainfall_plot"), height = "300px")
                ),
                bslib::card_footer(
                  fillable = TRUE,
                  bslib::layout_columns(
                    col_widths = c(2, 6, 4),
                    tags$label(
                      "Choose a storm event:",
                      style = "margin-top: 0.7rem; font-weight: bold;"
                    ),
                    shinyWidgets::pickerInput(
                      inputId = ns("event_selector_rainfall"),
                      choices = NULL,
                      multiple = FALSE
                    ),
                    shinyWidgets::downloadBttn(ns("download_plot_rainfall"), "Download plot")
                  )
                )
              ),
              # --- Lower Half: Table Output ---
              bslib::card(
                bslib::card_body(
                  DT::dataTableOutput(ns("rainfall_table"))
                ),
                bslib::card_footer(
                  fillable = TRUE,
                  bslib::layout_columns(
                    col_widths = c(6, 6),
                    shinyWidgets::downloadBttn(ns("download_table_rainfall"), "Download table"),
                    shinyWidgets::downloadBttn(ns("download_table_smc_rainfall"), "Download table in SMC format")
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

    # ----------------------------------------------------------------------------
    # 4. Data Processing Triggered by the Submit Button
    # ----------------------------------------------------------------------------
    data_input <- eventReactive(input$submit_rainfall, {
      req(input$rainfall_file)
      tryCatch({
        user_data <- readxl::read_excel(input$rainfall_file$datapath, sheet = "rainfall_data")
        user_data <- user_data %>% dplyr::select(datetime, rain)
        user_data
      }, error = function(e) {
        handleFatalError(paste("Error reading rainfall file:", e$message))
      })
    })

    payload <- eventReactive(input$submit_rainfall, {
      req(data_input())
      tryCatch({
        user_data <- data_input() %>% dplyr::arrange(datetime)
        list(rain = user_data) %>%
          jsonlite::toJSON(dataframe = "columns", POSIXt = "ISO8601", auto_unbox = TRUE)
      }, error = function(e) {
        handleFatalError(paste("Error preparing payload:", e$message))
      })
    })

    response <- eventReactive(input$submit_rainfall, {
      req(payload())
      tryCatch({
        showModal(modalDialog("Calculating...", footer = NULL))
        res <- httr::POST(
          "https://nexus.sccwrp.org/bmp_hydrology/api/rain",
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

    statistics <- eventReactive(input$submit_rainfall, {
      req(response())
      tryCatch({
        response()$statistics %>%
          tibble::as_tibble() %>%
          tidyr::unnest(
            cols = c(
              first_rain,
              last_rain,
              total_rainfall,
              avg_rainfall_intensity,
              peak_5_min_rainfall_intensity,
              peak_10_min_rainfall_intensity,
              peak_60_min_rainfall_intensity,
              antecedent_dry_period
            )
          ) %>%
          dplyr::arrange(first_rain) %>%
          dplyr::mutate(
            first_rain = lubridate::as_datetime(first_rain),
            last_rain = lubridate::as_datetime(last_rain),
            event = dplyr::row_number()
          ) %>%
          dplyr::select(
            event,
            first_rain,
            last_rain,
            total_rainfall,
            avg_rainfall_intensity,
            peak_5_min_rainfall_intensity,
            peak_10_min_rainfall_intensity,
            antecedent_dry_period,
            peak_60_min_rainfall_intensity
          )
      }, error = function(e) {
        handleFatalError(paste("Error processing statistics:", e$message))
      })
    })

    plot_data <- eventReactive(input$submit_rainfall, {
      req(data_input())
      tryCatch({
        data_input() %>%
          dplyr::mutate(
            cumsum = cumsum(rain),
            hours = as.numeric(difftime(datetime, min(datetime), units = "hours"))
          )
      }, error = function(e) {
        handleFatalError(paste("Error processing plot data:", e$message))
      })
    })

    rain_unit <- reactive({
      if (as.numeric(input$rainfall_resolution) == 0.01) {
        "inch"
      } else {
        "mm"
      }
    })

    # ----------------------------------------------------------------------------
    # 5. Update Storm Event Selector
    # ----------------------------------------------------------------------------
    observe({
      req(statistics())
      tryCatch({
        event_ids <- as.character(statistics()$event)
        event_ids <- c("All events", event_ids)
        shinyWidgets::updatePickerInput(
          session = session,
          inputId = "event_selector_rainfall",
          choices = event_ids,
          selected = event_ids[1]
        )
      }, error = function(e) {
        handleFatalError(paste("Error updating event selector:", e$message))
      })
    }) |> bindEvent(statistics())

    # ----------------------------------------------------------------------------
    # 6. Output: Plot and DataTable
    # ----------------------------------------------------------------------------
    output$rainfall_plot <- renderPlot({
      req(plot_data())
      tryCatch({
        df <- plot_data()
        selected_event <- input$event_selector_rainfall
        if (!is.null(selected_event) && selected_event != "All events") {
          ev <- as.numeric(selected_event)
          if (!is.null(statistics()) && ev %in% statistics()$event) {
            event_times <- statistics() %>% dplyr::filter(event == ev)
            df <- df %>% dplyr::filter(
              datetime >= event_times$first_rain,
              datetime <= event_times$last_rain
            )
          }
        }
        create_rainfall_plot(df)
      }, error = function(e) {
        handleFatalError(paste("Error rendering rainfall plot:", e$message))
      })
    })

    output$rainfall_table <- DT::renderDT({
      req(statistics())
      tryCatch({
        data <- statistics() %>%
          dplyr::select(-last_rain) %>%
          dplyr::mutate(
            first_rain = format(as.POSIXct(first_rain), format = "%Y-%m-%d %H:%M:%S"),
            total_rainfall = round(total_rainfall, 2),
            avg_rainfall_intensity = round(avg_rainfall_intensity, 2),
            peak_5_min_rainfall_intensity = round(peak_5_min_rainfall_intensity, 2),
            peak_10_min_rainfall_intensity = round(peak_10_min_rainfall_intensity, 2),
            peak_60_min_rainfall_intensity = round(peak_60_min_rainfall_intensity, 2),
            antecedent_dry_period = round(antecedent_dry_period, 2)
          )
        if (as.numeric(input$rainfall_resolution) == 0.1) {
          data <- data %>%
            dplyr::rename(
              `Event ID` = event,
              `Storm Date` = first_rain,
              `Total Rainfall (mm)` = total_rainfall,
              `Average Rainfall Intensity (mm/hr)` = avg_rainfall_intensity,
              `Peak 5-min Rainfall Intensity (mm/hr)` = peak_5_min_rainfall_intensity,
              `Peak 10-min Rainfall Intensity (mm/hr)` = peak_10_min_rainfall_intensity,
              `Peak 60-min Rainfall Intensity (mm/hr)` = peak_60_min_rainfall_intensity,
              `Antecedent Dry Period (hours)` = antecedent_dry_period
            ) %>%
            dplyr::select(
              `Event ID`,
              `Storm Date`,
              `Total Rainfall (mm)`,
              `Average Rainfall Intensity (mm/hr)`,
              `Peak 5-min Rainfall Intensity (mm/hr)`,
              `Peak 10-min Rainfall Intensity (mm/hr)`,
              `Peak 60-min Rainfall Intensity (mm/hr)`,
              `Antecedent Dry Period (hours)`
            )
        } else {
          data <- data %>%
            dplyr::rename(
              `Event ID` = event,
              `Storm Date` = first_rain,
              `Total Rainfall (in)` = total_rainfall,
              `Average Rainfall Intensity (in/hr)` = avg_rainfall_intensity,
              `Peak 5-min Rainfall Intensity (in/hr)` = peak_5_min_rainfall_intensity,
              `Peak 10-min Rainfall Intensity (in/hr)` = peak_10_min_rainfall_intensity,
              `Peak 60-min Rainfall Intensity (in/hr)` = peak_60_min_rainfall_intensity,
              `Antecedent Dry Period (hours)` = antecedent_dry_period
            ) %>%
            dplyr::select(
              `Event ID`,
              `Storm Date`,
              `Total Rainfall (in)`,
              `Average Rainfall Intensity (in/hr)`,
              `Peak 5-min Rainfall Intensity (in/hr)`,
              `Peak 10-min Rainfall Intensity (in/hr)`,
              `Peak 60-min Rainfall Intensity (in/hr)`,
              `Antecedent Dry Period (hours)`
            )
        }
        DT::datatable(
          data,
          rownames = FALSE,
          options = list(dom = 't', paging = FALSE, ordering = FALSE)
        )
      }, error = function(e) {
        handleFatalError(paste("Error rendering rainfall table:", e$message))
      })
    })

    # ----------------------------------------------------------------------------
    # 7. Download Handlers
    # ----------------------------------------------------------------------------
    output$download_plot_rainfall <- downloadHandler(
      filename = function() {
        paste0("rainfall_plot_", Sys.Date(), ".png")
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
          selected_event <- input$event_selector_rainfall
          if (!is.null(selected_event) && selected_event != "All events") {
            ev <- as.numeric(selected_event)
            if (!is.null(statistics()) && ev %in% statistics()$event) {
              event_times <- statistics() %>% dplyr::filter(event == ev)
              df <- df %>% dplyr::filter(
                datetime >= event_times$first_rain,
                datetime <= event_times$last_rain
              )
            }
          }
          p <- create_rainfall_plot(df)
          ggplot2::ggsave(file, plot = p, device = "png", width = 8, height = 6)
        }, error = function(e) {
          handleFatalError(paste("Error downloading rainfall plot:", e$message))
        })
      }
    )

    output$download_table_rainfall <- downloadHandler(
      filename = function() {
        if (as.numeric(input$rainfall_resolution) == 0.1) {
          paste0("rainfall_table_mm_", Sys.Date(), ".csv")
        } else {
          paste0("rainfall_table_in_", Sys.Date(), ".csv")
        }
      },
      content = function(file) {
        tryCatch({
          data <- statistics() %>%
            dplyr::select(-last_rain) %>%
            dplyr::mutate(
              first_rain = format(as.POSIXct(first_rain), format = "%Y-%m-%d %H:%M:%S"),
              total_rainfall = round(total_rainfall, 2),
              avg_rainfall_intensity = round(avg_rainfall_intensity, 2),
              peak_5_min_rainfall_intensity = round(peak_5_min_rainfall_intensity, 2),
              peak_10_min_rainfall_intensity = round(peak_10_min_rainfall_intensity, 2),
              peak_60_min_rainfall_intensity = round(peak_60_min_rainfall_intensity, 2),
              antecedent_dry_period = round(antecedent_dry_period, 2)
            )
          if (as.numeric(input$rainfall_resolution) == 0.1) {
            data <- data %>%
              dplyr::rename(
                `Event ID` = event,
                `Storm Date` = first_rain,
                `Total Rainfall (mm)` = total_rainfall,
                `Average Rainfall Intensity (mm/hr)` = avg_rainfall_intensity,
                `Peak 5-min Rainfall Intensity (mm/hr)` = peak_5_min_rainfall_intensity,
                `Peak 10-min Rainfall Intensity (mm/hr)` = peak_10_min_rainfall_intensity,
                `Peak 60-min Rainfall Intensity (mm/hr)` = peak_60_min_rainfall_intensity,
                `Antecedent Dry Period (hours)` = antecedent_dry_period
              ) %>%
              dplyr::select(
                `Event ID`,
                `Storm Date`,
                `Total Rainfall (mm)`,
                `Average Rainfall Intensity (mm/hr)`,
                `Peak 5-min Rainfall Intensity (mm/hr)`,
                `Peak 10-min Rainfall Intensity (mm/hr)`,
                `Peak 60-min Rainfall Intensity (mm/hr)`,
                `Antecedent Dry Period (hours)`
              )
          } else {
            data <- data %>%
              dplyr::rename(
                `Event ID` = event,
                `Storm Date` = first_rain,
                `Total Rainfall (in)` = total_rainfall,
                `Average Rainfall Intensity (in/hr)` = avg_rainfall_intensity,
                `Peak 5-min Rainfall Intensity (in/hr)` = peak_5_min_rainfall_intensity,
                `Peak 10-min Rainfall Intensity (in/hr)` = peak_10_min_rainfall_intensity,
                `Peak 60-min Rainfall Intensity (in/hr)` = peak_60_min_rainfall_intensity,
                `Antecedent Dry Period (hours)` = antecedent_dry_period
              ) %>%
              dplyr::select(
                `Event ID`,
                `Storm Date`,
                `Total Rainfall (in)`,
                `Average Rainfall Intensity (in/hr)`,
                `Peak 5-min Rainfall Intensity (in/hr)`,
                `Peak 10-min Rainfall Intensity (in/hr)`,
                `Peak 60-min Rainfall Intensity (in/hr)`,
                `Antecedent Dry Period (hours)`
              )
          }
          write.csv(data, file, row.names = FALSE)
        }, error = function(e) {
          handleFatalError(paste("Error downloading rainfall table:", e$message))
        })
      }
    )

    output$download_table_smc_rainfall <- downloadHandler(
      filename = function() {
        if (as.numeric(input$rainfall_resolution) == 0.1) {
          paste0("rainfall_table_smc_mm_", Sys.Date(), ".csv")
        } else {
          paste0("rainfall_table_smc_in_", Sys.Date(), ".csv")
        }
      },
      content = function(file) {
        tryCatch({
          df <- statistics()
          if (as.numeric(input$rainfall_resolution) == 0.1) {
            totaldepthunits <- 'mm'
            onehourpeakrateunit <- 'mm/hr'
          } else {
            totaldepthunits <- 'inch'
            onehourpeakrateunit <- 'inch/hr'
          }
          df <- df %>%
            dplyr::mutate(
              eventid = event,
              startdate = as.Date(first_rain),
              starttime = format(first_rain, "%H:%M:%S"),
              enddate = as.Date(last_rain),
              endtime = format(last_rain, "%H:%M:%S"),
              totaldepth = total_rainfall,
              totaldepthunits = totaldepthunits,
              onehourpeakrate = peak_60_min_rainfall_intensity,
              onehourpeakrateunit = onehourpeakrateunit,
              antecedentdryperiod = antecedent_dry_period
            ) %>%
            dplyr::select(
              eventid,
              startdate,
              starttime,
              enddate,
              endtime,
              totaldepth,
              totaldepthunits,
              onehourpeakrate,
              onehourpeakrateunit,
              antecedentdryperiod
            ) %>%
            dplyr::rename(antecedentdryperiod_days = antecedentdryperiod)
          write.csv(df, file, row.names = FALSE)
        }, error = function(e) {
          handleFatalError(paste("Error downloading rainfall SMC table:", e$message))
        })
      }
    )
  })
}


