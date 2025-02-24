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

    # Step 2: Upload Rainfall Data
    bslib::tooltip(
      span(strong("Step 1: Upload Data"), bsicons::bs_icon("question-circle")),
      "Upload rainfall data (.xlsx file)."
    ),
    fileInput(
      ns("rainfall_file"),
      label = "Upload rainfall data (.xlsx file)",
      multiple = FALSE,
      accept = ".xlsx"
    ),

    # Step 3: Choose a Rainfall Resolution
    bslib::tooltip(
      span(strong("Step 2: Choose a Rainfall Resolution"), bsicons::bs_icon("question-circle")),
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

    # Submit Button (disabled by default)
    bslib::card_body(
      shinyjs::disabled(shinyWidgets::actionBttn(ns("submit_rainfall"), "Submit"))
    )
  )

  main_panel <- bslib::page_navbar(
    id = ns("main_rainfall"),
    padding = 0,
    bslib::nav_panel(
      title = "Instruction",
      mod_rainfall_instruction_ui("rainfall_instruction")
    ),
    bslib::nav_panel(
      title = "Method",
      mod_rainfall_method_ui("rainfall_method")
    ),
    bslib::nav_panel(
      title = "Result",
      bslib::layout_columns(
        col_widths = 12,
        row_heights = c(1, 1),  # Two rows of equal height
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
                style = "margin-top: 0.7rem; font-weight: bold;"  # Bold text
              ),
              shinyWidgets::pickerInput(
                inputId = ns("event_selector_rainfall"),
                choices = NULL,
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
          ),
          bslib::card_footer(
            fillable = TRUE,
            bslib::layout_columns(
              col_widths = c(6, 6),
              shinyWidgets::actionBttn(ns("download_rainfall_table"), "Download Table",
                                       icon = bsicons::bs_icon("download")),
              shinyWidgets::actionBttn(ns("download_rainfall_table_smc"), "Download Table in SMC Format",
                                       icon = bsicons::bs_icon("download")),
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




# Server module for Rainfall Analysis
#' @import dplyr
mod_rainfall_analysis_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ----------------------------------------------------------------------------
    # 1. Enable the Submit Button When a File is Uploaded
    # ----------------------------------------------------------------------------
    observeEvent(input$rainfall_file, {
      if (!is.null(input$rainfall_file)) {
        shinyjs::enable("submit_rainfall")
      } else {
        shinyjs::disable("submit_rainfall")
      }
      #updateNavbarPage(session, "main_rainfall", selected = "Result")
    })
    observeEvent(input$submit_rainfall, {
      updateNavbarPage(session, "main_rainfall", selected = "Result")
    })

    # ----------------------------------------------------------------------------
    # 2. Data Processing Triggered by the Submit Button
    # ----------------------------------------------------------------------------

    # Read in the uploaded rainfall data from the "rainfall_data" sheet.
    data_input <- eventReactive(input$submit_rainfall, {
      print("user click")
      req(input$rainfall_file)

      user_data <- readxl::read_excel(input$rainfall_file$datapath, sheet = "rainfall_data")
      user_data <- user_data %>% dplyr::select(datetime, rain)
      user_data
    })

    # Build the JSON payload for the API call.
    payload <- eventReactive(input$submit_rainfall, {
      req(data_input())
      user_data <- data_input() %>% dplyr::arrange(datetime)
      list(rain = user_data) %>%
        jsonlite::toJSON(dataframe = "columns", POSIXt = "ISO8601", auto_unbox = TRUE)
    })

    # Call the API and return its response.
    response <- eventReactive(input$submit_rainfall, {
      req(payload())
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
    })

    # Process the returned statistics.
    statistics <- eventReactive(input$submit_rainfall, {
      req(response())
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
    })

    # Prepare data for the cumulative rainfall plot.
    plot_data <- eventReactive(input$submit_rainfall, {
      req(data_input())
      data_input() %>%
        dplyr::mutate(
          cumsum = cumsum(rain),
          hours = as.numeric(difftime(datetime, min(datetime), units = "hours"))
        )
    })

    # Determine the rainfall unit based on the selected resolution.
    rain_unit <- reactive({
      if (as.numeric(input$rainfall_resolution) == 0.01) {
        "inch"
      } else {
        "mm"
      }
    })

    observe({
      req(statistics())  # Only proceed if statistics() is ready
      event_ids <- as.character(statistics()$event)
      # Add "All events" as the first choice
      event_ids <- c("All events", event_ids)
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "event_selector_rainfall",
        choices = event_ids,
        selected = event_ids[1]  # default to "All events"
      )
    }) |> bindEvent(statistics())



    # ----------------------------------------------------------------------------
    # 3. Output: Plot and DataTable
    # ----------------------------------------------------------------------------

    output$rainfall_plot <- renderPlot({
      req(plot_data())
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


      ggplot2::ggplot(df, ggplot2::aes(x = hours, y = cumsum)) +
        ggplot2::geom_line(color = "steelblue", size = 1.5) +
        ggplot2::labs(
          x = "Elapsed hours from the first rain tip",
          y = paste("Cumulative rainfall (", rain_unit(), ")", sep = ""),
          title = input$title
        )
    })


    output$rainfall_table <- DT::renderDT({
      req(statistics())

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
          )
      } else {
        data <- data %>%
          dplyr::rename(
            `Event ID` = event,
            `Storm Date` = first_rain,
            `Total Rainfall (in)` = total_rainfall,
            `Average Rainfall Intensity (in/hr)` = avg_rainfall_intensity,
            `Peak 5-min Rainfall Intensity (in/hr)` = peak_5_min_rainfall_intensity,
            `Peak 10-min Rainfall Intensity (in/hr)`= peak_10_min_rainfall_intensity,
            `Peak 60-min Rainfall Intensity (in/hr)`= peak_60_min_rainfall_intensity,
            `Antecedent Dry Period (hours)` = antecedent_dry_period
          )
      }


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

    output$download_rainfall_plot <- downloadHandler(
      filename = function() {
        "downloaded_plot.png"
      },
      content = function(file) {
        df <- plot_data()
        selected_event <- input$event_selector_rainfall

        # Only filter if a specific event is selected.
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

        p <- ggplot2::ggplot(df, ggplot2::aes(x = hours, y = cumsum)) +
          ggplot2::geom_line(color = "steelblue", size = 1.5) +
          ggplot2::labs(
            x = "Elapsed hours from the first rain tip",
            y = paste("Cumulative rainfall (", rain_unit(), ")", sep = ""),
            title = input$title
          )

        ggplot2::ggsave(
          file,
          plot = p,
          device = "png"
        )
      }
    )

  })
}
