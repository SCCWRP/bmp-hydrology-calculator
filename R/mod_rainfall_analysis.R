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

    # Step 1: Download Demo Data
    bslib::tooltip(
      span(strong("Step 1: Download Demo Data"), bsicons::bs_icon("question-circle")),
      "Demo Data"
    ),
    shinyWidgets::downloadBttn(ns("download_demo_1min_rainfall"), "Download 1-min demo data"),
    shinyWidgets::downloadBttn(ns("download_demo_timeoftips_rainfall"), "Download time of tips demo data"),

    bslib::tooltip(
      span(strong("Step 2: Download Template"), bsicons::bs_icon("question-circle")),
      "Overwrite the template with your data. See Data Requirements on the Instructions tab."
    ),
    shinyWidgets::downloadBttn(ns("download_template_rainfall"), "Download Template"),

    # Step 2: Upload Rainfall Data
    bslib::tooltip(
      span(strong("Step 3: Upload Rainfall Data"), bsicons::bs_icon("question-circle")),
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
      span(strong("Step 4: Choose a Rainfall Resolution"), bsicons::bs_icon("question-circle")),
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
      title = "Instruction"
    ),
    bslib::nav_panel(
      title = "Method"
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
            bslib::layout_columns(
              col_widths = c(6, 6),
              shinyWidgets::pickerInput(
                inputId = ns("event_selector_rainfall"),
                choices = c(1, 2, 3),
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

    # ----------------------------------------------------------------------------
    # 3. Output: Plot and DataTable
    # ----------------------------------------------------------------------------

    output$rainfall_plot <- renderPlot({
      req(plot_data())
      df <- plot_data()

      # Filter data based on the selected event (using event_selector_rainfall).
      ev <- as.numeric(input$event_selector_rainfall)
      if (!is.null(statistics()) && ev %in% statistics()$event) {
        event_times <- statistics() %>% dplyr::filter(event == ev)
        df <- df %>% dplyr::filter(
          datetime >= event_times$first_rain,
          datetime <= event_times$last_rain
        )
      }

      ggplot2::ggplot(df, ggplot2::aes(x = hours, y = cumsum)) +
        ggplot2::geom_line() +
        ggplot2::labs(
          x = "Elapsed hours from the first rain tip",
          y = paste("Cumulative rainfall (", rain_unit(), ")", sep = ""),
          title = input$title
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text  = ggplot2::element_text(size = 12),
          axis.title = ggplot2::element_text(size = 14, face = "bold"),
          plot.title = ggplot2::element_text(size = 16, face = "bold")
        )
    })

    output$rainfall_table <- DT::renderDT({
      req(statistics())
      data <- statistics() %>%
        dplyr::select(-last_rain, -antecedent_dry_period, -peak_60_min_rainfall_intensity) %>%
        dplyr::mutate(
          first_rain = format(as.POSIXct(first_rain), format = "%Y-%m-%d %H:%M:%S"),
          avg_rainfall_intensity = round(avg_rainfall_intensity, 2),
          peak_5_min_rainfall_intensity = round(peak_5_min_rainfall_intensity, 2),
          peak_10_min_rainfall_intensity = round(peak_10_min_rainfall_intensity, 2)
        )
      if (as.numeric(input$rainfall_resolution) == 0.1) {
        data <- data %>%
          dplyr::rename(
            `Event ID` = event,
            `Storm Date` = first_rain,
            `Total Rainfall (P) (mm)` = total_rainfall,
            `Average Rainfall Intensity (mm/hr)` = avg_rainfall_intensity,
            `Peak 5-min Rainfall Intensity (mm/hr)` = peak_5_min_rainfall_intensity,
            `Peak 10-min Rainfall Intensity (mm/hr)` = peak_10_min_rainfall_intensity
          )
      } else {
        data <- data %>%
          dplyr::rename(
            `Event ID` = event,
            `Storm Date` = first_rain,
            `Total Rainfall (P) (inches)` = total_rainfall,
            `Average Rainfall Intensity (inch/hr)` = avg_rainfall_intensity,
            `Peak 5-min Rainfall Intensity (inch/hr)` = peak_5_min_rainfall_intensity,
            `Peak 10-min Rainfall Intensity (inch/hr)`= peak_10_min_rainfall_intensity
          )
      }
      DT::datatable(
        data,
        options = list(searching = FALSE),
        rownames = FALSE,
        caption = htmltools::tags$caption(
          style = 'caption-side: top; text-align: center; color: black; font-size: 200%;',
          'Statistics of the rainfall data'
        )
      )
    })

    # ----------------------------------------------------------------------------
    # 4. Download Handlers
    # ----------------------------------------------------------------------------
    output$download_demo_1min_rainfall <- downloadHandler(
      filename = "demo_rainfall_1min_data.xlsx",
      content = function(file) {
        file.copy("inst/extdata/demo_rainfall_1min_data.xlsx", file, overwrite = TRUE)
      }
    )

    output$download_demo_timeoftips_rainfall <- downloadHandler(
      filename = "demo_rainfall_timeoftips_data.xlsx",
      content = function(file) {
        file.copy("inst/extdata/demo_rainfall_timeoftips_data.xlsx", file, overwrite = TRUE)
      }
    )

    output$download_template_rainfall <- downloadHandler(
      filename = "rainfall_template.xlsx",
      content = function(file) {
        file.copy("rainfall_template.xlsx", file, overwrite = TRUE)
      }
    )

    output$download_rainfall_plot <- downloadHandler(
      filename = function() {
        "downloaded_plot.png"
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
