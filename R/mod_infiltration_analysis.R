#' infiltration_analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS
#' @import ggplot2 readr
mod_infiltration_analysis_ui <- function(id) {
  ns <- NS(id)
  sidebar <- bslib::sidebar(
    width = "17%",
    open = "always",
    class = "html-fill-container",
    bslib::tooltip(
      span(strong("Step 1: Download Demo Data", bsicons::bs_icon("question-circle"))),
      "See demo data for the required format. Depth data provided in the demo data are in centimeters."
    ),
    shinyWidgets::downloadBttn(ns("download_demo_infiltration")),
    bslib::tooltip(
      span(strong("Step 2: Download Template", bsicons::bs_icon("question-circle"))),
      "Overwrite the template with your data."
    ),
    shinyWidgets::downloadBttn(ns("download_template_infiltration")),
    strong("Step 3: Upload Data"),
    fileInput(
      ns("file"),
      "Choose Excel File",
      multiple = FALSE,
      accept = ".xlsx"
    ) |>
      bslib::as_fillable_container(style = "overflow-y:auto", max_height = "200px"),
    bslib::tooltip(
      span(strong("Step 4: Enter the title of the graph", bsicons::bs_icon("question-circle"))),
      "This is optional"
    ),
    textInput(ns("graph_title"), ""),
    bslib::tooltip(
      span(strong("Step 5: Click Submit Button", bsicons::bs_icon("question-circle"))),
      "Check everything before you submit"
    ),
    shinyWidgets::actionBttn(ns("submit_infiltration"), "Submit Data"),
    bslib::card_body(
      bslib::tooltip(
        span(strong("Constants for smoothing and regression", bsicons::bs_icon("question-circle"))),
        "These numbers are for informative only. They are not adjustable by the user for now. Hover on the constants for more information."
      ),
      shinyjs::disabled(
        bslib::tooltip(
          numericInput(
            inputId = ns("smoothing_window"),
            label = "Smoothing Window",
            value = 5
          ),
          "5 minute window for median filter"
        )
      ),
      shinyjs::disabled(
        bslib::tooltip(
          numericInput(
            inputId = ns("regression_window"),
            label = "Regression Window",
            value = 720
          ),
          "12 hour window for fitted regression"
        )
      ),
      shinyjs::disabled(
        bslib::tooltip(
          numericInput(
            inputId = ns("regression_threshold"),
            label = "Regression Threshold",
            value = 0.999
          ),
          "Regression tolerance can be very high due to smoothness of fit"
        )
      )
    )
  )

  main_panel <- bslib::page_navbar(
    id = ns("main_infiltration"),
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
            plotOutput(ns("plot_infiltration"), height = "100%")
            #shinyjs::disabled(downloadButton(ns("download_plot"), "Download Plot"))
          )
        ),
        bslib::card(
          bslib::card_body(
            DT::dataTableOutput(ns("table_infiltration"))
            #shinyjs::disabled(downloadButton(ns("download_table"), "Download Data Table"))
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

#' infiltration_analysis Server Functions
#'
#' @noRd
mod_infiltration_analysis_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Download demo/template handlers
    output$download_demo_infiltration <- downloadHandler(
      filename = "demo_infiltration_data.xlsx",
      content = function(file) {
        file.copy("inst/extdata/demo_infiltration_data.xlsx", file, overwrite = TRUE)
      }
    )

    output$download_template_infiltration <- downloadHandler(
      filename = "template_infiltration.xlsx",
      content = function(file) {
        file.copy("inst/extdata/infiltration_template.xlsx", file, overwrite = TRUE)
      }
    )
    observeEvent(input$file, {
      req(input$file)
      updateNavbarPage(session, "main_infiltration", selected = "Result")
    })

    # --- Trigger file validation on Submit button click ---
    observeEvent(input$submit_infiltration, {
      validated_file()  # This forces the validated_file reactive to run.
    })

    # --- Validated File Reactive Expression ---
    validated_file <- eventReactive(input$submit_infiltration, {
      req(input$file)
      errors <- c()  # error collector
      # Retrieve sheet names
      sheets <- readxl::excel_sheets(input$file$datapath)
      if (length(sheets) != 2 || !all(c("Instructions", "Data") %in% sheets)) {
        errors <- c(errors, "Excel file must have exactly two sheets: 'Instructions' and 'Data'.")
      }

      if (length(errors) > 0) {
        showModal(modalDialog(
          title = "Data Check Errors",
          paste(errors, collapse = "\n"),
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        return(NULL)
      }

      # Read the Data sheet (safe because the sheet names passed validation)
      data_df <- readxl::read_excel(input$file$datapath, sheet = "Data", .name_repair = "minimal")

      # Validate the required "datetime" column
      if (!"datetime" %in% names(data_df)) {
        errors <- c(errors, "- The Data sheet must have a column called 'datetime'.")
      } else {
        if (any(is.na(data_df$datetime))) {
          errors <- c(errors, "- Column 'datetime' must have no missing values.")
        }
        parsed_time <- as.POSIXct(data_df$datetime, tz = "UTC",
                                  tryFormats = c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d"))
        if (all(is.na(parsed_time))) {
          errors <- c(errors, "- Column 'datetime' is not in a valid timestamp format.")
        }
      }

      # Check for duplicate column names
      duplicate_names <- names(data_df)[duplicated(names(data_df))]
      if (length(duplicate_names) > 0) {
        for (name in duplicate_names) {
          errors <- c(errors, paste("- Duplicate column name found:", name))
        }
      }

      # Validate all other columns: they must be numeric and have no missing values.
      other_cols <- setdiff(names(data_df), "datetime")
      for (col in other_cols) {
        if (!is.numeric(data_df[[col]])) {
          converted <- suppressWarnings(as.numeric(data_df[[col]]))
          if (all(is.na(converted))) {
            errors <- c(errors, paste("- Column", col, "must be numeric."))
          } else {
            data_df[[col]] <- converted
          }
        }
        if (any(is.na(data_df[[col]]))) {
          errors <- c(errors, paste("- Column", col, "must have no missing values."))
        }
      }



      # If any errors were found, display them and abort.
      if (length(errors) > 0) {
        showModal(modalDialog(
          title = "Data Errors Report",
          tagList(
            paste(errors, collapse = "\n"),
            p("Please fix the errors and reupload. If you have any questions about the errors, contact us at ",
              a("stormwater@sccwrp.org", href = "mailto:stormwater@sccwrp.org"),
              " and provide a screenshot.")
          ),
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        return(NULL)
      } else {
        # SUCCESS modal with a custom Close button.
        showModal(modalDialog(
          title = "Data Errors Report",
          tagList(
            p("All data checks passed. You may close this window and proceed with the analysis.")),
          easyClose = FALSE,  # Force the user to click the custom button.
          footer = tagList(
            shinyWidgets::actionBttn(ns("close_success_modal"), "Close")
          )
        ))
        return(data_df)
      }
    })
    observeEvent(input$close_success_modal, {
      removeModal()
    })
    # --- Analysis Result Reactive Expression ---
    analysis_result <- eventReactive(input$close_success_modal, {
      data_df <- validated_file()
      if (is.null(data_df)) return(NULL)

      # Prepare the data: convert datetime to character for the API
      df <- data_df
      df$datetime <- as.character(as.POSIXct(df$datetime, tz = "UTC"))

      # Use constants from the UI
      SMOOTHING_WINDOW     <- input$smoothing_window
      REGRESSION_WINDOW    <- input$regression_window
      REGRESSION_THRESHOLD <- input$regression_threshold

      # Build the payload for the API
      payload <- list(
        data = df,
        SMOOTHING_WINDOW     = SMOOTHING_WINDOW,
        REGRESSION_WINDOW    = REGRESSION_WINDOW,
        REGRESSION_THRESHOLD = REGRESSION_THRESHOLD
      )

      payload_json <- jsonlite::toJSON(payload, auto_unbox = TRUE, POSIXt = "ISO8601")

      # Define API endpoint and make the POST request
      url <- "https://nexus.sccwrp.org/bmp_hydrology/api/infiltration"
      response <- httr::POST(url,
                             body = payload_json,
                             encode = "json",
                             httr::content_type_json())

      if (httr::status_code(response) != 200) {
        showModal(modalDialog(
          title = "Error",
          tagList(
            p(paste("API request failed with status:", httr::status_code(response))),
            p("Contact us at ",
              a("stormwater@sccwrp.org", href = "mailto:stormwater@sccwrp.org"),
              " and provide a screenshot.")
          ),
          easyClose = TRUE,
          footer = modalButton("Dismiss")
        ))
        return(NULL)
      }

      # Parse the API response
      result <- httr::content(response, "parsed")

      ## Process the returned dataframe
      local_df <- jsonlite::fromJSON(jsonlite::toJSON(result$dataframe), flatten = TRUE)
      local_df$datetime <- as.POSIXct(as.character(local_df$datetime), format = "%Y-%m-%dT%H:%M:%S")
      local_df$datetime <- format(local_df$datetime, "%Y-%m-%d %H:%M:%S")
      names(local_df) <- sub("\\..*$", "", names(local_df))

      # Dynamically convert all columns starting with "smooth_" to numeric
      smooth_cols <- grep("^smooth_", names(local_df), value = TRUE)
      local_df[smooth_cols] <- lapply(local_df[smooth_cols], function(x) as.numeric(as.character(x)))

      ## Reshape only the smoothed columns for plotting
      df_long <- local_df |>
        dplyr::select(datetime, dplyr::starts_with("smooth_")) |>
        tidyr::pivot_longer(
          cols = -datetime,
          names_to = "piezometer",
          values_to = "depth"
        ) |>
        dplyr::mutate(
          depth = as.numeric(as.character(depth)),
          # Remove the "smooth_" prefix to get the original piezometer name
          piezometer = sub("^smooth_", "", piezometer)
        )
      df_long$datetime <- as.POSIXct(df_long$datetime, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")

      ## Process best-fit line results from calc_results
      calc_results <- result$calc_results
      best_fit_df <- data.frame(
        datetime   = as.POSIXct(character()),
        best_fit   = numeric(),
        piezometer = character(),
        stringsAsFactors = FALSE
      )
      if (!is.null(calc_results)) {
        for (piez in names(calc_results)) {
          if (!is.null(calc_results[[piez]])) {
            ext_time <- unlist(calc_results[[piez]]$extended_time)
            ext_time <- as.POSIXct(ext_time,
                                   format = "%a, %d %b %Y %H:%M:%S GMT", tz = "GMT")
            best_fit_line <- calc_results[[piez]]$best_fit_line
            temp_df <- data.frame(
              datetime   = ext_time,
              best_fit   = as.numeric(unlist(best_fit_line)),
              piezometer = piez,
              stringsAsFactors = FALSE
            )
            best_fit_df <- rbind(best_fit_df, temp_df)
          }
        }
      }

      ## Create the plot using ggplot2
      p <- ggplot2::ggplot() +
        ggplot2::geom_line(
          data = df_long,
          ggplot2::aes(x = datetime, y = depth, color = paste("Original data", piezometer)),
          size = 1.5
        ) +
        { if (nrow(best_fit_df) > 0)
          ggplot2::geom_line(
            data = best_fit_df,
            ggplot2::aes(x = datetime, y = best_fit, color = paste("Regression Fits", piezometer)),
            linetype = "dashed",
            size = 1.5
          )
          else NULL } +
        ggplot2::labs(
          title = input$graph_title,
          x = "Datetime",
          y = "Depth (cm)",
          color = "Piezometer"
        )

      ## Prepare a table of metrics (e.g., infiltration rate, duration, average depth)
      metrics_list <- list()
      if (!is.null(calc_results)) {
        for (piez in names(calc_results)) {
          if (!is.null(calc_results[[piez]])) {
            metrics_list[[piez]] <- data.frame(
              Piezometer        = piez,
              Infiltration_rate = round(calc_results[[piez]]$infiltration_rate, 2),
              Duration_hrs      = round(calc_results[[piez]]$delta_x, 2),
              Average_depth     = round(calc_results[[piez]]$y_average, 2),
              stringsAsFactors  = FALSE
            )
          }
        }
      }
      metrics_df <- do.call(rbind, metrics_list)

      # Return a list with the plot and table
      list(plot = p, table = metrics_df)
    })

    # Render the plot in the UI
    output$plot_infiltration <- renderPlot({

      req(analysis_result())
      thematic::thematic_shiny(font = "auto")
      # Extract the ggplot from your analysis result
      p <- analysis_result()$plot
      p
      # # Define max_time_range. Adjust this as needed, for example, you might obtain it from analysis_result()
      # max_time_range <- if (!is.null(analysis_result()$max_time_range)) {
      #   analysis_result()$max_time_range
      # } else {
      #   24  # default value (in hours)
      # }
      #
      # # Convert the ggplot to an interactive Plotly chart
      # plotly::ggplotly(p, dynamicTicks = TRUE, source = "flow") |>
      #   plotly::config(displayModeBar = FALSE)
      #   # plotly::layout(
      #   #   dragmode = FALSE,
      #   #   xaxis = list(
      #   #     rangeslider = list(visible = TRUE),
      #   #     rangeselector = list(
      #   #       buttons = list(
      #   #         list(
      #   #           count = 1,
      #   #           label = "1 hr",
      #   #           step = "hour",
      #   #           stepmode = "backward",
      #   #           visible = max_time_range > 1
      #   #         ),
      #   #         list(
      #   #           count = 3,
      #   #           label = "3 hr",
      #   #           step = "hour",
      #   #           stepmode = "backward",
      #   #           visible = max_time_range > 3
      #   #         ),
      #   #         list(
      #   #           count = 6,
      #   #           label = "6 hr",
      #   #           step = "hour",
      #   #           stepmode = "backward",
      #   #           visible = max_time_range > 6
      #   #         ),
      #   #         list(
      #   #           count = 12,
      #   #           label = "12 hr",
      #   #           step = "hour",
      #   #           stepmode = "backward",
      #   #           visible = max_time_range > 12
      #   #         ),
      #   #         list(
      #   #           count = 24,
      #   #           label = "24 hr",
      #   #           step = "hour",
      #   #           stepmode = "backward",
      #   #           visible = max_time_range > 24
      #   #         ),
      #   #         list(step = "all")
      #   #       )
      #   #     )
      #   #   )
      #   # )
    })

    # Render the metrics table using DT
    output$table_infiltration <- DT::renderDataTable({
      req(analysis_result())
      dt <- analysis_result()$table

      # Define a dictionary (named vector) for column renaming.
      col_mapping <- c(
        "Piezometer"        = "Piezometer",
        "Infiltration_rate" = "Infiltration Rate (cm/hr)",
        "Duration_hrs"      = "Duration (hrs)",
        "Average_depth"     = "Average Depth (cm)"
      )

      # Rename columns using the mapping dictionary.
      names(dt) <- sapply(names(dt), function(x) {
        if (x %in% names(col_mapping)) {
          col_mapping[[x]]
        } else {
          x
        }
      })

      DT::datatable(
        dt,
        rownames = FALSE,
        options = list(
          dom = 't',
          paging = FALSE,
          ordering = FALSE
        )
      )
    })


    observe({
      req(analysis_result())
      shinyjs::enable("download_plot")
      shinyjs::enable("download_table")
    })

    # Download handler for the plot
    output$download_plot <- downloadHandler(
      filename = function() {
        paste0("plot_", Sys.Date(), ".png")
      },
      content = function(file) {
        thematic::thematic_local_theme(
          thematic::thematic_theme(
            bg = bslib::bs_get_contrast(bslib::bs_theme(preset = "cosmo"), "secondary"),
            fg = bslib::bs_get_variables(bslib::bs_theme(preset = "cosmo"), "secondary")
          )
        )

        # Retrieve the current plot object from your reactive analysis
        plot_obj <- analysis_result()$plot


        # Save the plot to the specified file (using ggplot2::ggsave)
        ggsave(file, plot = hydro_plot, device = "png", height = 6.94, width = 9.2302)
      }
    )

    # Download handler for the data table
    output$download_table <- downloadHandler(
      filename = function() {
        paste0("data_table_", Sys.Date(), ".csv")
      },
      content = function(file) {
        dt <- analysis_result()$table
        # Write the data table to a CSV file
        readr::write_csv(dt, file)
      }
    )



  })
}
