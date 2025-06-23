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
    width = "20%",
    open = "always",
    class = "html-fill-container",
    bslib::tooltip(
      span(strong("Step 1: Upload data"), bsicons::bs_icon("question-circle")),
      "Expects a single .xlsx file. See Data Requirements for more info."
    ),
    fileInput(
      ns("file"),
      label = NULL,
      multiple = FALSE,
      accept = ".xlsx"
    ) |>
      bslib::as_fillable_container(style = "overflow-y:auto", max_height = "200px"),
    bslib::tooltip(
      span(strong("Step 2: Validate data"), bsicons::bs_icon("question-circle")),
      "Data must be validated before proceeding."),
    shinyjs::disabled(
      shinyWidgets::actionBttn(ns("validate_infiltration"), "Validate data")
    ),
    bslib::tooltip(
      span(strong("Step 3: Select depth's unit of the data"), bsicons::bs_icon("question-circle")),
      "Note that the rate will be <your-unit>/hr"
    ),
    shinyjs::disabled(
      selectInput(
       ns("depth_unit_infiltration"),
       "Depth Unit",
       choices = c("mm", "cm", "in"),
       label = NULL
      )
    ),
    bslib::tooltip(
      span(strong("Step 4: Submit data"), bsicons::bs_icon("question-circle")),
      "Submit data when validation is successful."
    ),
    shinyjs::disabled(shinyWidgets::actionBttn(ns("submit_infiltration"), "Submit")),
    # bslib::card_body(
    #   bslib::tooltip(
    #     span(strong("Constants for smoothing and regression", bsicons::bs_icon("question-circle"))),
    #     "These numbers are for informative only. They are not adjustable by the user for now.
    #     Hover on the constants for more information."
    #   ),
    #   shinyjs::disabled(
    #     bslib::tooltip(
    #       numericInput(
    #         inputId = ns("smoothing_window"),
    #         label = "Smoothing window (min)",
    #         value = 5
    #       ),
    #       "5 minute window for median filter"
    #     )
    #   ),
    #   shinyjs::disabled(
    #     bslib::tooltip(
    #       numericInput(
    #         inputId = ns("regression_window"),
    #         label = "Regression window",
    #         value = 720
    #       ),
    #       "12 hour window for fitted regression"
    #     )
    #   ),
    #   shinyjs::disabled(
    #     bslib::tooltip(
    #       numericInput(
    #         inputId = ns("regression_threshold"),
    #         label = "Regression threshold",
    #         value = 0.999
    #       ),
    #       "Regression tolerance can be very high due to smoothness of fit"
    #     )
    #   )
    # )
  )

  main_panel <- bslib::navset_card_underline(
      id = ns("main_infiltration"),
      bslib::nav_panel(
        title = "Instruction",
        mod_infiltration_instruction_ui("infiltration_instruction")
      ),
      bslib::nav_panel(
        title = "Method",
        mod_infiltration_method_ui("infiltration_method")
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
#' infiltration_analysis Server Functions
#'
#' @noRd
mod_infiltration_analysis_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values to store validated data and analysis results (dictionary keyed by sheet names)
    validatedData <- reactiveVal(NULL)
    analysisResults <- reactiveVal(NULL)

    observeEvent(input$file, {
      shinyjs::enable("validate_infiltration")
      shinyjs::enable("depth_unit_infiltration")
    })

    observeEvent(input$validate_infiltration, {
      req(input$file)

      # Run validation.
      result <- validate_infiltration_file(input$file$datapath)

      if (length(result$errors) > 0) {
        # Combine error messages from all sheets.
        error_messages <- ""
        for (sheet in names(result$errors)) {
          error_messages <- paste0(
            error_messages,
            paste(result$errors[[sheet]], collapse = "\n"),
            "\n"
          )
        }
        showModal(modalDialog(
          size = "l",
          title = "Validation Error",
          pre(paste(error_messages, collapse = "\n")),,
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        validatedData(NULL)
        shinyjs::disable("submit_infiltration")
      } else {
        validatedData(result$valid_data)
        showModal(modalDialog(
          title = "Validation Successful",
          "The uploaded file has been validated successfully.",
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        shinyjs::enable("submit_infiltration")
      }
    })


    # When the user clicks "Submit", process all sheets via the API.
    observeEvent(input$submit_infiltration, {
      req(validatedData())
      shinyjs::disable("depth_unit_infiltration")
      bslib::nav_remove("main_infiltration", target = "Result")
      bslib::nav_insert(
        "main_infiltration",
        target = "Method",
        select = TRUE,
        bslib::nav_panel(
          title = "Result",
          bslib::layout_columns(
            col_widths = 12,
            row_heights = c(2, 1),
            bslib::card(
              full_screen = FALSE,
              bslib::card_header(
                bslib::layout_columns(
                  col_widths = c(6, 6),
                  # Custom label placed in its own column
                  tags$label(
                    "Choose a storm event to view the result:",
                    `for` = ns("choose_rain_event_infiltration"),
                    class = "form-label",
                    style = "margin-top: 0.7rem; font-weight: bold;"  # Bold text
                  ),
                  # Select input without a label
                  selectInput(
                    ns("choose_rain_event_infiltration"),
                    label = NULL,
                    choices = NULL
                  )
                )
              ),
              bslib::card_body(
                plotOutput(ns("plot_infiltration"), height = "100%")
              ),
              bslib::card_footer(
                bslib::layout_columns(
                  col_widths = c(6, 6),
                  shinyWidgets::downloadBttn(ns("download_plot_infiltration"), "Download this plot"),
                  shinyWidgets::downloadBttn(ns("download_all_plots_infiltration"), "Download all plots")
                )
              )
            ),
            bslib::card(
              bslib::card_body(
                DT::dataTableOutput(ns("table_infiltration"))
              ),
              bslib::card_footer(
                bslib::layout_columns(
                  col_widths = c(6, 6),
                  shinyWidgets::downloadBttn(ns("download_table_infiltration"), "Download table (SMC format)"),
                  shinyWidgets::downloadBttn(ns("download_all_results_table"), "Download table for all results (SMC format)")
                )
              )
            )
          )
        )
      )

      # Pop up a "Calculating" modal.
      showModal(modalDialog(
        title = "Calculating",
        "Please wait while we process the data. This might take a few minutes depending on your data size...",
        footer = NULL,
        easyClose = FALSE
      ))

      valid_data <- validatedData()
      results_list <- list()

      # Loop through each validated sheet.
      for (sheet in names(valid_data)) {
        tryCatch({
          data_df <- valid_data[[sheet]]
          df <- data_df
          df$datetime <- as.character(as.POSIXct(df$datetime, tz = "UTC"))

          # Use constants from the UI.
          SMOOTHING_WINDOW <- 5
          REGRESSION_WINDOW <- 720
          REGRESSION_THRESHOLD <- 0.999

          payload <- list(
            data = df,
            SMOOTHING_WINDOW = SMOOTHING_WINDOW,
            REGRESSION_WINDOW = REGRESSION_WINDOW,
            REGRESSION_THRESHOLD = REGRESSION_THRESHOLD
          )

          payload_json <- jsonlite::toJSON(payload, auto_unbox = TRUE, POSIXt = "ISO8601")
          url <- "https://nexus.sccwrp.org/bmp_hydrology/api/infiltration"
          response <- httr::POST(url,
                                 body = payload_json,
                                 encode = "json",
                                 httr::content_type_json(),
                                 config = httr::config(ssl_verifypeer = FALSE))

          if (httr::status_code(response) != 200) {
            results_list[[sheet]] <- list(error = paste("API request failed with status:", httr::status_code(response)))
            next
          }

          result <- httr::content(response, "parsed")

          ## Process the returned dataframe.
          local_df <- jsonlite::fromJSON(jsonlite::toJSON(result$dataframe), flatten = TRUE)
          local_df$datetime <- as.POSIXct(as.character(local_df$datetime), format = "%Y-%m-%dT%H:%M:%S")
          local_df$datetime <- format(local_df$datetime, "%Y-%m-%d %H:%M:%S")
          names(local_df) <- sub("\\..*$", "", names(local_df))

          # Convert columns starting with "smooth_" to numeric.
          smooth_cols <- grep("^smooth_", names(local_df), value = TRUE)
          local_df[smooth_cols] <- lapply(local_df[smooth_cols], function(x) as.numeric(as.character(x)))

          ## Reshape the smoothed columns for plotting.
          df_long <- local_df |>
            dplyr::select(datetime, dplyr::starts_with("smooth_")) |>
            tidyr::pivot_longer(
              cols = -datetime,
              names_to = "piezometer",
              values_to = "depth"
            ) |>
            dplyr::mutate(
              depth = as.numeric(as.character(depth)),
              piezometer = sub("^smooth_", "", piezometer)
            )
          df_long$datetime <- as.POSIXct(df_long$datetime, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")

          ## Process best-fit line results.
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
                ext_time <- as.POSIXct(ext_time, format = "%a, %d %b %Y %H:%M:%S GMT", tz = "GMT")
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

          ## Prepare a table of metrics.
          metrics_list <- list()
          if (!is.null(calc_results)) {
            for (piez in names(calc_results)) {
              if (!is.null(calc_results[[piez]])) {
                metrics_list[[piez]] <- data.frame(
                  Piezometer        = piez,
                  Infiltration_rate = round(calc_results[[piez]]$infiltration_rate, 2),
                  Duration_hrs      = round(calc_results[[piez]]$delta_x, 2),
                  #Average_depth     = round(calc_results[[piez]]$y_average, 2),
                  stringsAsFactors  = FALSE
                )
              }
            }
          }
          metrics_df <- do.call(rbind, metrics_list)

          ### **Validation Checks for `best_fit_df` and `metrics_df`**
          if (nrow(best_fit_df) == 0 || any(is.infinite(best_fit_df$best_fit)) ||
              is.null(metrics_df) || nrow(metrics_df) == 0 || any(is.infinite(metrics_df$Infiltration_rate))) {

            showModal(modalDialog(
              title = "Unable to Calculate Infiltration Rate",
              paste("Unable to calculate infiltration rate for this sheet:", sheet),
              easyClose = TRUE,
              footer = modalButton("Close")
            ))

            next  # **Skip adding this sheet to results_list and selectInput**
          }

          # Check if there are any -88 values in best_fit
          has_undetermined <- any(best_fit_df$best_fit == -88)
          if (has_undetermined) {
            metrics_df$Infiltration_rate <- "Undetermined"
          }

          # Create base plot
          p <- ggplot2::ggplot() +
            ggplot2::geom_line(
              data = df_long,
              ggplot2::aes(x = datetime, y = depth, color = paste("Original data", piezometer)),
              size = 1.5
            )

          # Add dummy line to include "Regression Fits undetermined" in legend if needed
          if (has_undetermined) {
            p <- p +
              ggplot2::geom_line(
                data = data.frame(
                  datetime = as.POSIXct(NA),
                  best_fit = as.numeric(NA)  # Ensures this is treated as numeric
                ),
                ggplot2::aes(x = datetime, y = best_fit, color = "Regression Fits Undetermined. Inf values detected"),
                linetype = "dashed"
              )
          } else {
            p <- p +
              ggplot2::geom_line(
                data = best_fit_df,
                ggplot2::aes(x = datetime, y = best_fit, color = paste("Regression Fits", piezometer)),
                linetype = "dashed",
                size = 1.5
              )
          }


          # Finalize plot with labels
          p <- p +
            ggplot2::labs(
              title = sheet,
              x = "Datetime",
              y = paste("Depth (", input$depth_unit_infiltration, ")", sep = ""),
              color = "Piezometer"
            )

          # Store the result for this sheet.
          results_list[[sheet]] <- list(plot = p, table = metrics_df)

        }, error = function(e) {
          # Display an error modal if something goes wrong.
          showModal(modalDialog(
            title = paste("Unable to determine the infiltration rate for sheet:", sheet),
            paste("Details:", e$message),
            easyClose = TRUE,
            footer = modalButton("Close")
          ))
          results_list[[sheet]] <- list(error = paste("An error occurred:", e$message))
        })
      }

      # Save all analysis results.
      analysisResults(results_list)

      # Update the dropdown with only successfully processed sheets.
      sheets <- names(results_list)
      updateSelectInput(session, "choose_rain_event_infiltration",
                        choices = sheets, selected = if (length(sheets) > 0) sheets[1] else NULL)

      updateNavbarPage(session, "main_infiltration", selected = "Result")

      removeModal()  # Remove the "Calculating" modal.

    })

    # Reactive expression to get the selected sheet's analysis result.
    selected_result <- reactive({
      req(analysisResults(), input$choose_rain_event_infiltration)
      analysisResults()[[input$choose_rain_event_infiltration]]
    })

    # Render the plot.
    output$plot_infiltration <- renderPlot({
      req(selected_result())
      res <- selected_result()
      if (!is.null(res$error)) {
        plot.new()
        text(0.5, 0.5, res$error)
      } else {
        thematic::thematic_shiny(font = "auto")
        res$plot
      }
    })

    # Render the metrics table.
    output$table_infiltration <- DT::renderDT({
      req(selected_result())
      res <- selected_result()
      if (!is.null(res$error)) {
        DT::datatable(data.frame(Error = res$error))
      } else {
        dt <- res$table
        col_mapping <- c(
          "Piezometer"        = "Piezometer",
          "Infiltration_rate" = paste("Infiltration Rate (", input$depth_unit_infiltration, "/hr)", sep=""),
          "Duration_hrs"      = "Duration (hr)"
          #"Average_depth"     = paste("Average Depth (", input$depth_unit_infiltration, ")", sep="")
        )

        names(dt) <- sapply(names(dt), function(x) {
          if (x %in% names(col_mapping)) col_mapping[[x]] else x
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
      }
    })


    # Download handler for the plot.
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

        ggplot2::ggsave(file, plot = selected_result()$plot + theme_bw(base_size = 20),   width = 1920,
                        height = 1017,
                        units = "px",
                        dpi = 93)

      }
    )
    output$download_plot_infiltration <- downloadHandler(
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
        ggplot2::ggsave(file, plot = selected_result()$plot + theme_bw(base_size = 20), width = 1920,
                        height = 1017,
                        units = "px",
                        dpi = 93)
      }
    )
    output$download_all_plots_infiltration <- downloadHandler(
      filename = function() {
        paste0("all_plots_", Sys.Date(), ".zip")
      },
      content = function(file) {
        req(analysisResults())
        results <- analysisResults()

        # Create a temporary directory to store the PNG files.
        tmpDir <- tempfile("plots_")
        dir.create(tmpDir)

        # Loop through each sheet and save the plot.
        for (sheet in names(results)) {
          res <- results[[sheet]]
          png_file <- file.path(tmpDir, paste0("plot_", sheet, ".png"))

          if (is.null(res$error)) {
            thematic::thematic_local_theme(
              thematic::thematic_theme(
                bg = bslib::bs_get_contrast(bslib::bs_theme(preset = "cosmo"), "secondary"),
                fg = bslib::bs_get_variables(bslib::bs_theme(preset = "cosmo"), "secondary")
              )
            )

            ggplot2::ggsave(png_file, plot = res$plot + theme_bw(base_size = 20),   width = 1920,
                            height = 1017,
                            units = "px",
                            dpi = 93)

          } else {
            # Optionally, create a placeholder image for sheets with errors.
            png(png_file, width = 9.2302 * 100, height = 6.94 * 100)
            plot.new()
            text(0.5, 0.5, paste("Error in", sheet, ":", res$error))
            dev.off()
          }
        }

        # Get the list of all PNG files.
        png_files <- list.files(tmpDir, full.names = TRUE)

        # Zip the files together.
        # Using the zip package (make sure it's installed)
        zip::zipr(zipfile = file, files = png_files, recurse = FALSE)

        # Clean up the temporary directory.
        unlink(tmpDir, recursive = TRUE)
      }
    )

    # --- Combined All Results Table Reactive ---
    all_results_table <- reactive({
      req(analysisResults())
      results <- analysisResults()

      # For each sheet, add a storm_name column and return its table if no error.
      combined_list <- lapply(names(results), function(sheet) {
        res <- results[[sheet]]
        if (is.null(res$error)) {
          dt <- res$table
          dt$storm_name <- sheet
          # Place storm_name as the first column.
          dt <- dt[, c("storm_name", setdiff(names(dt), "storm_name"))]
          return(dt)
        } else {
          return(NULL)
        }
      })

      # Combine the tables from all sheets.
      combined <- do.call(rbind, combined_list)
      combined
    })

    # --- Render Individual Storm Table ---
    output$table_infiltration <- DT::renderDT({
      req(selected_result())
      res <- selected_result()
      if (!is.null(res$error)) {
        DT::datatable(data.frame(Error = res$error))
      } else {
        dt <- res$table
        col_mapping <- c(
          "Piezometer"        = "Piezometer",
          "Infiltration_rate" = paste("Infiltration Rate (", input$depth_unit_infiltration, "/hr)", sep=""),
          "Duration_hrs"      = "Duration (hr)"
          #"Average_depth"     = paste("Average Depth (", input$depth_unit_infiltration, ")", sep="")
        )
        names(dt) <- sapply(names(dt), function(x) {
          if (x %in% names(col_mapping)) col_mapping[[x]] else x
        })
        DT::datatable(
          dt,
          rownames = FALSE,
          options = list(dom = 't', paging = FALSE, ordering = FALSE)
        )
      }
    })

    # --- Render Combined All Results Table ---
    output$table_all_results_infiltration <- DT::renderDT({
      req(all_results_table())
      dt <- all_results_table()
      col_mapping <- c(
        "storm_name"        = "Storm Name",
        "Piezometer"        = "Piezometer",
        "Infiltration_rate" = paste("Infiltration Rate (", input$depth_unit_infiltration, "/hr)", sep=""),
        "Duration_hrs"      = "Duration (hr)"
        #"Average_depth"     = paste("Average Depth (", input$depth_unit_infiltration, ")", sep="")
      )
      names(dt) <- sapply(names(dt), function(x) {
        if (x %in% names(col_mapping)) col_mapping[[x]] else x
      })
      DT::datatable(
        dt,
        rownames = FALSE,
        options = list(dom = 't', paging = FALSE, ordering = FALSE)
      )
    })

    # --- Download Handler for Individual Storm Table ---
    output$download_table_infiltration <- downloadHandler(
      filename = function() {
        paste0("infiltration_table_", Sys.Date(), ".csv")
      },
      content = function(file) {
        dt <- selected_result()$table %>%
          # Rename the columns.
          rename(
            piezometer = Piezometer,
            infiltration_rate = Infiltration_rate,
            duration          = Duration_hrs,
            average_depth     = Average_depth
          ) %>%
          # Add unit columns for each variable.
          mutate(
            infiltration_rate_unit = paste0(input$depth_unit_infiltration, "/hr"),
            duration_unit          = "hr",
            average_depth_unit     = input$depth_unit_infiltration
          ) %>%
          # Reorder columns so that the new unit columns follow their respective measures.
          select(
            piezometer,
            infiltration_rate, infiltration_rate_unit,
            duration, duration_unit,
            average_depth, average_depth_unit,
            everything()
          )

        readr::write_csv(dt, file)
      }
    )


    # --- Download Handler for Combined All Results Table ---
    output$download_all_results_table <- downloadHandler(
      filename = function() {
        paste0("infiltration_table_", Sys.Date(), "_ALL.csv")
      },
      content = function(file) {
        dt <- all_results_table() %>%
          # Rename the columns.
          rename(
            piezometer = Piezometer,
            infiltration_rate = Infiltration_rate,
            duration = Duration_hrs,
            average_depth = Average_depth
          ) %>%
          # Add unit columns for each variable.
          mutate(
            infiltration_rate_unit = paste0(input$depth_unit_infiltration, "/hr"),
            duration_unit = "hr",
            average_depth_unit = input$depth_unit_infiltration
          ) %>%
          # Reorder columns so that the new unit columns follow their respective measures.
          select(
            storm_name, piezometer,
            infiltration_rate, infiltration_rate_unit,
            duration, duration_unit,
            average_depth, average_depth_unit,
            everything()
          )
        readr::write_csv(dt, file)
      }
    )
  })
}
