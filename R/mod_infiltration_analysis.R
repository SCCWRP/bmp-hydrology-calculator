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
              bslib::card_body(
                plotOutput(ns("plot_infiltration"), height = "100%")
              ),
              bslib::card_footer(
                bslib::layout_columns(
                  col_widths = c(2, 6, 4),
                  tags$label(
                    "Choose a storm event:",
                    style = "margin-top: 0.7rem; font-weight: bold;"
                  ),
                  shinyWidgets::pickerInput(
                    ns("choose_rain_event_infiltration"),
                    choices = NULL,
                    options = shinyWidgets::pickerOptions(
                      container="body"
                    )
                  ),
                  shinyWidgets::downloadBttn(ns("download_plot_infiltration"), "Download this plot")
                  #shinyWidgets::downloadBttn(ns("download_all_plots_infiltration"), "Download all plots")
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
      showModal(
        modalDialog(
          title = "Calculating",
          tagList(
            p("Please wait while we process the data. This might take a few minutes depending on your data size and the complexity of the algorithm."),
            p("If the computation takes too long, your browser may time out. When this happens, the app will return with no error message but no result and no plot."),
            p("If this happens, and your file contains many sensor columns, please try splitting the file into smaller parts by including fewer sensor columns per file, and submitting them one at a time to reduce processing time."),
            p("Thank you for your patience while we process your data.")
          ),
          footer = NULL,
          easyClose = FALSE
        )
      )



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


          # --- NEW: depth-range check -----------------------------------------------
          depth_range <- max(df_long$depth, na.rm = TRUE) - min(df_long$depth, na.rm = TRUE)

          unit_selected <- input$depth_unit_infiltration
          shallow_threshold <- switch(unit_selected,
                                        "mm" = 2 * 25.4,   # 2 inches → mm
                                        "cm" = 2 * 2.54,   # 2 inches → cm
                                        "in" = 2,          # inches
                                        2)                 # default fallback
          shallow_flag <- depth_range < shallow_threshold
          # -------------------------------------------------


          ## Process best-fit line results.
          calc_results <- result$calc_results
          best_fit_df <- data.frame(
            datetime = as.POSIXct(character()),
            best_fit = numeric(),
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
                  datetime = ext_time,
                  best_fit = as.numeric(unlist(best_fit_line)),
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
                  Piezometer = piez,
                  Infiltration_rate = round(calc_results[[piez]]$infiltration_rate, 2),
                  Duration_hrs = round(calc_results[[piez]]$delta_x, 2),
                  #Average_depth = round(calc_results[[piez]]$y_average, 2),
                  stringsAsFactors = FALSE
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
            metrics_df$Infiltration_rate <- "Insufficient data to determine infiltration rate. Infiltration must occur over at least 1-hr."
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
          results_list[[sheet]] <- list(plot = p, table = metrics_df, best_fit_df = best_fit_df )

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
      shinyWidgets::updatePickerInput(session, "choose_rain_event_infiltration",
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

        ggplot2::ggsave(
          file,
          plot = selected_result()$plot + theme_bw(base_size = 20),
          width = 1920,
          height = 1441,
          units = "px",
          dpi = 93
        )

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
        ggplot2::ggsave(file, plot = selected_result()$plot + theme_bw(base_size = 30) + theme(
          axis.text.x = element_text(size = 25)
        ), width = 1920,
                        height = 1441,
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

            ggplot2::ggsave(
              png_file,
              plot = res$plot + theme_bw(base_size = 20),
              width = 1920,
              height = 1017,
              units = "px",
              dpi = 93
            )

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

    # --------------------------------------------------------------------
    # FULL REPLACEMENT for output$table_infiltration
    # --------------------------------------------------------------------
    output$table_infiltration <- DT::renderDT({
      req(selected_result())
      res <- selected_result()

      # 1 ── Handle per-sheet error ─────────────────────────────────────
      if (!is.null(res$error)) {
        return(DT::datatable(data.frame(Error = res$error)))
      }

      # 2 ── Base data and unit ─────────────────────────────────────────
      dt   <- res$table            # metrics_df
      bf   <- res$best_fit_df      # per-sheet best-fit lines
      unit <- input$depth_unit_infiltration

      # 3 ── Depth-range QA/QC per piezometer (no separate column) ─────
      depth_range_raw <- sapply(dt$Piezometer, function(pz) {
        vals <- bf$best_fit[bf$piezometer == pz]
        if (length(vals) == 0) NA_real_ else diff(range(vals, na.rm = TRUE))
      })

      depth_thresh <- switch(                   # 2-inch threshold in chosen unit
        unit, "mm" = 2 * 25.4,
        "cm" = 2 * 2.54,
        "in" = 2,
        2
      )

      depth_thresh_disp <- round(depth_thresh, 2)          # threshold in chosen unit

      dt$depth_qaqc <- ifelse(
        depth_range_raw < depth_thresh,
        paste0(
          "Warning: Infiltration rate calculated from shallow ponding depth (",
          "observed range = ",
          round(depth_range_raw, 2), " ", unit, ")"
        ),
        ""
      )

      # 4 ── Infiltration-rate QA/QC ────────────────────────────────────
      ir_thresh_in <- 150                               # inches hr⁻¹ baseline
      ir_thresh <- switch(
        unit, "mm" = ir_thresh_in * 25.4,
        "cm" = ir_thresh_in * 2.54,
        "in" = ir_thresh_in,
        ir_thresh_in
      )
      ir_lbl <- paste0(round(ir_thresh, 1), " ", unit, "/hr")

      ir_in_in <- suppressWarnings(
        switch(
          unit,
          "mm" = as.numeric(dt$Infiltration_rate) / 25.4,
          "cm" = as.numeric(dt$Infiltration_rate) / 2.54,
          "in" = as.numeric(dt$Infiltration_rate),
          as.numeric(dt$Infiltration_rate)
        )
      )

      dt$infiltration_rate_qaqc <- ifelse(
        is.na(ir_in_in),
        "Insufficient data",
        ifelse(
          ir_in_in < ir_thresh_in,
          #paste0("Within acceptable limit (< ", ir_lbl, ")"),
          "OK",
          paste0("Exceeds ", ir_lbl)
        )
      )

      # 5 ── Rename / arrange columns (Depth-Range column omitted) ──────
      dt <- dt %>% dplyr::select(-Duration_hrs)   # ← hide Duration for now

      col_map <- c(
        "Piezometer"             = "Piezometer",
        "Infiltration_rate"      = paste("Infiltration Rate (", unit, "/hr)", sep = ""),
        # "Duration_hrs"         = "Duration (hr)",   # ← commented-out
        "infiltration_rate_qaqc" = "Infiltration Rate QAQC",
        "depth_qaqc"             = "Depth QAQC"
      )
      names(dt) <- vapply(
        names(dt),
        function(x) if (x %in% names(col_map)) col_map[[x]] else x,
        character(1)
      )


      # 6 ── Render with colour cues ────────────────────────────────────
      DT::datatable(
        dt,
        rownames = FALSE,
        options  = list(dom = "t", paging = FALSE, ordering = FALSE)
      ) %>%
        DT::formatStyle(   # colour infiltration-rate QAQC
          "Infiltration Rate QAQC",
          target = "cell",
          backgroundColor = DT::styleEqual(
            c(
             "OK",
              paste0("Exceeds ",               ir_lbl),
              "Insufficient data"
            ),
            c("lightgreen", "yellow", "lightgrey")
          )
        ) %>%
        DT::formatStyle(   # colour depth QAQC when message present
          "Depth QAQC",
          target = "cell",
          backgroundColor = DT::styleEqual(
            unique(dt$`Depth QAQC`[dt$`Depth QAQC` != ""]),
            rep("orange", length(unique(dt$`Depth QAQC`[dt$`Depth QAQC` != ""])))
          )
        )
    })
    # --------------------------------------------------------------------


    # Individual-storm table  ── includes sheetname
    # ── Download: individual storm (QA code populated) ─────────────────────────
    output$download_table_infiltration <- downloadHandler(
      filename = function() paste0("infiltration_table_", Sys.Date(), ".csv"),
      content  = function(file) {

        res  <- selected_result()                      # plot/table/best_fit_df for this sheet
        dt   <- res$table
        bf   <- res$best_fit_df
        unit <- input$depth_unit_infiltration

        ## ── depth-range check -------------------------------------------------
        depth_range_raw <- sapply(dt$Piezometer, function(pz) {
          vals <- bf$best_fit[bf$piezometer == pz]
          if (length(vals) == 0) NA_real_ else diff(range(vals, na.rm = TRUE))
        })
        depth_thresh <- switch(unit, "mm" = 2*25.4, "cm" = 2*2.54, "in" = 2, 2)
        depth_fail   <- depth_range_raw < depth_thresh | is.na(depth_range_raw)

        ## ── infiltration-rate check ------------------------------------------
        ir_thresh_in <- 150
        ir_in_in <- suppressWarnings(as.numeric(dt$Infiltration_rate) /
                                       switch(unit, "mm" = 25.4, "cm" = 2.54, "in" = 1, 1))
        rate_fail  <- is.na(ir_in_in) | ir_in_in >= ir_thresh_in

        ## ── derive QA code ----------------------------------------------------
        dt$infiltrationqacode <- ifelse(
          !depth_fail & !rate_fail, "OK",
          ifelse(!depth_fail &  rate_fail, "H",
                 ifelse( depth_fail & !rate_fail, "DL", "HDL"))
        )

        ## ── final export frame -----------------------------------------------
        export <- dt %>%
          transmute(
            sheetname              = input$choose_rain_event_infiltration,
            piezometer             = Piezometer,
            infiltrationrate       = Infiltration_rate,
            infiltrationrateunits  = paste0(unit, "/hr"),
            infiltrationqacode
          )

        readr::write_csv(export, file)
      }
    )




    # Combined-results table  ── includes sheetname
    output$download_all_results_table <- downloadHandler(
      filename = function() paste0("infiltration_table_", Sys.Date(), "_ALL.csv"),
      content  = function(file) {

        unit     <- input$depth_unit_infiltration
        ir_thresh_in <- 150
        depth_thresh <- switch(unit, "mm" = 2*25.4, "cm" = 2*2.54, "in" = 2, 2)

        rows <- lapply(names(analysisResults()), function(sheet) {
          res <- analysisResults()[[sheet]]
          if (!is.null(res$error)) return(NULL)

          dt <- res$table
          bf <- res$best_fit_df

          ## depth check
          depth_range_raw <- sapply(dt$Piezometer, function(pz) {
            vals <- bf$best_fit[bf$piezometer == pz]
            if (length(vals) == 0) NA_real_ else diff(range(vals, na.rm = TRUE))
          })
          depth_fail <- depth_range_raw < depth_thresh | is.na(depth_range_raw)

          ## infiltration-rate check
          ir_in_in <- suppressWarnings(as.numeric(dt$Infiltration_rate) /
                                         switch(unit, "mm" = 25.4, "cm" = 2.54, "in" = 1, 1))
          rate_fail <- is.na(ir_in_in) | ir_in_in >= ir_thresh_in

          ## QA code
          qacode <- ifelse(
            !depth_fail & !rate_fail, "OK",
            ifelse(!depth_fail &  rate_fail, "H",
                   ifelse( depth_fail & !rate_fail, "DL", "HDL"))
          )

          dt %>%
            transmute(
              sheetname              = sheet,
              piezometer             = Piezometer,
              infiltrationrate       = Infiltration_rate,
              infiltrationrateunits  = paste0(unit, "/hr"),
              infiltrationqacode     = qacode
            )
        })

        export <- dplyr::bind_rows(rows)
        readr::write_csv(export, file)
      }
    )





  })
}
