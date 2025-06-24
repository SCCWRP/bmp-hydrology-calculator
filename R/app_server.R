#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  cowplot::set_null_device("agg")
  ggplot2::theme_set(ggplot2::theme_bw(base_size = 18))
  thematic::thematic_shiny(font = "auto")

  #Show maintenance modal on startup
  # showModal(
  #   modalDialog(
  #     title = "Maintenance Notice",
  #     "We're currently performing scheduled maintenance on our application to bring you an even better experience. We will be back online as soon as possible. Thank you!",
  #     easyClose = TRUE,
  #     footer = modalButton("Close")
  #   )
  # )

  mod_about_server("about_1")

  mod_rainfall_analysis_server("rainfall_analysis_1")
  mod_rainfall_instruction_server("rainfall_instruction")

  mod_flow_analysis_server("flow_analysis_1")
  mod_flow_instruction_server("flow_instruction")

  mod_infiltration_analysis_server("infiltration_analysis_1")
  mod_infiltration_instruction_server("infiltration_instruction")

  mod_faq_server("faq_1")
}

