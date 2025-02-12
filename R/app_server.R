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
  mod_instructions_server("instructions_1")
  mod_rainfall_analysis_server("rainfall_analysis_1")
  mod_flow_analysis_server("flow_analysis_1")
  mod_infiltration_analysis_server("infiltration_analysis_1")
  mod_faq_server("faq_1")
}
