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
}
