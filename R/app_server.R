#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {


  # observeEvent(input$main_navbar, {
  #   if (input$main_navbar %in% c("Rainfall Analysis","Flow Analysis")) {
  #     showModal(
  #       modalDialog(
  #         title = "Under Development",
  #         # You can use tags$a() to create a clickable link
  #         tagList(
  #           "This feature is under development. Please refer to the old version of BMP Hydrology app at ",
  #           tags$a(
  #             href = "https://sccwrp.shinyapps.io/rainfall_flow_analysis/",
  #             target = "_blank",
  #             "this link"
  #           ),
  #           "."
  #         ),
  #         # easyClose = TRUE still allows clicking outside to close,
  #         # but we also add a close button explicitly
  #         easyClose = TRUE,
  #         footer = modalButton("Close")  # <--- Close button
  #       )
  #     )
  #   }
  # })

  cowplot::set_null_device("agg")
  ggplot2::theme_set(ggplot2::theme_bw(base_size = 18))
  thematic::thematic_shiny(font = "auto")
  mod_instructions_server("instructions_1")
  mod_rainfall_analysis_server("rainfall_analysis_1")
  mod_flow_analysis_server("flow_analysis_1")
  mod_infiltration_analysis_server("infiltration_analysis_1")
  mod_faq_server("faq_1")
}
