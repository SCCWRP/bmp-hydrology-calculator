#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  ui <- tagList(
    golem_add_external_resources(),
    shinyjs::useShinyjs(),
    div(HTML(
      "<script type='text/x-mathjax-config' >
        MathJax.Hub.Config({
          tex2jax: {inlineMath: [['$','$']]}
        });
       </script >"
    )),
    useBusyIndicators(),
    bslib::page_navbar(
      id = "main_navbar",
      title = "BMP Hydrology Calculator 2.0",
      theme = bslib::bs_theme(preset='cosmo',font_scale = 1.3),
      navbar_options = bslib::navbar_options(
        bg = bslib::bs_get_variables(bslib::bs_theme(preset = "cosmo"), "primary")
      ),
      bslib::nav_panel(
        title = "About",
        mod_about_ui("about_1")
      ),
      bslib::nav_panel(
        title = "Rainfall Analysis",
        mod_rainfall_analysis_ui("rainfall_analysis_1")
      ),
      bslib::nav_panel(
        title = "Flow Analysis",
        mod_flow_analysis_ui("flow_analysis_1")
      ),
      bslib::nav_panel(
        title = "Infiltration Analysis (under QAQC)",
        mod_infiltration_analysis_ui("infiltration_analysis_1")
      ),
      bslib::nav_panel(
        title = "FAQ",
        mod_faq_ui("faq_1")
      )
    )
  )

  ui <- htmltools::tagQuery(ui)$
    find("aside")$
    addClass("html-fill-container")$
    find(".sidebar-content")$
    addClass("html-fill-container html-fill-item")$
    allTags()

  ui
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "BMPHydrologyCalculator"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
