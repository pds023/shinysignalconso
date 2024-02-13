#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny bslib highcharter bsicons shinyWidgets arrow DT sever waiter xts zoo plotly timetk markdown
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    page_fluid(
      theme =  bs_theme(),
      useSever(),
      useWaiter(),
      autoWaiter(html =  spin_dots(),color = "#FFF"),
      waiterPreloader(color = "#000",
                      html =  tagList(
                        spin_dots(),
                        tags$style(".waiter-overlay-content { display: flex; flex-direction: column; align-items: center; } .my-custom-space { margin-top: 40px; }"), # CSS personnalisé pour l'espacement
                        div(class = "my-custom-space", h4("Chargement, veuillez patienter...")) # Texte avec espace personnalisé
                      )),
      includeCSS("inst/app/www/styles.css"),
      list(tags$head(HTML('<link rel="icon", href="www/logoapp.png",
                                   type="image/png" />')),
           tags$head(
             tags$style(HTML("
            @font-face {
                font-family: 'Marianne';
                src: url('www/Marianne-Regular.woff') format('woff');
            }
            body {
                font-family: 'Marianne', sans-serif;
            }
        "))
           )),
page_navbar(
  title="ShinySignalConso",
  nav_panel_exploration(),
  nav_menu_apropos(),
  nav_spacer(),
  nav_item(actionButton(inputId = "suggestions",label = "Une suggestion ?")),
  nav_item(tags$a(shiny::icon("github"), "ShinySignalConso", href = "https://github.com/pds023/shinysignalconso", target = "_blank")),
  nav_item(tags$a(shiny::icon("linkedin"), "philippe-fontaine-ds", href = "https://www.linkedin.com/in/philippe-fontaine-ds/", target = "_blank")),

  nav_item(input_dark_mode(mode = "light")),
  tags$style(".footer{position: fixed;bottom: 0;width: 100%;background-color: rgba(8, 60, 116, 1);color: white;text-align: center;padding: 5px;margin-left:-25px;z-index:100;}"),
  tags$style(".footer a{color: white;}"),
  footer = tags$div(
    class = "footer",
    "Développé par ",
    tags$a(href = "https://www.philippefontaine.eu", target = "_blank", "Philippe Fontaine"))
)

    )
  )

}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny aws.s3
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(ext = 'png'),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Signal Conso"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
