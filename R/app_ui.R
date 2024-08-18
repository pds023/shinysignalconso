#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`. DO NOT REMOVE.
#' @importFrom shiny fluidPage includeCSS tags useShinyjs icon actionButton
#' @importFrom bslib bs_theme
#' @importFrom waiter useWaiter autoWaiter waiterPreloader spin_dots
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @importFrom shinyWidgets input_dark_mode
#' @noRd
app_ui <- function(request) {
  tagList(
    # Add external resources
    golem_add_external_resources(),

    # Application UI layout
    fluidPage(
      theme = bs_theme(),  # Applying a custom theme with Bootstrap
      useWaiter(),         # Initialize waiter package for loading screens
      autoWaiter(html = spin_dots(), color = "#FFF"),  # Loading animation
      waiterPreloader(
        color = "#000",
        html = tagList(
          spin_dots(),
          tags$style("
            .waiter-overlay-content { display: flex; flex-direction: column; align-items: center; }
            .my-custom-space { margin-top: 40px; }
          "),
          div(class = "my-custom-space", h4("Chargement, veuillez patienter..."))
        )
      ),

      # Include custom CSS
      includeCSS(app_sys("app/www/styles.css")),

      # Include custom font and icon in header
      tags$head(
        tags$style(HTML("
          @font-face {
            font-family: 'Marianne';
            src: url('www/Marianne-Regular.woff') format('woff');
          }
          body {
            font-family: 'Marianne', sans-serif;
          }
        ")),
        tags$link(rel = "icon", href = "www/logoapp.png", type = "image/png")
      ),

      # Main navigation bar with different panels
      page_navbar(
        title = "ShinySignalConso",
        nav_panel_exploration(),  # Exploration panel (define in another module)
        nav_menu_apropos(),       # About menu (define in another module)

        nav_spacer(),
        nav_item(
          actionButton(inputId = "suggestions", label = "Une suggestion ?")  # Suggestions button
        ),

        # External links
        nav_item(
          tags$a(icon("github"), "ShinySignalConso",
                 href = "https://github.com/pds023/shinysignalconso", target = "_blank")
        ),
        nav_item(
          tags$a(icon("linkedin"), "philippe-fontaine-ds",
                 href = "https://www.linkedin.com/in/philippe-fontaine-ds/", target = "_blank")
        ),

        # Dark mode toggle button
        nav_item(input_dark_mode(mode = "light")),

        # Custom footer
        tags$style("
          .footer {
            position: fixed;
            bottom: 0;
            width: 100%;
            background-color: rgba(8, 60, 116, 1);
            color: white;
            text-align: center;
            padding: 5px;
            margin-left: -25px;
            z-index: 100;
          }
          .footer a { color: white; }
        "),
        footer = tags$div(
          class = "footer",
          "Développé par ",
          tags$a(href = "https://www.philippefontaine.eu", target = "_blank", "Philippe Fontaine")
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path("www", app_sys("app/www"))  # Add www directory as resource path

  # Include resources like favicon and bundled CSS/JS
  tags$head(
    favicon(ext = "png"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Signal Conso"
    )
  )
}
