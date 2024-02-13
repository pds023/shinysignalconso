
#' Title
#'
#' @return
#' @export
#'
#' @examples
nav_menu_apropos <- function() {
  return(
    nav_menu("À propos",icon = bs_icon("info-circle-fill"),
             nav_panel("Crédits",icon = bs_icon("c-circle"),
                       card(card_header("Packages utilisés"),
                            card_body(htmlOutput("apropos")))),
             nav_panel("Méthodologie",icon = bs_icon("question-octagon"),
                       card(card_header("Méthodologie"),
                            card_body(htmlOutput("signalconsometh")))),
             nav_item(
               tags$a(href="mailto:philippe.fontaine.ds@proton.me",
                      class="nav-link",
                      tags$i(class="fa fa-envelope"), " Contact")
             )
    ))
}
