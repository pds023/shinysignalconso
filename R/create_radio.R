#' Title
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples
create_radio <- function(id,type,disabled_state = FALSE) {
  if(type %in% "unite"){
    return(radioGroupButtons(
      inputId = id,
      label = NULL,
      choices = c(`<i class="fa-solid fa-cube"></i>` = "nb",
                  `<i class="fa-solid fa-euro-sign"></i>` = "val"),
      selected = "val",
      justified = FALSE,
      disabled = disabled_state
    ))
  } else if(type %in% "pct"){
    return(radioGroupButtons(
      inputId = id,
      label = NULL,
      choices = c(`<i class="fa-solid fa-hashtag"></i>` = "niv",
                  `<i class="fa-solid fa-percent"></i>` = "percent"),
      justified = FALSE,
      disabled = disabled_state
    ))
  } else if(type %in% "graph"){
    return(radioGroupButtons(
      inputId = id,
      label = NULL,
      choices = c(`<i class='fa fa-bar-chart'></i>` = "bar",
                  `<i class='fa fa-pie-chart'></i>` = "pie"),
      justified = FALSE,
      disabled = disabled_state
    ))
  }
}
