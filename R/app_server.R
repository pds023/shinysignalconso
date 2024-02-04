#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  options(warn = -1)

  source("set_cfg.R")

  data <- reactiveVal()
  data_filtered <- reactiveVal()
  data_categories <- reactiveVal()
  data_subcategories <- reactiveVal()
  data_tags <- reactiveVal()
  data_territory <- reactiveVal()
  data_sigstate <- reactiveVal()
  data_years <- reactiveVal()

  filters_applied <- reactiveVal(FALSE)

  data(as.data.table(s3read_using(
    read_parquet,
    object = "signalconso.parquet",
    bucket = "awsbucketpf/shinysignalconso"
  )))

  suggestions <- s3read_using(
    read_parquet,
    object = "suggestions.parquet",
    bucket = "awsbucketpf/shinysignalconso"
  )

  output$signalconsometh <- renderText({
    includeMarkdown("inst/app/www/signalconsometh.md")
  })
  output$apropos <- renderText({
    includeMarkdown("inst/app/www/apropos.md")
  })


  observeEvent(input$suggestions,{
    showModal(modalDialog(
      title = "Soumettre une suggestion",
      textAreaInput(inputId = "suggestion_text",label = "Suggestion :",placeholder = "Merci de décrire votre suggestion"),
      h5("Merci de ne pas renseigner d'informations personnelles dans ce champ. Les informations soumises sont enregistrées."),
      footer = tagList(
        modalButton("Annuler"),
        actionButton("ok", "Envoyer")),
      easyClose = TRUE)
    )
  })
  observeEvent(input$ok,{
    removeModal()
    show_alert(title = "Merci pour votre suggestion !",type = "success")
    print(input$suggestion_text)
    suggestions <- as.data.table(rbind(suggestions,cbind(as.character(Sys.Date()),input$suggestion_text)))
    s3write_using(
      x = suggestions,
      FUN = write_parquet,
      object = "suggestions.parquet",
      bucket = "awsbucketpf/shinysignalconso"
    )
  })



  observe({
    if(is.null(data)){return(NULL)
    } else{
      if(!filters_applied()){
        data_categories(data()[,.N,category][order(N,decreasing = TRUE)])
        data_subcategories(data()[,.N,subcategories][order(N,decreasing = TRUE)])
        data_tags(data()[,.N,tags][order(N,decreasing = TRUE)])
        data_sigstate(data()[,.N,signalement_traitement][order(N,decreasing = TRUE)])
        data_territory(data()[,.N,.(reg_code,reg_name,dep_code,dep_name,`hc-key`)][order(N,decreasing = TRUE)])
        data_years(data()[,.N,annee][order(N,decreasing = TRUE)])
      } else{
        data_categories(data_filtered()[,.N,category][order(N,decreasing = TRUE)])
        data_subcategories(data_filtered()[,.N,subcategories][order(N,decreasing = TRUE)])
        data_tags(data_filtered()[,.N,tags][order(N,decreasing = TRUE)])
        data_sigstate(data_filtered()[,.N,signalement_traitement][order(N,decreasing = TRUE)])
        data_territory(data_filtered()[,.N,.(reg_code,reg_name,dep_code,dep_name,`hc-key`)][order(N,decreasing = TRUE)])
        data_years(data_filtered()[,.N,annee][order(N,decreasing = TRUE)])
      }
    }
  })

  # Dynamic sidebar filters ----------------
  output$sidebar_exploration <- sidebar_exploration()


  ## Maj PickerInput ----------------
  observe({
    if(is.null(data_years())){return(NULL)
    } else{
      if(!filters_applied()){
        updatePickerInput(session = session,inputId = "exploration_filter_category",choices = data_categories()[,category])
        updatePickerInput(session = session,inputId = "exploration_filter_subcategories",choices = data_subcategories()[,subcategories])
        updatePickerInput(session = session,inputId = "exploration_filter_tags",choices = data_tags()[,tags])
        updatePickerInput(session = session,inputId = "exploration_filter_region",choices = unique(data_territory()[,reg_name]))
        updatePickerInput(session = session,inputId = "exploration_filter_departement",choices = unique(data_territory()[,dep_name]))
        updatePickerInput(session = session,inputId = "exploration_filter_sigstate",choices = unique(data_sigstate()[,signalement_traitement]))
        updatePickerInput(session = session,inputId = "exploration_filter_annee",choices = unique(data_years()[,annee]))

        updatePickerInput(session = session,inputId = "variables_compare",choices = colnames(data()))
      }
    }
  })


  ## Apply filters ----------------

  observeEvent(input$exploration_filters_apply,{
    # Récupérer les données
    tmp <- data()

    # Réinitialiser l'état du filtre
    filters_applied(FALSE)

    # Appliquer les filtres si les inputs ne sont pas NULL ou vides
    if (!is.null(input$exploration_filter_category) && length(input$exploration_filter_category) > 0) {
      tmp <- tmp[category %in% input$exploration_filter_category]
      filters_applied(TRUE)
    }
    if (!is.null(input$exploration_filter_subcategories) && length(input$exploration_filter_subcategories) > 0) {
      tmp <- tmp[subcategories %in% input$exploration_filter_subcategories]
      filters_applied(TRUE)
    }
    if (!is.null(input$exploration_filter_tags) && length(input$exploration_filter_tags) > 0) {
      tmp <- tmp[tags %in% input$exploration_filter_tags]
      filters_applied(TRUE)
    }
    if (!is.null(input$exploration_filter_region) && length(input$exploration_filter_region) > 0) {
      tmp <- tmp[reg_name %in% input$exploration_filter_region]
      filters_applied(TRUE)
    }
    if (!is.null(input$exploration_filter_departement) && length(input$exploration_filter_departement) > 0) {
      tmp <- tmp[reg_name %in% input$exploration_filter_departement]
      filters_applied(TRUE)
    }
    if (!is.null(input$exploration_filter_sigstate) && length(input$exploration_filter_sigstate) > 0) {
      tmp <- tmp[signalement_traitement %in% input$exploration_filter_sigstate]
      filters_applied(TRUE)
    }
    if (!is.null(input$exploration_filter_annee) && length(input$exploration_filter_annee) > 0) {
      tmp <- tmp[annee %in% input$exploration_filter_annee]
      filters_applied(TRUE)
    }
    if (filters_applied()) {
      data_filtered(tmp)
    }
  })

  ## Reset filters ----------------
  observeEvent(input$exploration_filters_reset,{
    filters_applied(FALSE)
  })

  # Highcharts graphs tab1 ----------------
  observe({
    output$highchart_stats_categories <- renderHighchart(graph_explore(data = data_categories()[N > 5],
                                                                       input_type = input$highchart_stats_type,
                                                                       input_pct = input$highchart_stats_pct))
  })
  observe({
    output$highchart_stats_tags <- renderHighchart(graph_explore(data = data_tags()[N > 5],
                                                                 input_type = input$highchart_stats_type,
                                                                 input_pct = input$highchart_stats_pct))
  })
  observe({
    output$highchart_stats_territoire <- renderHighchart(graph_explore(data = data_territory()[N > 5,.(reg_name,dep_name,N)],
                                                                       input_type = input$highchart_stats_type,
                                                                       input_pct = input$highchart_stats_pct,
                                                                       group = TRUE))
  })
  observe({
    output$highchart_stats_sigstate <- renderHighchart(graph_explore(data = data_sigstate()[N > 5],
                                                                     input_type = input$highchart_stats_type,
                                                                     input_pct = input$highchart_stats_pct))
  })


  # TODO : split observe to optimize select_seasonal
  observe({
    if(is.null(data())){return(NULL)
    } else{
      if(!filters_applied()){
        data_temp <- data()}
      else{data_temp <- data_filtered()}
      data_temp <- data_temp[,date := as.Date(creationdate)]

      data_temp[,valeur := .N,date]

      data_temp <- data_temp[,.(date,valeur)]
      data_temp <- data_temp[!duplicated(date)]

      data_temp_xts <- xts(data_temp[,-1,with = FALSE],order.by = data_temp$date)
      data_temp_xts$ma30 <- rollapply(data_temp_xts, width = 30, FUN = mean, align = "right", fill = NA)
      data_temp_xts$ma90 <- rollapply(data_temp_xts$valeur, width = 90, FUN = mean, align = "right", fill = NA)


      output$exploration_timegraph <- renderHighchart(
        highchart(type = "stock") |>
          hc_add_series(data_temp_xts$valeur, yAxis = 0, name = "Signalements") |>
          hc_add_series(data_temp_xts$ma30,type = "line", yAxis = 0, name = "MM30") |>
          hc_add_series(data_temp_xts$ma90,type = "line", yAxis = 0, name = "MM90"))
    }

    req(input$select_seasonal)
    if(is.null(input$select_seasonal)){return(NULL)
    } else{
      if(input$select_seasonal %in% "Hebdomadaire"){type_graph <- "wday.lbl"}
      if(input$select_seasonal %in% "Mensuel"){type_graph <- "month.lbl"}
      if(input$select_seasonal %in% "Trimestriel"){type_graph <- "quarter"}
      if(input$select_seasonal %in% "Annuel"){type_graph <- "year"}

      output$exploration_timegraph_seasonal <- renderPlotly(
        data_temp %>%
          plot_seasonal_diagnostics(date, valeur, .interactive = TRUE,.feature_set = type_graph)
      )
    }
  })


  observe({
    output$exploration_map_dep <-  renderHighchart(
      hcmap(
        "countries/fr/fr-all-all",
        data = data_territory(),
        value = "N",
        joinBy = "hc-key",
        name = "Signalements",
        dataLabels = list(enabled = TRUE, format = "{point.name}"),
        borderColor = "#FAFAFA",
        borderWidth = 0.1,
        tooltip = list(
          valueDecimals = 0
        )  )|>
        hc_colorAxis(
          ticklength = 8,
          type = "logarithmic"
        )
    )
  })
  observe({
    data_map_reg <- data_territory()[,sum(N),.(reg_code,substr(`hc-key`,1,6))]
    colnames(data_map_reg) <- c("reg_code","hc-key","N")
    output$exploration_map_reg <-  renderHighchart(
      hcmap(
        "countries/fr/fr-all",
        data = data_map_reg,
        value = "N",
        joinBy = "hc-key",
        name = "Signalements",
        dataLabels = list(enabled = TRUE, format = "{point.name}"),
        borderColor = "#FAFAFA",
        borderWidth = 0.1,
        tooltip = list(
          valueDecimals = 0
        )  )|>
        hc_colorAxis(
          ticklength = 8,
          type = "logarithmic"
        )
    )
  })


  observeEvent(input$variables_compare,{
    req(input$variables_compare)
    if(length(input$variables_compare) > 0){
      updatePickerInput(session = session,inputId = "modalites_compare",choices = c(unique(data()[,get(input$variables_compare)])),
                        selected = NULL)
    }
  })

  observe({
    req(input$variables_compare)
    req(input$modalites_compare)

    if(!filters_applied()){
      output$highchart_compare_categories <- renderHighchart(
        graph_compare(data(),"category",input$variables_compare,input$modalites_compare,input$highchart_compare_pct)
      )
    } else{
      output$highchart_compare_categories <- renderHighchart(
        graph_compare(data_filtered(),"category",input$variables_compare,input$modalites_compare,input$highchart_compare_pct)
      )
    }
  })
  observe({
    req(input$variables_compare)
    req(input$modalites_compare)
    if(!filters_applied()){
      output$highchart_compare_tags <- renderHighchart(
        graph_compare(data(),"tags",input$variables_compare,input$modalites_compare,input$highchart_compare_pct)
      )
    } else {
      output$highchart_compare_tags <- renderHighchart(
        graph_compare(data_filtered(),"tags",input$variables_compare,input$modalites_compare,input$highchart_compare_pct)
      )
    }
  })
  observe({
    req(input$variables_compare)
    req(input$modalites_compare)
    if(!filters_applied()){
      output$highchart_compare_territoire <- renderHighchart(
        graph_compare(data(),"dep_name",input$variables_compare,input$modalites_compare,input$highchart_compare_pct)
      )
    } else {
      output$highchart_compare_territoire <- renderHighchart(
        graph_compare(data_filtered(),"dep_name",input$variables_compare,input$modalites_compare,input$highchart_compare_pct)
      )
    }
  })
  observe({
    req(input$variables_compare)
    req(input$modalites_compare)
    if(!filters_applied()){
      output$highchart_compare_sigstate <- renderHighchart(
        graph_compare(data(),"signalement_traitement",input$variables_compare,input$modalites_compare,input$highchart_compare_pct)
      )
    } else {
      output$highchart_compare_sigstate <- renderHighchart(
        graph_compare(data_filtered(),"signalement_traitement",input$variables_compare,input$modalites_compare,input$highchart_compare_pct)
      )
    }
  })


  create_dt(data(),length = 15,cols_names = NULL,select_cols = FALSE)

  observe({
    if(!filters_applied()){
      output$exploration_donnees_brutes <- create_dt(data()[,.(category,subcategories,date,status,tags,dep_name,reg_name,signalement_traitement)],
                                                     length = 15,
                                                     cols_names = c("Catégorie","Sous-catégorie","Date","Statut","Tags","Dep","Reg","Traitement"),
                                                     select_cols = FALSE)
    } else{
      output$exploration_donnees_brutes <- create_dt(data_filtered()[,.(category,subcategories,date,status,tags,dep_name,reg_name,signalement_traitement)],
                                                     length = 15,
                                                     cols_names = c("Catégorie","Sous-catégorie","Date","Statut","Tags","Dep","Reg","Traitement"),
                                                     select_cols = FALSE)
    }
  })

  observe({
    if(!filters_applied()){
      output$downloadData <- dl_button_serv(data = data(),label = "signalconso")
    } else {
      output$downloadData <- dl_button_serv(data = data_filtered(),label = "signalconso_filtered")
    }
  })

  observe({
    req(input$highchart_stats_type)
    if(input$highchart_stats_type %in% "pie"){
      updateRadioGroupButtons(session = session,inputId = "highchart_stats_pct", selected = "niv",disabled = TRUE)
    } else{
      updateRadioGroupButtons(session = session,inputId = "highchart_stats_pct",disabled = FALSE)
    }
  })

  observe({
    print(input$modalites_compare)
    if(is.null(input$modalites_compare)){
      updateRadioGroupButtons(session = session,inputId = "highchart_compare_pct",disabled = TRUE)
    } else{
      updateRadioGroupButtons(session = session,inputId = "highchart_compare_pct",disabled = FALSE)
    }

  })


}
