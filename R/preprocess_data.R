
#' Title
#'
#' @param path
#'
#' @return
#' @export
#' @import data.table arrow
#' @examples
preprocess_data <- function(path = "../../raw-data/signalconso.csv") {
  data <- as.data.table(read.csv(path, sep = ";"))
  data[,id := NULL]
  data[,.N,category]
  data[category %in% "CafeRestaurant", category := "Café/Restaurant"]
  data[category %in% "AchatMagasinInternet", category := "E-commerce"]
  data[category %in% "BanqueAssuranceMutuelle", category := "Banques et assimilés"]
  data[category %in% "VoyageLoisirs", category := "Voyage/Loisirs"]
  data[category %in% "AchatInternet", category := "E-commerce"]
  data[category %in% "VoitureVehiculeVelo", category := "Véhicules"]
  data[category %in% "AchatMagasin", category := "Achat en magasin"]
  data[category %in% "TravauxRenovations", category := "Travaux/Rénovations"]
  data[category %in% "ServicesAuxParticuliers", category := "Services aux particuliers"]
  data[category %in% "TelephonieFaiMedias", category := "Téléphonie/FAI/Médias"]
  data[category %in% "TelEauGazElec", category := "Tél/Eau/Electricité"]
  data[category %in% "VoitureVehicule", category := "Véhicules"]
  data[category %in% "Coronavirus", category := "Coronavirus"]
  data[category %in% "Téléphonie / Internet / médias", category := "Téléphonie/FAI/Médias"]


  subcategories <- data[,.N,subcategories]
  subcategories[, subcategory := strsplit(subcategories, ";")]
  subcategories_long <- subcategories[, .(subcategory = unlist(subcategory)), by = .(N)]
  subcategories_long[, subcategory := trimws(subcategory)]
  subcategories_long <- subcategories_long[subcategory != ""]
  DT_agg <- subcategories_long[, .(Count = sum(N)), by = subcategory]
  write_parquet(DT_agg[order(Count,decreasing = TRUE)],"data/list_subcategories.parquet")

  data_tags <- data[,.N,tags]
  data_tags[, tag := strsplit(tags, ";")]
  data_tags_long <- data_tags[, .(tag = unlist(tag)), by = .(N)]
  data_tags_long[, tag := trimws(tag)]
  data_tags_long <- data_tags_long[tag != ""]
  DT_agg <- data_tags_long[, .(Count = sum(N)), by = tag]
  write_parquet(DT_agg,"data/list_tags.parquet")

  data[status %in% "ConsulteIgnore",status := "Consulté/Ignoré"]
  data[status %in% "MalAttribue",status := "Mal attribué"]
  data[status %in% "NonConsulte",status := "Non consulté"]
  data[status %in% "PromesseAction",status := "Promesse d'action"]
  data[status %in% "TraitementEnCours",status := "Traitement en cours"]

  # On recode la variable signalement pour simplifier : un signalement lu est nécessairement transmis et un répondu
  # est nécessairement transmis + lu
  data[,signalement_traitement := "Non traité"]
  data[signalement_transmis == 1,signalement_traitement := "Signalement tranmis"]
  data[signalement_lu == 1,signalement_traitement := "Signalement lu"]
  data[signalement_reponse == 1,signalement_traitement := "Signalement répondu"]

  data[,signalement_transmis := NULL]
  data[,signalement_lu := NULL]
  data[,signalement_reponse := NULL]

  data[, annee := substr(creationdate,1,4)]

  # Remplacer tous les NA par "Non renseigné"
  data[] <- lapply(data, function(x) {
    x[is.na(x) | x == ""] <- "Non renseigné"
    return(x)
  })


  maphc <- readxl::read_xlsx("../../usefull-data/table_maphc.xlsx")
  data <- merge(data,maphc,by = c("reg_code","dep_code"),all = TRUE)

  write_parquet(data,"data/signalconso.parquet")
}
