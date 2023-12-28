
#' Title
#'
#' @param path
#'
#' @return
#' @export
#' @import data.table arrow
#' @examples
preprocess_data <- function(path = "../../data/signalconso.csv") {
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

  data[, annee := substr(creationdate,1,4)]

  write_parquet(data,"data/signalconso.parquet")
}
