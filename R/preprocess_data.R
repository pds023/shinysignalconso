
#' Title
#'
#' @param file
#'
#' @return
#' @export
#' @import data.table arrow
#' @examples
preprocess_data <- function(file = "signalconso.csv") {
  gc()
  data <- as.data.table(read.csv(file, sep = ";"))
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

  gc()

  maphc <- readxl::read_xlsx("data/table_maphc.xlsx")
  data <- merge(data,maphc,by = c("reg_code","dep_code"),all = TRUE)

  s3write_using(
    x = data,
    FUN = write_parquet,
    object = "signalconso.parquet",
    bucket = "awsbucketpf/shinysignalconso"
  )

  file.remove(file)
}
