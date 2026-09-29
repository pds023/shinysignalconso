# Data sources are explicit: a local file, an S3 object, or a labelled demo.

signalconso_territories <- function() {
  path <- system.file("extdata", "territories.csv", package = "shinySignalConso")
  if (!nzchar(path)) path <- file.path("inst", "extdata", "territories.csv")
  if (!file.exists(path)) return(NULL)
  data.table::fread(path, colClasses = "character", encoding = "UTF-8")
}

signalconso_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  values <- trimws(as.character(x))
  result <- rep(as.Date(NA), length(values))
  iso <- !is.na(values) & grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}($|[ T])", values)
  french <- !is.na(values) & grepl("^[0-9]{2}/[0-9]{2}/[0-9]{4}$", values)
  result[iso] <- as.Date(substr(values[iso], 1L, 10L), format = "%Y-%m-%d")
  result[french] <- as.Date(values[french], format = "%d/%m/%Y")
  result
}

normalize_signalconso_data <- function(data) {
  if (!is.data.frame(data)) stop("La source doit contenir un tableau de données.", call. = FALSE)
  data <- data.table::copy(data.table::as.data.table(data))
  if (!"creationdate" %in% names(data)) {
    if ("date" %in% names(data)) {
      data[, creationdate := date]
    } else if (nrow(data)) {
      stop("La source ne contient pas la colonne obligatoire « creationdate » (ou « date »).", call. = FALSE)
    } else {
      data[, creationdate := as.Date(character())]
    }
  }
  dates <- signalconso_date(data$creationdate)
  if (anyNA(dates)) {
    stop(sprintf("%s date(s) manquante(s) ou invalide(s) dans « creationdate ». Formats acceptés : AAAA-MM-JJ (avec heure facultative) ou JJ/MM/AAAA.", sum(is.na(dates))), call. = FALSE)
  }
  data[, `:=`(creationdate = dates, date = dates, annee = format(dates, "%Y"))]

  text_columns <- c("category", "subcategories", "tags", "status", "dep_code", "dep_name", "reg_code", "reg_name", "hc-key")
  for (column in text_columns) {
    values <- if (column %in% names(data)) trimws(as.character(data[[column]])) else rep(NA_character_, nrow(data))
    values[is.na(values) | !nzchar(values)] <- "Non renseigné"
    data.table::set(data, j = column, value = values)
  }
  for (column in c("dep_code", "reg_code")) {
    values <- toupper(data[[column]])
    single_digit <- grepl("^[0-9]$", values)
    values[single_digit] <- paste0("0", values[single_digit])
    values[values == "NON RENSEIGNÉ"] <- "Non renseigné"
    data.table::set(data, j = column, value = values)
  }

  categories <- c(
    CafeRestaurant = "Café / Restaurant", AchatMagasinInternet = "E-commerce",
    AchatInternet = "E-commerce", BanqueAssuranceMutuelle = "Banque / Assurance",
    VoyageLoisirs = "Voyage / Loisirs", VoitureVehiculeVelo = "Véhicules",
    VoitureVehicule = "Véhicules", AchatMagasin = "Achat en magasin",
    TravauxRenovations = "Travaux / Rénovations", ServicesAuxParticuliers = "Services aux particuliers",
    TelephonieFaiMedias = "Téléphonie / Internet", TelEauGazElec = "Eau / Gaz / Électricité",
    "Téléphonie / Internet / médias" = "Téléphonie / Internet",
    "Téléphonie/FAI/Médias" = "Téléphonie / Internet", "Café/Restaurant" = "Café / Restaurant",
    "Banques et assimilés" = "Banque / Assurance", "Voyage/Loisirs" = "Voyage / Loisirs",
    "Travaux/Rénovations" = "Travaux / Rénovations", "Tél/Eau/Electricité" = "Eau / Gaz / Électricité"
  )
  statuses <- c(ConsulteIgnore = "Consulté / Ignoré", "Consulté/Ignoré" = "Consulté / Ignoré",
                MalAttribue = "Mal attribué", NonConsulte = "Non consulté",
                PromesseAction = "Promesse d'action", TraitementEnCours = "Traitement en cours")
  for (column in c("category", "status")) {
    dictionary <- if (column == "category") categories else statuses
    replacement <- unname(dictionary[data[[column]]])
    found <- !is.na(replacement)
    data.table::set(data, i = which(found), j = column, value = replacement[found])
  }

  progress <- if ("signalement_traitement" %in% names(data)) trimws(as.character(data$signalement_traitement)) else rep(NA_character_, nrow(data))
  progress[progress == "Signalement tranmis" & !is.na(progress)] <- "Signalement transmis"
  flags <- c(signalement_transmis = "Signalement transmis", signalement_lu = "Signalement lu", signalement_reponse = "Signalement répondu")
  available <- intersect(names(flags), names(data))
  if (length(available)) {
    known <- Reduce(`|`, lapply(available, function(column)
      tolower(trimws(as.character(data[[column]]))) %in% c("0", "1", "false", "true", "f", "t", "non", "oui", "no", "yes")))
    progress[known & (is.na(progress) | !nzchar(progress))] <- "Non traité"
    for (column in available) {
      positive <- tolower(trimws(as.character(data[[column]]))) %in% c("1", "true", "t", "oui", "yes")
      progress[positive] <- flags[[column]]
    }
  }
  progress[is.na(progress) | !nzchar(progress)] <- "Non renseigné"
  data[, signalement_traitement := progress]

  territories <- signalconso_territories()
  if (!is.null(territories)) {
    # A lookup preserves the number and order of reports, including unknown codes.
    index <- match(data$dep_code, territories$dep_code)
    for (column in c("reg_code", "hc-key")) {
      missing <- data[[column]] == "Non renseigné" & !is.na(index)
      data.table::set(data, i = which(missing), j = column, value = territories[[column]][index[missing]])
    }
  }
  columns <- c("creationdate", "date", "annee", "category", "subcategories", "tags", "status",
               "signalement_traitement", "dep_code", "dep_name", "reg_code", "reg_name", "hc-key")
  data[, columns, with = FALSE]
}

signalconso_demo_data <- function(n = 18000L) {
  # Restore the caller's RNG state, so opening a session has no random side effects.
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) previous_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit(if (had_seed) assign(".Random.seed", previous_seed, envir = .GlobalEnv) else
    if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv))
  set.seed(20250901)
  territories <- data.table::data.table(
    dep_code = c("75", "59", "69", "13", "33", "31", "44", "35", "67", "34", "06", "76", "37", "21", "87", "25", "29", "63", "80", "51", "45", "54", "83", "38", "2A", "974"),
    dep_name = c("Paris", "Nord", "Rhône", "Bouches-du-Rhône", "Gironde", "Haute-Garonne", "Loire-Atlantique", "Ille-et-Vilaine", "Bas-Rhin", "Hérault", "Alpes-Maritimes", "Seine-Maritime", "Indre-et-Loire", "Côte-d'Or", "Haute-Vienne", "Doubs", "Finistère", "Puy-de-Dôme", "Somme", "Marne", "Loiret", "Meurthe-et-Moselle", "Var", "Isère", "Corse-du-Sud", "La Réunion"),
    reg_code = c("11", "32", "84", "93", "75", "76", "52", "53", "44", "76", "93", "28", "24", "27", "75", "27", "53", "84", "32", "44", "24", "44", "93", "84", "94", "04"),
    reg_name = c("Île-de-France", "Hauts-de-France", "Auvergne-Rhône-Alpes", "Provence-Alpes-Côte d'Azur", "Nouvelle-Aquitaine", "Occitanie", "Pays de la Loire", "Bretagne", "Grand Est", "Occitanie", "Provence-Alpes-Côte d'Azur", "Normandie", "Centre-Val de Loire", "Bourgogne-Franche-Comté", "Nouvelle-Aquitaine", "Bourgogne-Franche-Comté", "Bretagne", "Auvergne-Rhône-Alpes", "Hauts-de-France", "Grand Est", "Centre-Val de Loire", "Grand Est", "Provence-Alpes-Côte d'Azur", "Auvergne-Rhône-Alpes", "Corse", "La Réunion")
  )
  categories <- c("E-commerce", "Achat en magasin", "Services aux particuliers", "Téléphonie / Internet", "Voyage / Loisirs", "Café / Restaurant", "Banque / Assurance", "Travaux / Rénovations", "Véhicules", "Eau / Gaz / Électricité")
  subcategories <- list(
    c("Livraison", "Remboursement", "Produit non conforme", "Service après-vente"),
    c("Prix affiché", "Garantie", "Qualité du produit", "Pratiques commerciales"),
    c("Résiliation", "Facturation", "Qualité de la prestation"),
    c("Résiliation", "Facturation", "Qualité du service"),
    c("Annulation", "Remboursement", "Hébergement"),
    c("Hygiène", "Prix affiché", "Information du consommateur"),
    c("Frais bancaires", "Résiliation", "Information du consommateur"),
    c("Devis", "Qualité des travaux", "Retard d'exécution"),
    c("Réparation", "Garantie", "Vente de véhicule"),
    c("Facturation", "Démarchage", "Changement de fournisseur")
  )
  days <- seq(as.Date("2023-01-01"), as.Date("2025-12-31"), by = "day")
  weights <- (1 + as.numeric(days - min(days)) / 1500) * ifelse(format(days, "%m") %in% c("11", "12"), 1.25, 1)
  category_index <- sample(seq_along(categories), n, replace = TRUE, prob = c(26, 18, 11, 10, 9, 8, 6, 5, 4, 3))
  data <- territories[sample(seq_len(nrow(territories)), n, replace = TRUE, prob = c(14, 8, 8, 7, 6, 5, 5, 4, 4, 4, 3, 3, 2, 2, 1, 1, 2, 2, 2, 1, 1, 1, 2, 2, 1, 1))]
  data[, `:=`(creationdate = sample(days, n, replace = TRUE, prob = weights), category = categories[category_index])]
  data[, subcategories := vapply(category_index, function(i) sample(subcategories[[i]], 1L), character(1))]
  data[, tags := sample(c("Achat sur internet", "Achat en magasin", "Démarchage", "Abonnement", "Service après-vente"), n, replace = TRUE, prob = c(35, 25, 10, 15, 15))]
  data[category == "E-commerce", tags := "Achat sur internet"]
  data[category == "Achat en magasin", tags := "Achat en magasin"]
  data[, signalement_traitement := sample(c("Non traité", "Signalement transmis", "Signalement lu", "Signalement répondu"), n, replace = TRUE, prob = c(8, 12, 22, 58))]
  data[, status := ifelse(signalement_traitement == "Signalement répondu", "Promesse d'action",
                         ifelse(signalement_traitement == "Signalement lu", "Consulté / Ignoré", "Non consulté"))]
  normalize_signalconso_data(data[order(creationdate)])
}

read_signalconso_file <- function(path) {
  if (!file.exists(path)) stop(sprintf("Fichier de données introuvable : %s", path), call. = FALSE)
  extension <- tolower(tools::file_ext(path))
  switch(extension,
    csv = data.table::fread(path, encoding = "UTF-8", colClasses = "character"),
    rds = readRDS(path),
    parquet = {
      if (!requireNamespace("arrow", quietly = TRUE)) stop("Le format Parquet nécessite le package R « arrow ».", call. = FALSE)
      as.data.frame(arrow::read_parquet(path))
    },
    stop("Format non pris en charge. Utilisez un fichier .csv, .rds ou .parquet.", call. = FALSE)
  )
}

load_signalconso_data <- function() {
  path <- trimws(Sys.getenv("SIGNALCONSO_DATA", unset = ""))
  bucket <- trimws(Sys.getenv("SIGNALCONSO_S3_BUCKET", unset = ""))
  if (nzchar(path)) {
    data <- tryCatch(normalize_signalconso_data(read_signalconso_file(path)), error = function(e)
      stop(paste("Impossible de charger la source locale :", conditionMessage(e)), call. = FALSE))
    return(list(data = data, mode = "local", label = "Données locales", detail = paste("Source :", basename(path))))
  }
  if (nzchar(bucket)) {
    object <- Sys.getenv("SIGNALCONSO_S3_OBJECT", unset = "signalconso.parquet")
    if (!nzchar(object)) stop("SIGNALCONSO_S3_OBJECT ne peut pas être vide.", call. = FALSE)
    if (!requireNamespace("aws.s3", quietly = TRUE)) stop("La source S3 nécessite le package R « aws.s3 ».", call. = FALSE)
    temporary <- tempfile(fileext = paste0(".", tools::file_ext(object)))
    on.exit(unlink(temporary), add = TRUE)
    data <- tryCatch({
      aws.s3::save_object(object = object, bucket = bucket, file = temporary)
      normalize_signalconso_data(read_signalconso_file(temporary))
    }, error = function(e) stop("Impossible de charger la source S3. Vérifiez le bucket, le nom de l'objet, les autorisations et le format des données (.csv, .rds ou .parquet).", call. = FALSE))
    return(list(data = data, mode = "s3", label = "Données S3", detail = paste("Objet :", basename(object))))
  }
  list(data = signalconso_demo_data(), mode = "demo", label = "Données de démonstration",
       detail = "18 000 signalements fictifs, de janvier 2023 à décembre 2025. Ces données synthétiques illustrent l'application et ne décrivent pas l'activité réelle de SignalConso.")
}
