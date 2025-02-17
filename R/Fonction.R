#' @title Validation du schéma du dataframe élus-conseillers-municipaux-cm
#' @name validate_schema
#' @description
#' Cette fonction vérifie que le dataframe fourni respecte le schéma attendu
#' pour le fichier "élus-conseillers-municipaux-cm".
#' Si le schéma ne correspond pas, la fonction génère une erreur.
#'
#' @param df Un dataframe contenant les données des élus conseillers municipaux.
#' @return Un message confirmant que le schéma est conforme si la validation réussit.
#'
#' @examples
#' df <- data.frame(
#'   "Code.du.département" = character(),
#'   "Libellé.du.département" = character(),
#'   "Code.de.la.collectivité.à.statut.particulier" = character(),
#'   "Libellé.de.la.collectivité.à.statut.particulier" = character(),
#'   "Code.de.la.commune" = character(),
#'   "Libellé.de.la.commune" = character(),
#'   "Nom.de.l.élu" = character(),
#'   "Prénom.de.l.élu" = character(),
#'   "Code.sexe" = character(),
#'   "Date.de.naissance" = character(),
#'   "Code.de.la.catégorie.socio.professionnelle" = character(),
#'   "Libellé.de.la.catégorie.socio.professionnelle" = character(),
#'   "Date.de.début.du.mandat" = character(),
#'   "Libellé.de.la.fonction" = character(),
#'   "Date.de.début.de.la.fonction" = character(),
#'   "Code.nationalité" = character()
#' )
#' validate_schema(df)

validate_schema <- function(df) {
  schema <- c("Code.du.département", "Libellé.du.département", "Code.de.la.collectivité.à.statut.particulier",
              "Libellé.de.la.collectivité.à.statut.particulier", "Code.de.la.commune",
              "Libellé.de.la.commune", "Nom.de.l.élu", "Prénom.de.l.élu",
              "Code.sexe", "Date.de.naissance", "Code.de.la.catégorie.socio.professionnelle",
              "Libellé.de.la.catégorie.socio.professionnelle", "Date.de.début.du.mandat",
              "Libellé.de.la.fonction", "Date.de.début.de.la.fonction", "Code.nationalité"
  )

  if (!identical(colnames(df), schema)) {
    stop("Erreur : Le schéma du dataframe ne correspond pas au format attendu.")
  }

  message("Le schéma du dataframe est conforme.")
}



#' @title Compter le nombre d'élus
#' @name compter_nombre_d_elu
#' @description
#' Cette fonction permet de compter le nombre d'élus dans un dataframe qui
#' valide le schéma (mêmes noms et nombres de colonnes) du dataframe
#' élus-conseillers-municipaux-cm.
#'
#' @param df Un dataframe qui valide le schéma contenant les données
#' des élus conseillers municipaux.
#' @return Le nombre d'élus.
#'
#' @import dplyr

compter_nombre_d_elu <- function(df) {
  validate_schema(df)

  df |>
    select(Nom.de.l.élu, Prénom.de.l.élu, Date.de.naissance) |>
    distinct() |>
    nrow()
}



#' @title Compter le nombre d'adjoints
#' @name compter_nombre_d_adjoints
#' @description
#' Cette fonction permet de compter le nombre d'adjoints dans un dataframe
#' qui valide le schéma (mêmes noms et nombres de colonnes) du dataframe
#' élus-conseillers-municipaux-cm.
#'
#' @param df Un dataframe qui valide le schéma contenant les données
#' des élus conseillers municipaux.
#' @return Le nombre d'adjoints.
#'
#' @import stringr

compter_nombre_d_adjoints <- function(df) {
  validate_schema(df)

  sum(str_detect(df$Libellé.de.la.fonction, "adjoint"))
}



#' @title Trouver l'élu le plus âgé
#' @name trouver_l_elu_le_plus_age
#' @description
#' Cette fonction permet de l'élu le plus âgé dans un dataframe qui valide
#' le schéma (mêmes noms et nombres de colonnes) du dataframe
#' élus-conseillers-municipaux-cm.
#'
#' @param df Un dataframe qui valide le schéma contenant les données
#' des élus conseillers municipaux.
#' @return La fonction retourne l'élus le plus âgé dans le dataframe utilisé.
#'
#' @import dplyr
#' @import lubridate

trouver_l_elu_le_plus_age <- function(df) {
  validate_schema(df)

  df |>
    mutate(Date.de.naissance = dmy(Date.de.naissance)) |>
    slice(which.min(Date.de.naissance)) |>
    mutate(Âge = as.integer(interval(Date.de.naissance, Sys.Date()) / years(1))) |>
    select(Nom.de.l.élu, Prénom.de.l.élu, Date.de.naissance, Âge)
}



#' @title Distribution des âges
#' @name calcul_distribution_age
#' @description
#' Cette fonction calculera les quantiles 0, 25, 50, 75, 100 de l’âge des élu.e.s.
#'
#' @param df Un dataframe qui valide le schéma contenant les données
#' des élus conseillers municipaux.
#' @return La fonction retourne la distribution des âges dans chaque quantiles
#' dans le dataframe choisie.
#'
#' @import dplyr
#' @import lubridate


calcul_distribution_age <- function(df) {
  validate_schema(df)

  df <- df |>
    mutate(Date.de.naissance = dmy(Date.de.naissance)) |>
    mutate(Valid.Date = !is.na(Date.de.naissance))

  df <- df |>
    mutate(Age = as.integer(interval(start = Date.de.naissance, end = today()) / years(1)))

  quantiles <- quantile(df$Age, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)

  return(quantiles)
}



#' @title Graphique du nombre d'élus par catégorie socio professionnelle
#' @name plot_code_professions
#' @description
#' La fonction compte le nombre d'élus par catégorie socio professionnelle
#' différentes, dans un dataframe qui valide le schéma (mêmes noms et nombres de colonnes)
#' du dataframe élus-conseillers-municipaux-cm.
#'
#' @param df Un dataframe qui valide le schéma contenant les données
#' des élus conseillers municipaux.
#' @return La fonction retourne un diagramme en barres horizontal de la catégorie
#' socio professionnelle en fonction du nombre d'élus.
#'
#' @import dplyr
#' @import ggplot2

plot_code_professions <- function(df) {
  validate_schema(df)

  professions_code <- df |>
    group_by(Code.de.la.catégorie.socio.professionnelle) |>
    summarise(n = n()) |>
    arrange(desc(n))

  ggplot(professions_code,
         aes(x = n,
             y = reorder(Code.de.la.catégorie.socio.professionnelle, n))) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(title = paste("Nombre d'élus par catégorie socio professionnelle"),
         x = "Nombre d'élus", y = "Code Professionnel") +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 5))
}



#' @title Résumer des informations des objets de type 'communes'
#' @name summary.commune
#' @description
#' Fonction générique qui sert à résumer les informations des objets de type 'communes'.
#' Ces objets 'communes' sont des dataframes qui valide le schéma (mêmes noms et nombres de colonnes)
#' du dataframe élus-conseillers-municipaux-cm. Ces objets doivent être attribué de la classe 'commune',
#' ce qui permet de reconnaître que ces dataframes doivent être traités par la méthode spécifique
#' summary.commune lorsque la fonction summary() est appelée sur eux.
#'
#' @param x Un paramètre x qui sera un objet de type 'commune'.
#' @return La fonction retourne :
#' Le nom de la commune.
#' Le nombre d'élu.e.s dans la commune.
#' La distribution des âges des élu.e.s dans la commune.
#' Le nom, le prénom et l'âge de l'élu.e le ou la plus âgé.e de la commune.
#'
#' Lorsque l'objet n'est pas de type commune, la fonction retourne warning :
#' "L'objet doit être de type 'commune'."
#'
#' @examples
#' # Affectation de la classe 'commune' au dataframe df_Nantes.
#' class(df_Nantes) <- c("commune", class(df_Nantes))
#' # Ne pas exécuter plusieurs fois !
#'
#' summary(df_Nantes)
#' summary.commune(df_Nantes)
#'
#' @import dplyr
#' @import lubridate
#'
#' @export

summary.commune <- function(x) {

  if (!inherits(x, "commune")) {
    stop("L'objet doit être de type 'commune'.")
  }

  elu_plus_age <- trouver_l_elu_le_plus_age(x)

  cat("Nom de la commune :", unique(x$Libellé.de.la.commune), "\n")
  cat("Nombre d'élu.e.s :",  compter_nombre_d_elu(x), "\n")

  cat("Distribution des âges des élu.e.s :", "\n")
  print(calcul_distribution_age(x))

  cat("L'élu.e le ou la plus âgé.e :", "\n")
  cat("Nom :", elu_plus_age$Nom.de.l.élu, "\n")
  cat("Prénom :", elu_plus_age$Prénom.de.l.élu, "\n")
  cat("Âge :", elu_plus_age$Âge, "\n")

}


#' @title Résumer des informations des objets de type 'département'
#' @name summary.departement
#' @description
#' Fonction générique qui sert à résumer les informations des objets de type 'département'.
#' Ces objets 'département' sont des dataframes qui valide le schéma (mêmes noms et nombres de colonnes)
#' du dataframe élus-conseillers-municipaux-cm. Ces objets doivent être attribué de la classe 'département',
#' ce qui permet de reconnaître que ces dataframes doivent être traités par la méthode spécifique
#' summary.departement lorsque la fonction summary() est appelée sur eux.
#'
#' @param x Un paramètre x qui sera un objet de type 'département'.
#' @return La fonction retourne :
#' Le nom du département.
#' Le nombre de commune dans le département.
#' Le nombre d'élu.e.s dans le département.
#' La distribution des âges des élu.e.s dans le département.
#' Le nom, l'âge et la commune de l'élu.e le ou la plus âgé.e du département.
#' Le nom, l'âge et la commune de l'élu.e le ou la plus jeunes du département.
#' Le nom de la commune qui à la moyenne d’âge la plus faible, et la distribution des âges des
#' élu.e.s pour cette commune dans le département.
#' Le nom de la commune qui à la moyenne d’âge la plus élevé, et la distribution des âges des
#' élu.e.s pour cette commune dans le département.
#'
#' Lorsque l'objet n'est pas de type département, la fonction retourne warning :
#' "L'objet doit être de type 'département'."
#' @examples
#' # Affectation de la classe 'département' au dataframe df_Gers.
#' class(df_Gers) <- c("département", class(df_Gers))
#' # Ne pas exécuter plusieurs fois !
#'
#' summary(df_Gers)
#' summary.departement(df_Gers)
#'
#' @import dplyr
#' @import lubridate
#'
#' @export

summary.departement <- function(x) {

  if (!inherits(x, "département")) {
    stop("L'objet doit être de type 'département'.")
  }

  cat("Nom du département :", unique(x$Libellé.du.département), "\n")
  cat("Nombre de communes :", length(unique(x$Libellé.de.la.commune)), "\n")
  cat("Nombre d'élu.e.s :",  compter_nombre_d_elu(x), "\n")
  cat("Distribution des âges des élu.e.s :", "\n")
  print(calcul_distribution_age(x))

  trouver_elu_le_plus_age <- function(df) {
    validate_schema(df)
    df |>
      mutate(Date.de.naissance = dmy(Date.de.naissance)) |>
      slice(which.min(Date.de.naissance)) |>
      mutate(Âge = as.integer(interval(Date.de.naissance, Sys.Date()) / years(1)))
  }

  elu_max <- trouver_elu_le_plus_age(x)
  cat("Élu.e le/la plus âgé.e :",  elu_max$Nom.de.l.élu,
      ", Âge :",  elu_max$Âge,
      ", Commune :", elu_max$Libellé.de.la.commune, "\n")

  trouver_l_elu_le_plus_jeune <- function(df) {
    validate_schema(df)
    df |>
      mutate(Date.de.naissance = dmy(Date.de.naissance)) |>
      slice(which.max(Date.de.naissance)) |>
      mutate(Âge = as.integer(interval(Date.de.naissance, Sys.Date()) / years(1)))
  }

  elu_min <- trouver_l_elu_le_plus_jeune(x)
  cat("Élu.e le/la plus jeune :", elu_min$Nom.de.l.élu,
      ", Âge :", elu_min$Âge,
      ", Commune :", elu_min$Libellé.de.la.commune, "\n")

  x <- x |>
    mutate(Âge = as.integer(interval(dmy(Date.de.naissance), Sys.Date()) / years(1)))

  age_par_commune <- aggregate(Âge ~ Libellé.de.la.commune, data = x, FUN = mean)

  commune_min_age <- age_par_commune[which.min(age_par_commune$Âge), "Libellé.de.la.commune"]
  cat("Commune avec la moyenne d’âge la plus faible :", commune_min_age, "\n")
  print(summary(x$Âge[x$Libellé.de.la.commune == commune_min_age]))

  commune_max_age <- age_par_commune[which.max(age_par_commune$Âge), "Libellé.de.la.commune"]
  cat("Commune avec la moyenne d’âge la plus élevée :", commune_max_age, "\n")
  print(summary(x$Âge[x$Libellé.de.la.commune == commune_max_age]))

}



#' @title Graphique du nombre d'élus par catégorie socio professionnelle par commune
#' @name plot.commune
#' @description
#' La fonction compte le nombre d'élus par catégorie socio professionnelle
#' différentes, dans un dataframe qui contient une seule commune et qui valide le schéma
#' (mêmes noms et nombres de colonnes) du dataframe élus-conseillers-municipaux-cm.
#'
#' @param df Un dataframe qui contient une seule commune et qui valide le schéma
#' contenant les données des élus conseillers municipaux.
#' @return La fonction retourne un diagramme en barres horizontal de la catégorie
#' socio professionnelle en fonction du nombre d'élus de la commune.
#'
#' Lorsque le dataframe contient plusieurs communes, il retourne Warning :
#' "Le data.frame contient plusieurs communes. Veuillez filtrer pour une seule commune."
#'
#' @import dplyr
#' @import ggplot2
#'
#' @export

plot.commune <- function(df) {
  validate_schema(df)

  commune_name <- unique(df$Libellé.de.la.commune)
  departement_name <- unique(df$Libellé.du.département)

  if (length(commune_name) > 1) {
    stop("Le data.frame contient plusieurs communes. Veuillez filtrer pour une seule commune.")
  }

  professions_code <- df |>
    group_by(Code.de.la.catégorie.socio.professionnelle) |>
    summarise(n = n(), .groups = "drop") |>
    arrange(desc(n))

  ggplot(professions_code,
         aes(x = n,
             y = reorder(Code.de.la.catégorie.socio.professionnelle, n))) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(
      title = paste(commune_name, "-", departement_name),
      x = paste("Libellés des codes professionnels pour les élus (", compter_nombre_d_elu(df), " élus)", sep = ""),
      y = "Code Professionnel"
    ) +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 5))
}



#' @title Graphique du nombre d'élus par catégorie socio professionnelle par départements
#' @name plot.departement
#' @description
#' Cette fonction génère un graphique en barres horizontal représentant la distribution
#' des élu.e.s municipaux selon leur catégorie socio-professionnelle pour un département donné.
#' Elle sélectionne automatiquement les 10 catégories les plus représentées en nombre d'élu.e.s.
#'
#' @param df Un data.frame de classe "departement" contenant les données des élus.
#'           Doit contenir les colonnes :
#'           \itemize{
#'             \item Code.du.département
#'             \item Libellé.de.la.commune
#'             \item Code.de.la.catégorie.socio.professionnelle
#'           }
#'
#' @return Un objet ggplot2 représentant un diagramme en barres horizontal avec :
#'         \itemize{
#'           \item En ordonnée : les codes des catégories socio-professionnelles
#'           \item En abscisse : le nombre d'élus
#'           \item Un titre incluant le nom du département et le nombre de communes
#'           \item Les barres triées par ordre décroissant du nombre d'élus
#'           \item Les 10 codes professionnels les plus importants en nombre d’élu.e.s du département
#'         }
#'
#' @details
#' La fonction réalise les étapes suivantes :
#' \enumerate{
#'   \item Vérifie la validité du schéma des données
#'   \item Extrait le nom du département et compte le nombre de communes uniques
#'   \item Vérifie qu'il n'y a qu'un seul département dans les données
#'   \item Compte les occurrences de chaque code professionnel
#'   \item Sélectionne les 10 codes les plus fréquents
#'   \item Crée un graphique avec ggplot2 utilisant :
#'         - Une couleur steelblue pour les barres
#'         - Un thème minimal
#'         - Une taille de texte réduite (5) pour les codes sur l'axe Y
#' }
#'
#' @section Erreurs:
#' La fonction s'arrête avec une erreur si :
#' \itemize{
#'   \item Le data.frame contient des données de plusieurs départements
#'   \item Le schéma des données n'est pas valide
#' }
#'
#' @import dplyr
#' @import ggplot2
#'
#' @export

plot.departement <- function(df) {
  validate_schema(df)

  departement_name <- unique(df$Libellé.du.département)
  nombre_communes <- length(unique(df$Libellé.de.la.commune))

  if (length(departement_name) > 1) {
    stop("Le data.frame contient plusieurs départements. Veuillez filtrer pour un seul département.")
  }

  professions_code <- df |>
    group_by(Code.de.la.catégorie.socio.professionnelle) |>
    summarise(n = n(), .groups = "drop") |>
    arrange(desc(n)) |>
    slice_max(n, n = 10)

  ggplot(professions_code,
         aes(x = n,
             y = reorder(Code.de.la.catégorie.socio.professionnelle, n))) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(
      title = paste(departement_name, "-", nombre_communes, "communes"),
      x = paste("Libellés des 10 codes professionnels les plus représentés pour le département", departement_name),
      y = "Code Professionnel"
    ) +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 5))
}


#' @title Créer un objet de classe commune
#' @name creer_commune
#' @description
#' Cette fonction prend en entrée un data.frame contenant des données communales
#' et crée un objet de classe "commune". Elle vérifie que toutes les données
#' appartiennent à une seule et même commune.
#'
#' @param df Un data.frame contenant les données d'une commune.
#'           Doit contenir une colonne 'Code.de.la.commune'.
#'
#' @return Un data.frame de classe "commune" contenant les données d'une seule commune.
#'
#' @details
#' La fonction effectue les vérifications suivantes :
#' \itemize{
#'   \item Valide le schéma des données via la fonction validate_schema()
#'   \item Vérifie que toutes les lignes correspondent à une seule commune
#'   \item Ajoute la classe "commune" si elle n'existe pas déjà
#' }
#'
#'Une erreur est levée si le `data.frame` contient des données de plusieurs communes.
#'
#' @examples
#' \dontrun{
#' # Créer un objet commune à partir d'un data.frame
#' data_commune <- data.frame(
#'   Code.de.la.commune = rep("01001", 3),
#'   Nom.de.l.élu = c("Dupont", "Martin", "Durand"),
#'   Prénom.de.l.élu = c("Jean", "Marie", "Pierre"),
#'   etc.
#' )
#' ma_commune <- creer_commune(data_commune)
#' }
#'
#' @export

creer_commune <- function(df) {
  validate_schema(df)

  unique_commune_code <- df$Code.de.la.commune |>
    unique() |>
    length()

  if(unique_commune_code > 1) {
    stop("Le dataframe est composé de plusieurs communes")
  }

  if(!inherits(df, "commune")) {
    class(df) <- c("commune", class(df))
  }

  return(df)
}



#' @title Créer un objet de classe département
#' @name creer_departement
#'
#' @description
#' Cette fonction prend en entrée un data.frame contenant des données départementales
#' et crée un objet de classe "departement". Elle vérifie que toutes les données
#' appartiennent à un seul et même département.
#'
#' @param df Un data.frame contenant les données d'un département.
#'           Doit contenir une colonne 'Code.du.département'.
#'
#' @return Un data.frame de classe "departement" contenant les données d'un seul département.
#'
#' @details
#' La fonction effectue les vérifications suivantes :
#' \itemize{
#'   \item Valide le schéma des données via la fonction validate_schema()
#'   \item Vérifie que toutes les lignes correspondent à un seul département
#'   \item Ajoute la classe "departement" si elle n'existe pas déjà
#' }
#'
#' Une erreur est levée si le `data.frame` contient des données de plusieurs départements.
#'
#' @examples
#' \dontrun{
#' # Créer un objet commune à partir d'un data.frame
#' data_departement <- data.frame(
#'   Code.du.département = rep("01", 3),
#'   Libellé.du.département = rep("Ain", 3),
#'   Code.de.la.commune = c("01001", "01002", "01003"),
#'   Nom.de.l.élu = c("Dupont", "Martin", "Durand"),
#'   Prénom.de.l.élu = c("Jean", "Marie", "Pierre"),
#'   etc.
#' )
#' mon_departement <- creer_departement(data_departement)
#' }
#'
#' @export

creer_departement <- function(df) {
  validate_schema(df)

  unique_dept_code <- df$Code.du.département |>
    unique() |>
    length()

  if(unique_dept_code > 1) {
    stop("Le dataframe est composé de plusieurs départements")
  }

  if(!inherits(df, "departement")) {
    class(df) <- c("departement", class(df))
  }

  return(df)
}
