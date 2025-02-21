#' @title Validation du schéma du dataframe élus-conseillers-municipaux-cm
#' @name validate_schema
#' @description
#' Cette fonction vérifie que le dataframe fourni respecte le schéma attendu
#' pour les données des élus conseillers municipaux. Si le schéma ne correspond
#' pas aux spécifications, la fonction génère une erreur détaillée.
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
#' Cette fonction calcule le nombre total d'élus à partir d'un dataframe
#' respectant le schéma des données des élus conseillers municipaux.
#' Le dataframe doit contenir les mêmes colonnes que le fichier
#' élus-conseillers-municipaux-cm.
#'
#' @param df Un dataframe qui valide le schéma contenant les données
#' des élus conseillers municipaux.
#' @return  Un entier représentant le nombre total d'élus dans le dataframe
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
#' Cette fonction calcule le nombre total d'adjoints au maire à partir d'un
#' dataframe respectant le schéma des données des élus conseillers municipaux.
#' Le dataframe doit contenir les mêmes colonnes que le fichier
#' élus-conseillers-municipaux-cm.
#'
#' @param df Un dataframe qui valide le schéma contenant les données
#' des élus conseillers municipaux.
#' @return  Un entier représentant le nombre total d'adjoints dans le dataframe
#'
#' @import stringr
#' @import dplyr

compter_nombre_d_adjoints <- function(df) {
  validate_schema(df)

  sum(str_detect(df$Libellé.de.la.fonction, "adjoint"))
}



#' @title Trouver l'élu le plus âgé
#' @name trouver_l_elu_le_plus_age
#' @description
#' Cette fonction identifie l'élu le plus âgé dans un dataframe respectant
#' le schéma des données des élus conseillers municipaux. Elle retourne les
#' informations de l'élu (nom, prénom, date de naissance et âge calculé).
#' Le dataframe doit contenir les mêmes colonnes que le fichier
#' élus-conseillers-municipaux-cm.
#'
#' @param df Un dataframe qui valide le schéma contenant les données
#' des élus conseillers municipaux.
#' @return Un dataframe d'une ligne contenant le nom, prénom, date de naissance
#'         et âge de l'élu le plus âgé
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
#' Cette fonction calcule la distribution des âges des élus en utilisant les
#' quantiles 0% (minimum), 25%, 50% (médiane), 75% et 100% (maximum).
#' Les âges sont calculés à partir des dates de naissance jusqu'à la date
#' du jour. Les dates non valides sont exclues du calcul.
#'
#' @param df Un dataframe qui valide le schéma contenant les données
#' des élus conseillers municipaux.
#'
#' @return Un vecteur nommé contenant les 5 quantiles calculés (0%, 25%, 50%, 75%, 100%)
#'         des âges des élus
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
#' Cette fonction crée un diagramme en barres horizontal représentant la
#' distribution des élus selon leur catégorie socio-professionnelle.
#' Les données doivent provenir d'un dataframe respectant le schéma du
#' fichier élus-conseillers-municipaux-cm. Les catégories sont triées
#' par nombre d'élus décroissant.
#'
#' @param df Un dataframe qui valide le schéma contenant les données
#' des élus conseillers municipaux.
#'
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

