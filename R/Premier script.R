#' @title Compter le nombre d'élus
#' @name compter_nombre_d_elu
#' @description
#' Fonction qui permet de compter le nombre d'élus dans le dataframe élu_conseiller_municipaux
#' @param df = le dataframe élu_conseiller_municipaux
#' @return Le nombre d'élus
#' @export
#' @import dplyr


compter_nombre_d_elu <- function(df) {
  schema <- c("Code.du.département", "Libellé.du.département", "Code.de.la.collectivité.à.statut.particulier",
              "Libellé.de.la.collectivité.à.statut.particulier", "Code.de.la.commune",
              "Libellé.de.la.commune", "Nom.de.l.élu", "Prénom.de.l.élu",
              "Code.sexe", "Date.de.naissance", "Code.de.la.catégorie.socio.professionnelle",
              "Libellé.de.la.catégorie.socio.professionnelle", "Date.de.début.du.mandat",
              "Libellé.de.la.fonction", "Date.de.début.de.la.fonction", "Code.nationalité"
  )
  stopifnot(identical(colnames(df), schema))

  df |>
    select(Nom.de.l.élu, Prénom.de.l.élu, Date.de.naissance) |>
    distinct() |>
    nrow()
}
