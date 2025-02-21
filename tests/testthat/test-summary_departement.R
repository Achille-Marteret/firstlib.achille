library(testthat)
library(dplyr)
library(lubridate)

# Supposons que 'département' est une classe S3, nous devons créer un objet de cette classe pour les tests
departement <- function(df) {
  structure(df, class = c("département", "data.frame"))
}

test_that("summary.departement génère un résumé correct pour un objet de type 'département'", {
  # Création d'un dataframe valide et transformation en objet de type 'département'
  df_valide <- data.frame(
    "Code.du.département" = c("01", "01", "01", "02", "02"),
    "Libellé.du.département" = c("Ain", "Ain", "Ain", "Aisne", "Aisne"),
    "Code.de.la.collectivité.à.statut.particulier" = c("001", "001", "001", "002", "002"),
    "Libellé.de.la.collectivité.à.statut.particulier" = c("Collectivité 1", "Collectivité 1", "Collectivité 1", "Collectivité 2", "Collectivité 2"),
    "Code.de.la.commune" = c("0001", "0001", "0002", "0003", "0004"),
    "Libellé.de.la.commune" = c("Commune 1", "Commune 1", "Commune 2", "Commune 3", "Commune 4"),
    "Nom.de.l.élu" = c("Dupont", "Martin", "Durand", "Dubois", "Moreau"),
    "Prénom.de.l.élu" = c("Jean", "Marie", "Paul", "Alice", "Luc"),
    "Code.sexe" = c("M", "F", "M", "F", "M"),
    "Date.de.naissance" = c("01/01/1980", "01/01/1990", "01/01/1970", "01/01/1960", "01/01/2000"),
    "Code.de.la.catégorie.socio.professionnelle" = c("CSP1", "CSP2", "CSP3", "CSP4", "CSP5"),
    "Libellé.de.la.catégorie.socio.professionnelle" = c("Catégorie 1", "Catégorie 2", "Catégorie 3", "Catégorie 4", "Catégorie 5"),
    "Date.de.début.du.mandat" = c("01/01/2020", "01/01/2021", "01/01/2019", "01/01/2018", "01/01/2022"),
    "Libellé.de.la.fonction" = c("Maire", "Adjoint", "Adjoint au maire", "Conseiller", "Conseiller municipal"),
    "Date.de.début.de.la.fonction" = c("01/01/2020", "01/01/2021", "01/01/2019", "01/01/2018", "01/01/2022"),
    "Code.nationalité" = c("FR", "FR", "FR", "FR", "FR"),
    stringsAsFactors = FALSE
  )
  departement_obj <- departement(df_valide)

  # Test 1 : Vérifie que la fonction génère un résumé sans erreur
  expect_silent(summary.departement(departement_obj))

  # Capturer la sortie de la fonction pour vérifier les résultats
  output <- capture.output(summary.departement(departement_obj))

  # Vérifier que les informations attendues sont présentes dans la sortie
  expect_true(any(grepl("Nom du département : Ain", output)), "Le nom du département devrait être affiché.")
  expect_true(any(grepl("Nombre de communes : 3", output)), "Le nombre de communes devrait être affiché.")
  expect_true(any(grepl("Nombre d'élu.e.s : 5", output)), "Le nombre d'élus devrait être affiché.")
  expect_true(any(grepl("Élu.e le/la plus âgé.e :", output)), "Les informations sur l'élu le plus âgé devraient être affichées.")
  expect_true(any(grepl("Élu.e le/la plus jeune :", output)), "Les informations sur l'élu le plus jeune devraient être affichées.")
  expect_true(any(grepl("Commune avec la moyenne d’âge la plus faible :", output)), "Les informations sur la commune avec la moyenne d'âge la plus faible devraient être affichées.")
  expect_true(any(grepl("Commune avec la moyenne d’âge la plus élevée :", output)), "Les informations sur la commune avec la moyenne d'âge la plus élevée devraient être affichées.")
})

test_that("summary.departement gère correctement les entrées invalides", {
  # Test 2 : Vérifie que la fonction génère une erreur pour un objet non 'département'
  expect_error(summary.departement(df_valide), "L'objet doit être de type 'département'.")
})
