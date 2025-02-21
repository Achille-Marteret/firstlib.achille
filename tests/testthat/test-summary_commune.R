library(testthat)
library(dplyr)
library(lubridate)

# Supposons que 'commune' est une classe S3, nous devons créer un objet de cette classe pour les tests
commune <- function(df) {
  structure(df, class = c("commune", "data.frame"))
}

test_that("summary.commune génère un résumé correct pour un objet de type 'commune'", {
  # Création d'un dataframe valide et transformation en objet de type 'commune'
  df_valide <- data.frame(
    "Code.du.département" = c("01", "01", "01"),
    "Libellé.du.département" = c("Ain", "Ain", "Ain"),
    "Code.de.la.collectivité.à.statut.particulier" = c("001", "001", "001"),
    "Libellé.de.la.collectivité.à.statut.particulier" = c("Collectivité 1", "Collectivité 1", "Collectivité 1"),
    "Code.de.la.commune" = c("0001", "0001", "0001"),
    "Libellé.de.la.commune" = c("Commune 1", "Commune 1", "Commune 1"),
    "Nom.de.l.élu" = c("Dupont", "Martin", "Durand"),
    "Prénom.de.l.élu" = c("Jean", "Marie", "Paul"),
    "Code.sexe" = c("M", "F", "M"),
    "Date.de.naissance" = c("01/01/1980", "01/01/1990", "01/01/1970"),
    "Code.de.la.catégorie.socio.professionnelle" = c("CSP1", "CSP2", "CSP3"),
    "Libellé.de.la.catégorie.socio.professionnelle" = c("Catégorie 1", "Catégorie 2", "Catégorie 3"),
    "Date.de.début.du.mandat" = c("01/01/2020", "01/01/2021", "01/01/2019"),
    "Libellé.de.la.fonction" = c("Maire", "Adjoint", "Adjoint au maire"),
    "Date.de.début.de.la.fonction" = c("01/01/2020", "01/01/2021", "01/01/2019"),
    "Code.nationalité" = c("FR", "FR", "FR"),
    stringsAsFactors = FALSE
  )
  commune_obj <- commune(df_valide)

  # Test 1 : Vérifie que la fonction génère un résumé sans erreur
  expect_silent(summary.commune(commune_obj))

  # Capturer la sortie de la fonction pour vérifier les résultats
  output <- capture.output(summary.commune(commune_obj))

  # Vérifier que les informations attendues sont présentes dans la sortie
  expect_true(any(grepl("Nom de la commune : Commune 1", output)), "Le nom de la commune devrait être affiché.")
  expect_true(any(grepl("Nombre d'élu.e.s : 3", output)), "Le nombre d'élus devrait être affiché.")
  expect_true(any(grepl("L'élu.e le ou la plus âgé.e :", output)), "Les informations sur l'élu le plus âgé devraient être affichées.")
  expect_true(any(grepl("Nom : Durand", output)), "Le nom de l'élu le plus âgé devrait être affiché.")
  expect_true(any(grepl("Prénom : Paul", output)), "Le prénom de l'élu le plus âgé devrait être affiché.")
  expect_true(any(grepl("Âge :", output)), "L'âge de l'élu le plus âgé devrait être affiché.")
})

test_that("summary.commune gère correctement les entrées invalides", {
  # Test 2 : Vérifie que la fonction génère une erreur pour un objet non 'commune'
  expect_error(summary.commune(df_valide), "L'objet doit être de type 'commune'.")
})
