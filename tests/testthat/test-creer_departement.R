library(testthat)
library(dplyr)

test_that("creer_departement ajoute correctement la classe 'departement' à un dataframe valide", {
  # Création d'un dataframe valide avec un seul département
  df_valide <- data.frame(
    "Code.du.département" = c("01", "01", "01"),
    "Libellé.du.département" = c("Ain", "Ain", "Ain"),
    "Code.de.la.collectivité.à.statut.particulier" = c("001", "001", "001"),
    "Libellé.de.la.collectivité.à.statut.particulier" = c("Collectivité 1", "Collectivité 1", "Collectivité 1"),
    "Code.de.la.commune" = c("0001", "0002", "0003"),
    "Libellé.de.la.commune" = c("Commune 1", "Commune 2", "Commune 3"),
    "Nom.de.l.élu" = c("Dupont", "Martin", "Durand"),
    "Prénom.de.l.élu" = c("Jean", "Marie", "Paul"),
    "Code.sexe" = c("M", "F", "M"),
    "Date.de.naissance" = c("01/01/1980", "01/01/1990", "01/01/1970"),
    "Code.de.la.catégorie.socio.professionnelle" = c("CSP1", "CSP2", "CSP1"),
    "Libellé.de.la.catégorie.socio.professionnelle" = c("Catégorie 1", "Catégorie 2", "Catégorie 1"),
    "Date.de.début.du.mandat" = c("01/01/2020", "01/01/2021", "01/01/2019"),
    "Libellé.de.la.fonction" = c("Maire", "Adjoint", "Adjoint au maire"),
    "Date.de.début.de.la.fonction" = c("01/01/2020", "01/01/2021", "01/01/2019"),
    "Code.nationalité" = c("FR", "FR", "FR"),
    stringsAsFactors = FALSE
  )

  # Test 1 : Vérifie que la fonction ajoute la classe 'departement' sans erreur
  departement_obj <- creer_departement(df_valide)
  expect_true(inherits(departement_obj, "departement"), "L'objet devrait hériter de la classe 'departement'.")

  # Test 2 : Vérifie que la fonction génère une erreur pour un dataframe avec plusieurs départements
  df_plusieurs_departements <- df_valide
  df_plusieurs_departements$Code.du.département[1] <- "02"
  expect_error(creer_departement(df_plusieurs_departements), "Le dataframe est composé de plusieurs départements")
})
