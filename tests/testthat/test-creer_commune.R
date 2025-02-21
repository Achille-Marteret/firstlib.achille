library(testthat)
library(dplyr)

test_that("creer_commune ajoute correctement la classe 'commune' à un dataframe valide", {
  # Création d'un dataframe valide avec une seule commune
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
    "Code.de.la.catégorie.socio.professionnelle" = c("CSP1", "CSP2", "CSP1"),
    "Libellé.de.la.catégorie.socio.professionnelle" = c("Catégorie 1", "Catégorie 2", "Catégorie 1"),
    "Date.de.début.du.mandat" = c("01/01/2020", "01/01/2021", "01/01/2019"),
    "Libellé.de.la.fonction" = c("Maire", "Adjoint", "Adjoint au maire"),
    "Date.de.début.de.la.fonction" = c("01/01/2020", "01/01/2021", "01/01/2019"),
    "Code.nationalité" = c("FR", "FR", "FR"),
    stringsAsFactors = FALSE
  )

  # Test 1 : Vérifie que la fonction ajoute la classe 'commune' sans erreur
  commune_obj <- creer_commune(df_valide)
  expect_true(inherits(commune_obj, "commune"), "L'objet devrait hériter de la classe 'commune'.")

  # Test 2 : Vérifie que la fonction génère une erreur pour un dataframe avec plusieurs communes
  df_plusieurs_communes <- df_valide
  df_plusieurs_communes$Code.de.la.commune[1] <- "0002"
  expect_error(creer_commune(df_plusieurs_communes), "Le dataframe est composé de plusieurs communes")
})
