library(testthat)

test_that("validate_schema vérifie correctement le schéma du dataframe", {
  # Création d'un dataframe avec le bon schéma
  df_valide <- data.frame(
    "Code.du.département" = "01",
    "Libellé.du.département" = "Ain",
    "Code.de.la.collectivité.à.statut.particulier" = "001",
    "Libellé.de.la.collectivité.à.statut.particulier" = "Collectivité",
    "Code.de.la.commune" = "0001",
    "Libellé.de.la.commune" = "Commune",
    "Nom.de.l.élu" = "Dupont",
    "Prénom.de.l.élu" = "Jean",
    "Code.sexe" = "M",
    "Date.de.naissance" = "01/01/1980",
    "Code.de.la.catégorie.socio.professionnelle" = "CSP1",
    "Libellé.de.la.catégorie.socio.professionnelle" = "Catégorie",
    "Date.de.début.du.mandat" = "01/01/2020",
    "Libellé.de.la.fonction" = "Maire",
    "Date.de.début.de.la.fonction" = "01/01/2020",
    "Code.nationalité" = "FR",
    stringsAsFactors = FALSE
  )

  # Test 1 : Vérifie que la fonction ne génère pas d'erreur pour un dataframe valide
  expect_message(validate_schema(df_valide), "Le schéma du dataframe est conforme.")

  # Création d'un dataframe avec un schéma incorrect
  df_invalide <- df_valide
  colnames(df_invalide)[1] <- "Code.du.departement"  # Modification du nom de colonne

  # Test 2 : Vérifie que la fonction génère une erreur pour un dataframe invalide
  expect_error(validate_schema(df_invalide), "Erreur : Le schéma du dataframe ne correspond pas au format attendu.")
})
