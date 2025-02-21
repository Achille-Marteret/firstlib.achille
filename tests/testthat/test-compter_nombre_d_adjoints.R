library(testthat)
library(dplyr)
library(stringr)

test_that("compter_nombre_d_adjoints gère correctement les entrées valides", {
  # Création d'un dataframe valide avec des adjoints
  df_valide <- data.frame(
    "Code.du.département" = c("01", "01", "02"),
    "Libellé.du.département" = c("Ain", "Ain", "Aisne"),
    "Code.de.la.collectivité.à.statut.particulier" = c("001", "001", "002"),
    "Libellé.de.la.collectivité.à.statut.particulier" = c("Collectivité 1", "Collectivité 1", "Collectivité 2"),
    "Code.de.la.commune" = c("0001", "0001", "0002"),
    "Libellé.de.la.commune" = c("Commune 1", "Commune 1", "Commune 2"),
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

  # Test 1 : Vérifie que la fonction compte correctement le nombre d'adjoints
  expect_equal(compter_nombre_d_adjoints(df_valide), 2, "Le nombre d'adjoints devrait être 2.")

  # Test 2 : Vérifie que la fonction retourne 0 lorsqu'il n'y a pas d'adjoints
  df_sans_adjoints <- df_valide
  df_sans_adjoints$Libellé.de.la.fonction <- c("Maire", "Conseiller", "Conseiller municipal")
  expect_equal(compter_nombre_d_adjoints(df_sans_adjoints), 0, "Le nombre d'adjoints devrait être 0.")
})

test_that("compter_nombre_d_adjoints gère correctement les entrées invalides", {
  # Test 3 : Vérifie que la fonction génère une erreur pour un dataframe avec un schéma incorrect
  df_invalide <- df_valide
  colnames(df_invalide)[1] <- "Code.du.departement"  # Modification du nom de colonne
  expect_error(compter_nombre_d_adjoints(df_invalide), "Erreur : Le schéma du dataframe ne correspond pas au format attendu.")

  # Test 4 : Vérifie que la fonction génère une erreur pour un dataframe vide
  df_vide <- data.frame()
  expect_error(compter_nombre_d_adjoints(df_vide), "Erreur : Le schéma du dataframe ne correspond pas au format attendu.")
})
