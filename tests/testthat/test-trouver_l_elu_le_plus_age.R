library(testthat)
library(dplyr)
library(lubridate)

test_that("trouver_l_elu_le_plus_age trouve correctement le nom, le prénom et l'âge de l'élu le plus âgé", {
  # Création d'un dataframe valide avec des élus de différents âges
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

  # Test 1 : Vérifie que la fonction trouve correctement l'élu le plus âgé
  resultat <- trouver_l_elu_le_plus_age(df_valide)
  expect_equal(resultat$Nom.de.l.élu, "Durand", "Le nom de l'élu le plus âgé devrait être Durand.")
  expect_equal(resultat$Prénom.de.l.élu, "Paul", "Le prénom de l'élu le plus âgé devrait être Paul.")
  expect_equal(resultat$Âge, as.integer(interval(dmy("01/01/1970"), Sys.Date()) / years(1)), "L'âge calculé est incorrect.")

  # Test 2 : Vérifie que la fonction gère correctement un dataframe avec des élus du même âge
  df_meme_age <- data.frame(
    "Code.du.département" = c("01", "01"),
    "Libellé.du.département" = c("Ain", "Ain"),
    "Code.de.la.collectivité.à.statut.particulier" = c("001", "001"),
    "Libellé.de.la.collectivité.à.statut.particulier" = c("Collectivité 1", "Collectivité 1"),
    "Code.de.la.commune" = c("0001", "0001"),
    "Libellé.de.la.commune" = c("Commune 1", "Commune 1"),
    "Nom.de.l.élu" = c("Dupont", "Martin"),
    "Prénom.de.l.élu" = c("Jean", "Marie"),
    "Code.sexe" = c("M", "F"),
    "Date.de.naissance" = c("01/01/1980", "01/01/1980"),
    "Code.de.la.catégorie.socio.professionnelle" = c("CSP1", "CSP2"),
    "Libellé.de.la.catégorie.socio.professionnelle" = c("Catégorie 1", "Catégorie 2"),
    "Date.de.début.du.mandat" = c("01/01/2020", "01/01/2021"),
    "Libellé.de.la.fonction" = c("Maire", "Adjoint"),
    "Date.de.début.de.la.fonction" = c("01/01/2020", "01/01/2021"),
    "Code.nationalité" = c("FR", "FR"),
    stringsAsFactors = FALSE
  )
  resultat_meme_age <- trouver_l_elu_le_plus_age(df_meme_age)
  expect_true(resultat_meme_age$Nom.de.l.élu %in% c("Dupont", "Martin"), "Le nom de l'élu le plus âgé devrait être Dupont ou Martin.")
  expect_true(resultat_meme_age$Prénom.de.l.élu %in% c("Jean", "Marie"), "Le prénom de l'élu le plus âgé devrait être Jean ou Marie.")
})

test_that("trouver_l_elu_le_plus_age gère correctement les entrées invalides", {
  # Test 3 : Vérifie que la fonction génère une erreur pour un dataframe avec un schéma incorrect
  df_invalide <- df_valide
  colnames(df_invalide)[1] <- "Date.de.naissance"  # Modification du nom de colonne pour simuler un schéma incorrect
  expect_error(trouver_l_elu_le_plus_age(df_invalide), "Erreur : Le schéma du dataframe ne correspond pas au format attendu.")
})
