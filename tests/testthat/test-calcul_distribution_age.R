library(testthat)
library(dplyr)
library(lubridate)

test_that("calcul_distribution_age calcule correctement les quantiles d'âge", {
  # Création d'un dataframe valide avec des élus de différents âges
  df_valide <- data.frame(
    "Code.du.département" = c("01", "01", "02", "03", "04"),
    "Libellé.du.département" = c("Ain", "Ain", "Aisne", "Allier", "Alpes-de-Haute-Provence"),
    "Code.de.la.collectivité.à.statut.particulier" = c("001", "001", "002", "003", "004"),
    "Libellé.de.la.collectivité.à.statut.particulier" = c("Collectivité 1", "Collectivité 1", "Collectivité 2", "Collectivité 3", "Collectivité 4"),
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

  # Test 1 : Vérifie que la fonction calcule correctement les quantiles d'âge
  quantiles <- calcul_distribution_age(df_valide)
  ages <- c(44, 34, 24, 14, 0)  # Âges calculés pour les quantiles
  expect_equal(quantiles, quantile(ages, probs = c(0, 0.25, 0.5, 0.75, 1)), "Les quantiles d'âge calculés sont incorrects.")

  # Test 2 : Vérifie que la fonction gère correctement un dataframe avec des dates de naissance identiques
  df_meme_date <- data.frame(
    "Code.du.département" = c("01", "01", "01"),
    "Libellé.du.département" = c("Ain", "Ain", "Ain"),
    "Code.de.la.collectivité.à.statut.particulier" = c("001", "001", "001"),
    "Libellé.de.la.collectivité.à.statut.particulier" = c("Collectivité 1", "Collectivité 1", "Collectivité 1"),
    "Code.de.la.commune" = c("0001", "0001", "0001"),
    "Libellé.de.la.commune" = c("Commune 1", "Commune 1", "Commune 1"),
    "Nom.de.l.élu" = c("Dupont", "Martin", "Durand"),
    "Prénom.de.l.élu" = c("Jean", "Marie", "Paul"),
    "Code.sexe" = c("M", "F", "M"),
    "Date.de.naissance" = c("01/01/1980", "01/01/1980", "01/01/1980"),
    "Code.de.la.catégorie.socio.professionnelle" = c("CSP1", "CSP2", "CSP3"),
    "Libellé.de.la.catégorie.socio.professionnelle" = c("Catégorie 1", "Catégorie 2", "Catégorie 3"),
    "Date.de.début.du.mandat" = c("01/01/2020", "01/01/2021", "01/01/2019"),
    "Libellé.de.la.fonction" = c("Maire", "Adjoint", "Adjoint au maire"),
    "Date.de.début.de.la.fonction" = c("01/01/2020", "01/01/2021", "01/01/2019"),
    "Code.nationalité" = c("FR", "FR", "FR"),
    stringsAsFactors = FALSE
  )
  quantiles_meme_date <- calcul_distribution_age(df_meme_date)
  expect_equal(quantiles_meme_date, quantile(c(44, 44, 44), probs = c(0, 0.25, 0.5, 0.75, 1)), "Les quantiles d'âge calculés sont incorrects pour des dates de naissance identiques.")
})

test_that("calcul_distribution_age gère correctement les entrées invalides", {
  # Test 3 : Vérifie que la fonction génère une erreur pour un dataframe avec un schéma incorrect
  df_invalide <- df_valide
  colnames(df_invalide)[1] <- "Date.de.naissance"  # Modification du nom de colonne pour simuler un schéma incorrect
  expect_error(calcul_distribution_age(df_invalide), "Erreur : Le schéma du dataframe ne correspond pas au format attendu.")
})
