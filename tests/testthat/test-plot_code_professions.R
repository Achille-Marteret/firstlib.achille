library(testthat)
library(dplyr)
library(ggplot2)

test_that("plot_code_professions génère correctement un graphique", {
  # Création d'un dataframe valide avec des catégories socio-professionnelles
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
    "Code.de.la.catégorie.socio.professionnelle" = c("CSP1", "CSP2", "CSP1", "CSP3", "CSP2"),
    "Libellé.de.la.catégorie.socio.professionnelle" = c("Catégorie 1", "Catégorie 2", "Catégorie 1", "Catégorie 3", "Catégorie 2"),
    "Date.de.début.du.mandat" = c("01/01/2020", "01/01/2021", "01/01/2019", "01/01/2018", "01/01/2022"),
    "Libellé.de.la.fonction" = c("Maire", "Adjoint", "Adjoint au maire", "Conseiller", "Conseiller municipal"),
    "Date.de.début.de.la.fonction" = c("01/01/2020", "01/01/2021", "01/01/2019", "01/01/2018", "01/01/2022"),
    "Code.nationalité" = c("FR", "FR", "FR", "FR", "FR"),
    stringsAsFactors = FALSE
  )

  # Test 1 : Vérifie que la fonction génère un graphique sans erreur
  plot <- plot_code_professions(df_valide)
  expect_s3_class(plot, "ggplot", "Le résultat devrait être un objet ggplot.")

  # Test 2 : Vérifie que le graphique contient les éléments attendus
  plot_data <- layer_data(plot)
  expect_true(any(plot_data$n == 2), "Le graphique devrait contenir une barre avec 2 élus.")
  expect_true(any(plot_data$n == 1), "Le graphique devrait contenir une barre avec 1 élu.")
  expect_true(any(plot_data$y %in% c("CSP1", "CSP2", "CSP3")), "Le graphique devrait contenir les codes professionnels attendus.")
})

test_that("plot_code_professions gère correctement les entrées invalides", {
  # Test 3 : Vérifie que la fonction génère une erreur pour un dataframe avec un schéma incorrect
  df_invalide <- df_valide
  colnames(df_invalide)[1] <- "Date.de.naissance"  # Modification du nom de colonne pour simuler un schéma incorrect
  expect_error(plot_code_professions(df_invalide), "Erreur : Le schéma du dataframe ne correspond pas au format attendu.")
})
