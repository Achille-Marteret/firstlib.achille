library(testthat)
library(dplyr)
library(ggplot2)

test_that("plot.commune génère correctement un graphique pour une seule commune", {
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

  # Test 1 : Vérifie que la fonction génère un graphique sans erreur
  plot <- plot.commune(df_valide)
  expect_s3_class(plot, "ggplot", "Le résultat devrait être un objet ggplot.")

  # Test 2 : Vérifie que le graphique contient les éléments attendus
  plot_data <- layer_data(plot)
  expect_true(any(plot_data$n == 2), "Le graphique devrait contenir une barre avec 2 élus.")
  expect_true(any(plot_data$n == 1), "Le graphique devrait contenir une barre avec 1 élu.")
  expect_true(any(plot_data$y %in% c("CSP1", "CSP2")), "Le graphique devrait contenir les codes professionnels attendus.")
})

test_that("plot.commune gère correctement les entrées invalides", {
  # Test 3 : Vérifie que la fonction génère une erreur pour un dataframe avec plusieurs communes
  df_plusieurs_communes <- df_valide
  df_plusieurs_communes$Libellé.de.la.commune[1] <- "Commune 2"
  expect_error(plot.commune(df_plusieurs_communes), "Le data.frame contient plusieurs communes. Veuillez filtrer pour une seule commune.")
})
