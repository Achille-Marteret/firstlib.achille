library(testthat)
library(dplyr)

test_that("compter_nombre_d_elu compte correctement le nombre d'élus", {
  # Création d'un dataframe valide avec des élus distincts
  df_valide <- data.frame(
    "Code.du.département" = c("01", "01", "02"),
    "Libellé.du.département" = c("Ain", "Ain", "Aisne"),
    "Code.de.la.collectivité.à.statut.particulier" = c("001", "001", "002"),
    "Libellé.de.la.collectivité.à.statut.particulier" = c("Collectivité 1", "Collectivité 1", "Collectivité 2"),
    "Code.de.la.commune" = c("0001", "0001", "0002"),
    "Libellé.de.la.commune" = c("Commune 1", "Commune 1", "Commune 2"),
    "Nom.de.l.élu" = c("Dupont", "Dupont", "Martin"),
    "Prénom.de.l.élu" = c("Jean", "Jean", "Marie"),
    "Code.sexe" = c("M", "M", "F"),
    "Date.de.naissance" = c("01/01/1980", "01/01/1980", "01/01/1990"),
    "Code.de.la.catégorie.socio.professionnelle" = c("CSP1", "CSP1", "CSP2"),
    "Libellé.de.la.catégorie.socio.professionnelle" = c("Catégorie 1", "Catégorie 1", "Catégorie 2"),
    "Date.de.début.du.mandat" = c("01/01/2020", "01/01/2020", "01/01/2021"),
    "Libellé.de.la.fonction" = c("Maire", "Maire", "Adjoint"),
    "Date.de.début.de.la.fonction" = c("01/01/2020", "01/01/2020", "01/01/2021"),
    "Code.nationalité" = c("FR", "FR", "FR"),
    stringsAsFactors = FALSE
  )

  # Test 1 : Vérifie que la fonction compte correctement le nombre d'élus distincts
  expect_equal(compter_nombre_d_elu(df_valide), 2)

  # Création d'un dataframe avec des élus en double
  df_doublons <- data.frame(
    "Code.du.département" = c("01", "01", "01"),
    "Libellé.du.département" = c("Ain", "Ain", "Ain"),
    "Code.de.la.collectivité.à.statut.particulier" = c("001", "001", "001"),
    "Libellé.de.la.collectivité.à.statut.particulier" = c("Collectivité 1", "Collectivité 1", "Collectivité 1"),
    "Code.de.la.commune" = c("0001", "0001", "0001"),
    "Libellé.de.la.commune" = c("Commune 1", "Commune 1", "Commune 1"),
    "Nom.de.l.élu" = c("Dupont", "Dupont", "Dupont"),
    "Prénom.de.l.élu" = c("Jean", "Jean", "Jean"),
    "Code.sexe" = c("M", "M", "M"),
    "Date.de.naissance" = c("01/01/1980", "01/01/1980", "01/01/1980"),
    "Code.de.la.catégorie.socio.professionnelle" = c("CSP1", "CSP1", "CSP1"),
    "Libellé.de.la.catégorie.socio.professionnelle" = c("Catégorie 1", "Catégorie 1", "Catégorie 1"),
    "Date.de.début.du.mandat" = c("01/01/2020", "01/01/2020", "01/01/2020"),
    "Libellé.de.la.fonction" = c("Maire", "Maire", "Maire"),
    "Date.de.début.de.la.fonction" = c("01/01/2020", "01/01/2020", "01/01/2020"),
    "Code.nationalité" = c("FR", "FR", "FR"),
    stringsAsFactors = FALSE
  )

  # Test 2 : Vérifie que la fonction compte correctement le nombre d'élus distincts même avec des doublons
  expect_equal(compter_nombre_d_elu(df_doublons), 1)
})
