#' @title Résumer des informations des objets de type 'communes'
#' @name summary.commune
#' @description
#' Méthode S3 pour la fonction générique summary() appliquée aux objets de
#' classe 'commune'. Cette méthode produit un résumé des informations principales
#' d'une commune et de ses élus (nom de la commune, nombre d'élus, distribution
#' des âges, informations sur l'élu le plus âgé).
#'
#' Les objets de classe 'commune' sont des dataframes qui respectent le schéma
#' du fichier élus-conseillers-municipaux-cm. Pour utiliser cette méthode,
#' les dataframes doivent être préalablement assignés à la classe 'commune'.
#'
#' @param x Un paramètre x qui sera un objet de classe 'commune'
#' @return Affiche dans la console :
#'   - Le nom de la commune
#'   - Le nombre total d'élus
#'   - La distribution des âges (quantiles)
#'   - Les informations sur l'élu le plus âgé (nom, prénom, âge)
#'
#' @section Erreur :
#' La fonction génère une erreur "L'objet doit être de type 'commune'."
#' si l'objet n'est pas de classe 'commune'
#'
#' @examples
#' # Création d'un exemple de dataframe
#' df_exemple <- data.frame(
#'   "Libellé.de.la.commune" = "Exemple",
#'   "Nom.de.l.élu" = c("Dupont", "Martin"),
#'   "Prénom.de.l.élu" = c("Jean", "Marie"),
#'   "Date.de.naissance" = c("01/01/1950", "01/01/1960")
#' )
#'
#' # Attribution de la classe 'commune'
#' class(df_exemple) <- c("commune", class(df_exemple))
#'
#' # Utilisation de la méthode
#' summary(df_exemple)
#' summary.commune(df_exemple)
#'
#' @import dplyr
#' @import lubridate
#'
#' @export
summary.commune <- function(x) {

  if (!inherits(x, "commune")) {
    stop("L'objet doit être de type 'commune'.")
  }

  elu_plus_age <- trouver_l_elu_le_plus_age(x)

  cat("Nom de la commune :", unique(x$Libellé.de.la.commune), "\n")
  cat("Nombre d'élu.e.s :",  compter_nombre_d_elu(x), "\n")

  cat("Distribution des âges des élu.e.s :", "\n")
  print(calcul_distribution_age(x))

  cat("L'élu.e le ou la plus âgé.e :", "\n")
  cat("Nom :", elu_plus_age$Nom.de.l.élu, "\n")
  cat("Prénom :", elu_plus_age$Prénom.de.l.élu, "\n")
  cat("Âge :", elu_plus_age$Âge, "\n")

}


#' @title Résumer des informations des objets de type 'département'
#' @name summary.departement
#' @description
#' Fonction générique permettant de résumer les informations des objets de type 'département'.
#' Ces objets sont des dataframes conformes au schéma du fichier `élus-conseillers-municipaux-cm`
#' (mêmes noms et nombres de colonnes) et doivent être attribués de la classe 'département'.
#'
#' @param x Un objet de classe 'département'.
#' @return La fonction retourne les informations suivantes :
#' \itemize{
#'   \item Le nom du département.
#'   \item Le nombre de communes dans le département.
#'   \item Le nombre d'élu.e.s dans le département.
#'   \item La distribution des âges des élu.e.s dans le département.
#'   \item Le nom, l'âge et la commune de l'élu.e le ou la plus âgé.e du département.
#'   \item Le nom, l'âge et la commune de l'élu.e le ou la plus jeune du département.
#'   \item La commune ayant la moyenne d’âge la plus basse et la distribution des âges des élu.e.s dans cette commune.
#'   \item La commune ayant la moyenne d’âge la plus élevée et la distribution des âges des élu.e.s dans cette commune.
#' }
#'
#' @details
#' Si l'objet fourni n'est pas de type 'département', la fonction génère un message d'avertissement :
#' `"L'objet doit être de type 'département'."`
#'
#' @examples
#' # Attribution de la classe 'département' au dataframe "mon_dataframe"
#' mon_dataframe <- creer_departement(mon_dataframe)
#'
#' summary(mon_dataframe)
#'
#' @import dplyr
#' @import lubridate
#'
#' @export
summary.departement <- function(x) {

  if (!inherits(x, "departement")) {
    stop("L'objet doit être de type 'departement'.")
  }

  cat("Nom du département :", unique(x$Libellé.du.département), "\n")
  cat("Nombre de communes :", length(unique(x$Libellé.de.la.commune)), "\n")
  cat("Nombre d'élu.e.s :",  compter_nombre_d_elu(x), "\n")
  cat("Distribution des âges des élu.e.s :", "\n")
  print(calcul_distribution_age(x))

  #Trouver l'élu le plus agé
  trouver_elu_le_plus_age <- function(df) {
    validate_schema(df)
    df |>
      mutate(Date.de.naissance = dmy(Date.de.naissance)) |>
      slice(which.min(Date.de.naissance)) |>
      mutate(Âge = as.integer(interval(Date.de.naissance, Sys.Date()) / years(1)))
  }

  elu_max <- trouver_elu_le_plus_age(x)
  cat("L'élu.e le ou la plus âgé.e :", "\n")
  cat("Nom :", elu_max$Nom.de.l.élu, "\n")
  cat("Âge :", elu_max$Âge, "\n")
  cat("Commune :", elu_max$Libellé.de.la.commune, "\n")

  # Trouver l'élu le/la plus jeune
  trouver_l_elu_le_plus_jeune <- function(df) {
    validate_schema(df)
    df |>
      mutate(Date.de.naissance = dmy(Date.de.naissance)) |>
      slice(which.max(Date.de.naissance)) |>
      mutate(Âge = as.integer(interval(Date.de.naissance, Sys.Date()) / years(1)))
  }

  elu_min <- trouver_l_elu_le_plus_jeune(x)
  cat("Élu.e le/la plus jeune :", "\n")
  cat("Nom :", elu_min$Nom.de.l.élu, "\n")
  cat("Âge :", elu_min$Âge, "\n")
  cat("Commune :", elu_min$Libellé.de.la.commune, "\n")

  # Commune avec la moyenne d’âge la plus faible
  x <- x |>
    mutate(Âge = as.integer(interval(dmy(Date.de.naissance), Sys.Date()) / years(1)))

  age_par_commune <- aggregate(Âge ~ Libellé.de.la.commune, data = x, FUN = mean)

  commune_min_age <- age_par_commune[which.min(age_par_commune$Âge), "Libellé.de.la.commune"]
  cat("Commune avec la moyenne d’âge la plus faible :", commune_min_age, "\n")
  print(summary(x$Âge[x$Libellé.de.la.commune == commune_min_age]))

  # Commune avec la moyenne d’âge la plus élevée
  commune_max_age <- age_par_commune[which.max(age_par_commune$Âge), "Libellé.de.la.commune"]
  cat("Commune avec la moyenne d’âge la plus élevée :", commune_max_age, "\n")
  print(summary(x$Âge[x$Libellé.de.la.commune == commune_max_age]))

}
