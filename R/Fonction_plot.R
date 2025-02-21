
#' @title Graphique du nombre d'élus par catégorie socio professionnelle dans une commune
#' @name plot.commune
#' @description
#' Cette fonction génère un diagramme en barres horizontal représentant le nombre d'élus
#' par catégorie socio-professionnelle dans une commune donnée.
#' Le dataframe fourni doit contenir les données d'une seule commune et respecter le schéma
#' du fichier `élus-conseillers-municipaux-cm` (mêmes noms et nombres de colonnes).
#'
#' @param df Un dataframe qui contient une seule commune et qui valide le schéma
#' contenant les données des élus conseillers municipaux.
#'
#' @return La fonction retourne un diagramme en barres horizontal de la catégorie
#' socio professionnelle en fonction du nombre d'élus de la commune.
#'
#' @details
#' Si le dataframe contient plusieurs communes, la fonction génère une erreur :
#' `"Le data.frame contient plusieurs communes. Veuillez filtrer pour une seule commune."`
#'
#' @import dplyr
#' @import ggplot2
#'
#' @export
plot.commune <- function(df) {
  validate_schema(df)

  commune_name <- unique(df$Libellé.de.la.commune)
  departement_name <- unique(df$Libellé.du.département)

  if (length(commune_name) > 1) {
    stop("Le data.frame contient plusieurs communes. Veuillez filtrer pour une seule commune.")
  }

  professions_code <- df |>
    group_by(Code.de.la.catégorie.socio.professionnelle) |>
    summarise(n = n(), .groups = "drop") |>
    arrange(desc(n))

  ggplot(professions_code,
         aes(x = n,
             y = reorder(Code.de.la.catégorie.socio.professionnelle, n))) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(
      title = paste(commune_name, "-", departement_name),
      x = paste("Libellés des codes professionnels pour les élus (", compter_nombre_d_elu(df), " élus)", sep = ""),
      y = "Code Professionnel"
    ) +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 5))
}



#' @title Graphique du nombre d'élus par catégorie socio professionnelle par départements
#' @name plot.departement
#' @description
#' Cette fonction génère un graphique en barres horizontal représentant la distribution
#' des élu.e.s municipaux selon leur catégorie socio-professionnelle pour un département donné.
#' Elle sélectionne automatiquement les 10 catégories les plus représentées en nombre d'élu.e.s.
#'
#' @param df Un data.frame de classe "departement" contenant les données des élus.
#'           Doit contenir les colonnes :
#'           \itemize{
#'             \item Code.du.département
#'             \item Libellé.de.la.commune
#'             \item Code.de.la.catégorie.socio.professionnelle
#'           }
#'
#' @return Un objet ggplot2 représentant un diagramme en barres horizontal avec :
#'         \itemize{
#'           \item En ordonnée : les codes des catégories socio-professionnelles
#'           \item En abscisse : le nombre d'élus
#'           \item Un titre incluant le nom du département et le nombre de communes
#'           \item Les barres triées par ordre décroissant du nombre d'élus
#'           \item Les 10 codes professionnels les plus importants en nombre d’élu.e.s du département
#'         }
#'
#' @details
#' La fonction réalise les étapes suivantes :
#' \enumerate{
#'   \item Vérifie la validité du schéma des données
#'   \item Extrait le nom du département et compte le nombre de communes uniques
#'   \item Vérifie qu'il n'y a qu'un seul département dans les données
#'   \item Compte les occurrences de chaque code professionnel
#'   \item Sélectionne les 10 codes les plus fréquents
#'   \item Crée un graphique avec ggplot2 utilisant :
#'         - Une couleur steelblue pour les barres
#'         - Un thème minimal
#'         - Une taille de texte réduite (5) pour les codes sur l'axe Y
#' }
#'
#' @section Erreurs:
#' La fonction s'arrête avec une erreur si :
#' \itemize{
#'   \item Le data.frame contient des données de plusieurs départements
#'   \item Le schéma des données n'est pas valide
#' }
#'
#' @import dplyr
#' @import ggplot2
#'
#' @export
plot.departement <- function(df) {
  validate_schema(df)

  departement_name <- unique(df$Libellé.du.département)
  nombre_communes <- length(unique(df$Libellé.de.la.commune))

  if (length(departement_name) > 1) {
    stop("Le data.frame contient plusieurs départements. Veuillez filtrer pour un seul département.")
  }

  professions_code <- df |>
    group_by(Code.de.la.catégorie.socio.professionnelle) |>
    summarise(n = n(), .groups = "drop") |>
    arrange(desc(n)) |>
    slice_max(n, n = 10)

  ggplot(professions_code,
         aes(x = n,
             y = reorder(Code.de.la.catégorie.socio.professionnelle, n))) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(
      title = paste(departement_name, "-", nombre_communes, "communes"),
      x = paste("Libellés des 10 codes professionnels les plus représentés pour le département", departement_name),
      y = "Code Professionnel"
    ) +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 5))
}
