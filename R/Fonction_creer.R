

#' @title Créer un objet de classe commune
#' @name creer_commune
#' @description
#' Cette fonction prend en entrée un data.frame contenant des données communales
#' et crée un objet de classe "commune". Elle vérifie que toutes les données
#' appartiennent à une seule et même commune.
#'
#' @param df Un data.frame contenant les données d'une commune.
#'           Doit contenir une colonne 'Code.de.la.commune'.
#'
#' @return Un data.frame de classe "commune" contenant les données d'une seule commune.
#'
#' @details
#' La fonction effectue les vérifications suivantes :
#' \itemize{
#'   \item Valide le schéma des données via la fonction validate_schema()
#'   \item Vérifie que toutes les lignes correspondent à une seule commune
#'   \item Ajoute la classe "commune" si elle n'existe pas déjà
#' }
#'
#'Une erreur est levée si le `data.frame` contient des données de plusieurs communes.
#'
#' @examples
#' \dontrun{
#' # Créer un objet commune à partir d'un data.frame
#' data_commune <- data.frame(
#'   Code.de.la.commune = rep("01001", 3),
#'   Nom.de.l.élu = c("Dupont", "Martin", "Durand"),
#'   Prénom.de.l.élu = c("Jean", "Marie", "Pierre"),
#'   etc.
#' )
#' ma_commune <- creer_commune(data_commune)
#' }
#'
#' @import dplyr
#'
#' @export
creer_commune <- function(df) {
  validate_schema(df)

  unique_commune_code <- df$Code.de.la.commune |>
    unique() |>
    length()

  if(unique_commune_code > 1) {
    stop("Le dataframe est composé de plusieurs communes")
  }

  if(!inherits(df, "commune")) {
    class(df) <- c("commune", class(df))
  }

  return(df)
}



#' @title Créer un objet de classe département
#' @name creer_departement
#'
#' @description
#' Cette fonction prend en entrée un data.frame contenant des données départementales
#' et crée un objet de classe "departement". Elle vérifie que toutes les données
#' appartiennent à un seul et même département.
#'
#' @param df Un data.frame contenant les données d'un département.
#'           Doit contenir une colonne 'Code.du.département'.
#'
#' @return Un data.frame de classe "departement" contenant les données d'un seul département.
#'
#' @details
#' La fonction effectue les vérifications suivantes :
#' \itemize{
#'   \item Valide le schéma des données via la fonction validate_schema()
#'   \item Vérifie que toutes les lignes correspondent à un seul département
#'   \item Ajoute la classe "departement" si elle n'existe pas déjà
#' }
#'
#' Une erreur est levée si le `data.frame` contient des données de plusieurs départements.
#'
#' @examples
#' \dontrun{
#' # Créer un objet commune à partir d'un data.frame
#' data_departement <- data.frame(
#'   Code.du.département = rep("01", 3),
#'   Libellé.du.département = rep("Ain", 3),
#'   Code.de.la.commune = c("01001", "01002", "01003"),
#'   Nom.de.l.élu = c("Dupont", "Martin", "Durand"),
#'   Prénom.de.l.élu = c("Jean", "Marie", "Pierre"),
#'   etc.
#' )
#' mon_departement <- creer_departement(data_departement)
#' }
#'
#' @import dplyr
#'
#' @export
creer_departement <- function(df) {
  validate_schema(df)

  unique_dept_code <- df$Code.du.département |>
    unique() |>
    length()

  if(unique_dept_code > 1) {
    stop("Le dataframe est composé de plusieurs départements")
  }

  if(!inherits(df, "departement")) {
    class(df) <- c("departement", class(df))
  }

  return(df)
}
