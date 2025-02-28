#' Générer un rapport à partir du fichier rapport.qmd
#'
#' Cette fonction génère un rapport HTML basé sur les paramètres fournis pour une commune et un département.
#' Elle utilise Quarto pour compiler le rapport.
#'
#' @param commune Code INSEE de la commune pour laquelle générer le rapport.
#' @param departement Code INSEE du département pour lequel générer le rapport.
#' @param output Chemin du fichier de sortie (par exemple, "rapport_final.html").
#'
#' @details La fonction utilise le fichier \code{rapport.qmd} situé dans le dossier \code{inst} du package.
#' Les paramètres \code{commune} et \code{departement} sont passés au rapport pour configurer les données à afficher.
#'
#' @return Aucune valeur de retour, mais génère un fichier HTML à l'emplacement spécifié par \code{output}.
#'
#' @examples
#' \dontrun{
#' generer_rapport(commune = "44109", departement = "44", output = "rapport_final.html")
#' }
#'
#' @import quarto
#' @export
generer_rapport <- function(commune, departement, output) {
  # Chemin vers le fichier rapport.qmd dans le package
  qmd_file <- system.file("rapport.qmd", package = "firstlib.achille")

  # Définir les paramètres à passer au rapport
  params <- list(
    code_commune = commune,
    code_departement = departement
  )

  # Générer le rapport avec Quarto
  quarto::quarto_render(
    input = qmd_file,
    output_file = output,
    execute_params = params,
    output_format = "html"
  )
}
