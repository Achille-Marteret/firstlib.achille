% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Fonction.R
\name{compter_nombre_d_elu}
\alias{compter_nombre_d_elu}
\title{Compter le nombre d'élus}
\usage{
compter_nombre_d_elu(df)
}
\arguments{
\item{df}{Un dataframe qui valide le schéma contenant les données
des élus conseillers municipaux.}
}
\value{
Un entier représentant le nombre total d'élus dans le dataframe
}
\description{
Cette fonction calcule le nombre total d'élus à partir d'un dataframe
respectant le schéma des données des élus conseillers municipaux.
Le dataframe doit contenir les mêmes colonnes que le fichier
élus-conseillers-municipaux-cm.
}
