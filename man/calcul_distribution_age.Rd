% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Fonction.R
\name{calcul_distribution_age}
\alias{calcul_distribution_age}
\title{Distribution des âges}
\usage{
calcul_distribution_age(df)
}
\arguments{
\item{df}{Un dataframe qui valide le schéma contenant les données
des élus conseillers municipaux.}
}
\value{
Un vecteur nommé contenant les 5 quantiles calculés (0\%, 25\%, 50\%, 75\%, 100\%)
des âges des élus
}
\description{
Cette fonction calcule la distribution des âges des élus en utilisant les
quantiles 0\% (minimum), 25\%, 50\% (médiane), 75\% et 100\% (maximum).
Les âges sont calculés à partir des dates de naissance jusqu'à la date
du jour. Les dates non valides sont exclues du calcul.
}
