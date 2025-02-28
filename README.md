# firstlib.achille

## Outils d'analyse des données des élus conseillers municipaux en France

**Version** : 0.0.0.9000

**Auteur** : Achille Marteret  
**Email** : [marteret.achille@gmail.com](mailto:marteret.achille@gmail.com)

---

## Description

Le package **`firstlib.achille`** fournit une suite d'outils d'analyse statistique pour les données du **Répertoire National des Élus (RNE)**. Il permet d'automatiser le traitement des informations relatives aux élus communaux et départementaux, et offre plusieurs fonctions facilitant l'exploration et la visualisation des données.

### Principales fonctionnalités :
- **Importation et prétraitement des données** des élus à partir du RNE.
- **Calcul du nombre total d’élus et d’adjoints** par commune et département.
- **Identification des élus les plus âgés** et analyse de la distribution des âges.
- **Visualisation des professions des élus** via des graphiques en barres.
- **Méthodes génériques** : `summary.commune()` et `summary.departement()` pour générer des rapports détaillés sur les caractéristiques des élus par commune et département.
- **Visualisations spécifiques aux communes et départements** : Utilisation des fonctions `plot.commune()` et `plot.departement()` pour afficher la répartition des professions des élus.

Ce package permet d'assurer une meilleure compréhension des dynamiques locales en fournissant des indicateurs clés sur la représentation politique au niveau municipal et départemental.

---

## Installation

### Prérequis

Avant d'installer **`firstlib.achille`**, assurez-vous d'avoir installé les logiciels suivants :

- [R](https://cran.r-project.org/) (version >= 3.5)
- [RStudio](https://rstudio.com/)

### Installer le package depuis GitHub

Pour installer le package directement depuis GitHub, vous pouvez utiliser **`devtools`** :

```r
# Installer devtools si ce n'est pas déjà fait
install.packages("devtools")

# Installer firstlib.achille depuis GitHub
devtools::install_github("achille-marteret/firstlib.achille")
```
