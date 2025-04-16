# Nettoyage automatique avec Janitor - Application Shiny

[![Shiny](https://img.shields.io/badge/Shiny-2.0+-blue?logo=r&logoColor=white)](https://shiny.rstudio.com/)
[![janitor](https://img.shields.io/badge/janitor-2.2.0-green)](https://github.com/sfirke/janitor)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## Description

Application Shiny interactive pour le nettoyage et la préparation de données utilisant le package R [`janitor`](https://github.com/sfirke/janitor).


## Fonctionnalités clés

### Nettoyage de base
- [`clean_names()`](https://sfirke.github.io/janitor/reference/clean_names.html) : Standardisation des noms de colonnes
- [`remove_empty()`](https://sfirke.github.io/janitor/reference/remove_empty.html) : Suppression des lignes/colonnes vides
- [`remove_constant()`](https://sfirke.github.io/janitor/reference/remove_constant.html) : Élimination des colonnes sans variation
- [`excel_numeric_to_date()`](https://sfirke.github.io/janitor/reference/excel_numeric_to_date.html) : Conversion des dates Excel

### Analyse des doublons
- [`get_dupes()`](https://sfirke.github.io/janitor/reference/get_dupes.html) : Identification des doublons
- [`compare_df_cols()`](https://sfirke.github.io/janitor/reference/compare_df_cols.html) : Comparaison de structure

### Tableaux Croisés
- [`tabyl()`](https://sfirke.github.io/janitor/reference/tabyl.html) : Création de tableaux croisés
- Fonctions [`adorn_*`](https://sfirke.github.io/janitor/reference/adorn_totals.html) : Mise en forme avancée

### Structure des fichiers
- **UI** : Interface organisée en onglets thématiques
- **Serveur** : Gestion réactive des données


### Ressources utiles

Voici quelques ressources pour l'utilisation du package `janitor`:

- [Documentation officielle du package janitor](https://sfirke.github.io/janitor/)
- [Fiche janitor sur RDocumentation](https://rdocumentation.org/packages/janitor/versions/0.3.0)
- [Tutoriels R sur DellaData.fr](https://delladata.fr/)
- [Cours et ressources R de Lise Vaudor (ENS Lyon)](https://perso.ens-lyon.fr/lise.vaudor/)





Ce projet a été réalisé par :

- **Hildegarde EDIMA BIYENDA**  
   [eddiebugb@gmail.com](mailto:eddiebugb@gmail.com)

- **Djerakei MISTALENGAR**  
  [yvesdjerake@gmail.com](mailto:yvesdjerake@gmail.com)

Sous la supervision de **M. Aboubacar HEMA**,  
[Analyste de Recherche à l'IFPRI](https://www.ifpri.org/profile/aboubacar-hema)

