# global.R - Chargement global de l'application Janitor

# Installation et chargement des packages nécessaires
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  shiny, shinyjs, shinyWidgets, shinycssloaders, fontawesome, bslib,fst,
  janitor, dplyr, tidyr, readr, readxl, haven, data.table, rio, jsonlite,
  stringr, forcats, ggplot2, plotly, DT, lubridate, rmarkdown, writexl, beepr
)

# Augmenter la limite mémoire pour les uploads volumineux
options(future.globals.maxSize = 16000 * 1024^2)  # 16GB

# Chargement des fonctions auxiliaires
#source("modules/helpers.R")

# Liste des fonctions janitor disponibles
janitor_functions <- sort(ls("package:janitor")[!grepl("^\\.", ls("package:janitor"))])

# Initialisation des valeurs réactives
rv <- reactiveValues(
  data = data.frame(),  # Toujours initialiser avec une structure vide appropriée
  results = list(valid = TRUE),  # Liste avec élément par défaut
  logs = character(0)   # Vecteur vide pour les logs
)

log_action <- function(message) {
  if (!is.null(message) && is.character(message) && nzchar(message)) {
    rv$logs <- c(rv$logs, paste(Sys.time(), "-", message))
  }
}

