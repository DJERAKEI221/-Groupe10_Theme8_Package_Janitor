# modules/helpers.R

# Fonction utilitaire pour créer une liste sans éléments vides
safe_list <- function(...) {
  elements <- list(...)
  elements[lengths(elements) > 0]
}

# Fonction de journalisation avec horodatage
log_action <- function(message) {
  if (!is.null(message) && is.character(message) && nzchar(message)) {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    rv$logs <- c(rv$logs, paste0(timestamp, " - ", message))
  }
}

# Fonctions d'affichage d'un loader (si shinyjs activé)
showLoader <- function() {
  shinyjs::html(id = "loading-content", 
                html = '<div class="loader"></div><p>Chargement en cours...</p>', 
                add = FALSE)
  shinyjs::show(id = "loading-overlay")
}

hideLoader <- function() {
  shinyjs::hide(id = "loading-overlay")
}
