# app.R - Point d’entrée de l’application Janitor

source("global.R")
ui <- source("ui/ui_main.R")$value
server <- source("server/server_main.R")$value

options(
  shiny.maxRequestSize = 500*1024^2,  # Doit être identique à app.R
  timeout = 600  # Augmenter le timeout à 10 minutes
)

shinyApp(ui, server)
