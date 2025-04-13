# ui/ui_main.R

navbarPage(
  title = div(
    img(src = "images/janitor.png", height = "90px"),
    span("Janitor", style = "margin-left:10px;")
  ),
  windowTitle = "Janitor - Nettoyage de données",
  collapsible = TRUE,
  theme = bslib::bs_theme(
    version = 5,
    bootswatch = "flatly",
    base_font = font_google("Roboto"),
    heading_font = font_google("Roboto"),
    "nav-tab-font-size" = "1.1rem",
    "nav-link-padding-y" = "1.5rem"
  ),
  header = tags$head(
    tags$style(HTML(".navbar-brand { display: flex; align-items: center; font-weight: 600; font-size: 30px;}
      .container-fluid {font-size: 20px;}
      .well-panel { background: white; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); padding: 20px; margin-bottom: 20px;}
      .btn-action { width: 100%; margin-bottom: 10px; }
      .file-input-container { border: 2px dashed #ddd; border-radius: 8px; padding: 20px; text-align: center; background: #f8f9fa; transition: all 0.3s; }
      .file-input-container:hover { border-color: #3498db; background: #f0f8ff; }
      .data-summary-card { background: white; border-radius: 8px; padding: 15px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); height: 100%; }
      .loader { border: 5px solid #f3f3f3; border-top: 5px solid #3498db; border-radius: 50%; width: 50px; height: 50px; animation: spin 2s linear infinite; margin: 20px auto; }
      @keyframes spin { 0% { transform: rotate(0deg); } 100% { transform: rotate(360deg); } }
      #loading-overlay { position: fixed; top: 0; left: 0; right: 0; bottom: 0; background-color: rgba(0,0,0,0.5); z-index: 9999; display: flex; justify-content: center; align-items: center; }
      #loading-content { background: white; padding: 20px; border-radius: 10px; text-align: center; }
      .tab-content { padding-top: 20px; }"))
  ),
  tabPanel("Importation", icon = icon("upload"), source("ui/ui_import.R")$value),
  tabPanel("Nettoyage Base", icon = icon("broom"),source("ui/ui_clean_base.R")$value),
  tabPanel("Nettoyage Avancé",icon = icon("broom"), source("ui/ui_clean_advanced.R")$value),
  tabPanel("Analyse",icon = icon("chart-bar"), source("ui/ui_analysis.R")$value),
  tabPanel("Export",icon = icon("file-export"),source("ui/ui_export.R")$value),
  tabPanel("Rapport",icon = icon("file-alt"),source("ui/ui_report.R")$value),
  tabPanel("Recherche",icon = icon("search"), source("ui/ui_search.R")$value),
  footer = div(class = "text-center", style = "padding:20px; background-color:#f8f9fa; margin-top:30px;",
               p("Janitor v2.0 - ", a(href = "https://github.com/sfirke/janitor", target = "_blank", "Package janitor")),
               p("Développé avec R")
  )
)
