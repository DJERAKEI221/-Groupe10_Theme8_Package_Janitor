# app.R - Application complète de nettoyage de données avec Janitor

# 1. Installation des packages (si nécessaire) ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  shiny, shinyjs, janitor, DT, ggplot2, plotly, beepr, tinytex, knitr,rmarkdown,
  readxl, dplyr, tidyr, forcats, haven, jsonlite, data.table, rio,
  shinyWidgets, shinycssloaders, fontawesome, writexl, readr, bslib, lubridate, stringr
)

# Dans app.R, en début de fichier (avant shinyApp())
options(future.globals.maxSize = 16000 * 1024^2)  # 16GB en bytes
gc()  # Nettoyage mémoire

# 2. Interface utilisateur (UI) ----
ui <- navbarPage(
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
    tags$style(HTML("
      .navbar-brand { display: flex; align-items: center; font-weight: 600; font-size: 30px;}
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
      .tab-content { padding-top: 20px; }
    "))
  ),
  
  # Onglet Importation
  tabPanel("Importation", icon = icon("upload"),
           div(class = "container-fluid",
               div(class = "row",
                   column(4,
                          div(class = "well-panel",
                              h4(icon("file-import"), "Importer des données"),
                              fileInput("file_input", NULL, 
                                        accept = c(".csv",".xlsx",".xls",".tsv",".txt",".sav",".dta",".sas7bdat",".json",".rds")
                              ),
                              hr(),
                              h4(icon("cog"), "Options d'import"),
                              materialSwitch("header", "Première ligne comme en-tête", TRUE, status = "primary"),
                              conditionalPanel(
                                condition = "input.file_input && ['csv','tsv','txt'].includes(input.file_input.name.split('.').pop().toLowerCase())",
                                selectInput("sep", "Séparateur:", c("Virgule" = ",", "Point-virgule" = ";", "Tabulation" = "\t", "Espace" = " ")),
                                selectInput("dec", "Séparateur décimal:", c("Point" = ".", "Virgule" = ","))
                              ),
                              actionButton("reset_data", "Réinitialiser tout", icon = icon("trash"), 
                                           class = "btn btn-danger btn-action"
                              )
                          ),
                          div(class = "well-panel",
                              h4(icon("info-circle"), "Résumé des données"),
                              verbatimTextOutput("data_summary"),
                              hr(),
                              h4(icon("database"), "Métadonnées"),
                              verbatimTextOutput("data_types")
                          )
                   ),
                   column(8,
                          div(class = "well-panel",
                              h4(icon("table"), "Aperçu des données"),
                              withSpinner(DTOutput("raw_table"), type = 6, color = "#3498db")
                          )
                   )
               )
           )
  ),
  
  # Onglet Nettoyage de base
  tabPanel("Nettoyage Base", icon = icon("broom"),
           div(class = "container-fluid",
               div(class = "row",
                   column(4,
                          div(class = "well-panel",
                              h4(icon("magic"), "Nettoyage automatique"),
                              actionButton("clean_names", "Nettoyer les noms", icon = icon("pencil-alt"), 
                                           class = "btn btn-primary btn-action"
                              ),
                              actionButton("remove_empty", "Supprimer les vides", icon = icon("trash-alt"), 
                                           class = "btn btn-primary btn-action"
                              ),
                              actionButton("remove_constant", "Supprimer constantes", icon = icon("filter"), 
                                           class = "btn btn-primary btn-action"
                              ),
                              actionButton("clean_factors", "Nettoyer les facteurs", icon = icon("tags"), 
                                           class = "btn btn-primary btn-action"
                              )
                          ),
                          div(class = "well-panel",
                              h4(icon("calendar-alt"), "Conversion de dates"),
                              numericInput("excel_date", "Valeur Excel:", 44197),
                              actionButton("convert_date", "Convertir en date", icon = icon("exchange-alt"), 
                                           class = "btn btn-warning btn-action"
                              ),
                              verbatimTextOutput("excel_date_result")
                          )
                   ),
                   column(8,
                          div(class = "well-panel",
                              h4(icon("clone"), "Gestion des doublons"),
                              selectizeInput("dupe_cols", "Colonnes à vérifier:", choices = NULL, multiple = TRUE),
                              actionButton("find_dupes", "Identifier les doublons", icon = icon("search"), 
                                           class = "btn btn-warning"
                              ),
                              hr(),
                              withSpinner(DTOutput("dupes_table"), type = 6, color = "#3498db")
                          )
                   )
               )
           )
  ),
  
  # Onglet Nettoyage Avancé
  tabPanel("Nettoyage Avancé", icon = icon("broom"),
           div(class = "container-fluid",
               div(class = "row",
                   column(4,
                          div(class = "well-panel",
                              h4(icon("calculator"), "Manipulation numérique"),
                              numericInput("round_digits", "Décimales:", 2, min = 0, max = 10),
                              actionButton("round_numeric", "Arrondir les nombres", icon = icon("calculator"), 
                                           class = "btn btn-info btn-action"
                              ),
                              hr(),
                              h4(icon("question-circle"), "Gestion des NA"),
                              textInput("na_strings", "Valeurs à convertir en NA:", "NA, N/A, null, missing"),
                              actionButton("convert_to_na", "Convertir en NA", icon = icon("ban"), 
                                           class = "btn btn-warning btn-action"
                              ),
                              actionButton("remove_na_cols", "Supprimer colonnes avec NA", icon = icon("trash"), 
                                           class = "btn btn-danger btn-action"
                              )
                          )
                   ),
                   column(4,
                          div(class = "well-panel",
                              h4(icon("columns"), "Gestion des colonnes"),
                              actionButton("remove_single_value", "Supprimer colonnes à valeur unique", 
                                           icon = icon("filter"), class = "btn btn-warning btn-action"
                              ),
                              actionButton("clean_datetime", "Nettoyer les dates/heures", 
                                           icon = icon("clock"), class = "btn btn-info btn-action"
                              ),
                              selectInput("datetime_col", "Colonne datetime:", choices = NULL),
                              hr(),
                              h4(icon("table"), "Formatage"),
                              actionButton("clean_case", "Uniformiser la casse", icon = icon("font"), 
                                           class = "btn btn-primary btn-action"
                              )
                          )
                   ),
                   column(4,
                          div(class = "well-panel",
                              h4(icon("not-equal"), "Comparaison"),
                              actionButton("compare_df", "Comparer avec données brutes", 
                                           icon = icon("code-compare"), class = "btn btn-info btn-action"
                              ),
                              hr(),
                              h4(icon("file-excel"), "Fonctions Excel"),
                              actionButton("excel_numeric", "Convertir dates Excel", 
                                           icon = icon("calendar"), class = "btn btn-success btn-action"
                              ),
                              numericInput("excel_col", "Colonne numérique:", value = 1, min = 1)
                          )
                   )
               )
           )
  ),
  
  # Onglet Analyse
  tabPanel("Analyse", icon = icon("chart-bar"),
           div(class = "container-fluid",
               div(class = "row",
                   column(4,
                          div(class = "well-panel",
                              h4(icon("table"), "Tableaux croisés"),
                              selectizeInput("tabyl_vars", "Variables pour tabyl:", 
                                             choices = NULL, multiple = TRUE,
                                             options = list(maxItems = 2)),
                              actionButton("create_tabyl", "Générer le tableau", icon = icon("calculator"), 
                                           class = "btn btn-success btn-action"
                              ),
                              hr(),
                              awesomeCheckboxGroup("adorn_funcs", "Options de formatage:", 
                                                   c("Ajouter totaux" = "totals", 
                                                     "Calculer pourcentages" = "percent", 
                                                     "Formater pourcentages" = "pct_format", 
                                                     "Afficher effectifs" = "ns")
                              ),
                              selectInput("percent_axis", "Axe pour les %:", 
                                          choices = c("ligne" = "row", "colonne" = "col"))
                          )
                   ),
                   column(8,
                          div(class = "well-panel",
                              h4(icon("chart-line"), "Visualisation"),
                              selectInput("plot_var", "Variable à visualiser:", choices = NULL),
                              withSpinner(plotlyOutput("data_plot", height = "500px"), type = 6, color = "#3498db"),
                              hr(),
                              h4(icon("table"), "Résultats"),
                              withSpinner(DTOutput("tabyl_table"), type = 6, color = "#3498db")
                          )
                   )
               )
           )
  ),
  
  # Onglet Export
  tabPanel("Export", icon = icon("file-export"),
           div(class = "container-fluid",
               div(class = "row",
                   column(6,
                          div(class = "well-panel",
                              h4(icon("download"), "Exporter les données"),
                              selectInput("export_format", "Format:", 
                                          c("CSV" = "csv", "Excel" = "xlsx", "RDS" = "rds", 
                                            "SPSS" = "sav", "JSON" = "json")
                              ),
                              textInput("export_filename", "Nom du fichier:", "donnees_nettoyees"),
                              downloadButton("download_data", "Télécharger", class = "btn btn-success btn-action")
                          )
                   ),
                   column(6,
                          div(class = "well-panel",
                              h4(icon("check-circle"), "Données nettoyées"),
                              withSpinner(DTOutput("clean_table"), type = 6, color = "#3498db"),
                              hr(),
                              h4(icon("code-compare"), "Comparaison"),
                              withSpinner(DTOutput("comparison_table"), type = 6, color = "#3498db")
                          )
                   )
               )
           )
  ), 
  
  # Onglet Rapport
  # Onglet Rapport HTML
  tabPanel("Rapport", icon = icon("file-alt"),
           div(class = "container-fluid",
               div(class = "row",
                   column(6,
                          div(class = "well-panel",
                              h4(icon("file-alt"), "Générer un rapport HTML"),
                              textInput("report_title", "Titre du rapport", "Rapport de nettoyage"),
                              textAreaInput("report_notes", "Notes additionnelles", "", rows = 4),
                              downloadButton("download_report", "Télécharger le rapport")                          )
                   )
               )
           )
  ),
  
  # Footer
  footer = div(class = "text-center", style = "padding:20px; background-color:#f8f9fa; margin-top:30px;",
               p("Janitor v2.0 - ", 
                 a(href="https://github.com/sfirke/janitor", target="_blank", "Package janitor")
               ),
               p("Développé avec en utilisant R, Shiny et Janitor")
  )
)

# 3. Serveur ----
server <- function(input, output, session) {
  
  # Fonction helper pour créer des listes safe
  safe_list <- function(...) {
    elements <- list(...)
    elements[lengths(elements) > 0]  # Ne garde que les éléments non-vides
  }
  
  observeEvent(input$compare_df, {
    req(rv$raw_data, rv$clean_data)
    tryCatch({
      rv$comparison_data <- safe_list(
        comparison = janitor::compare_df_cols(rv$raw_data, rv$clean_data),
        stats = if(nrow(rv$raw_data) > 0) list(
          rows_raw = nrow(rv$raw_data),
          rows_clean = nrow(rv$clean_data)
        )
      )
      showNotification("Comparaison effectuée", type = "message")
    }, error = function(e) {
      showNotification(paste("Erreur:", e$message), type = "error")
    })
  })
  
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
  
  observeEvent(input$process_data, {
    req(input$file_input)  # Vérifie que l'input existe
    
    tryCatch({
      # Lecture sécurisée des données
      raw_data <- read.csv(input$file_input$datapath)
      
      # Vérification du contenu
      if(nrow(raw_data) > 0) {
        # Création de liste avec vérification
        processed <- list(
          metadata = list(
            rows = nrow(raw_data),
            cols = ncol(raw_data)
          ),
          # Élément conditionnel
          samples = if(nrow(raw_data) > 5) head(raw_data, 5) else NULL
        )
        
        # Filtrage des éléments NULL avant assignation
        rv$results <- processed[!sapply(processed, is.null)]
        
        # Journalisation
        rv$logs <- c(rv$logs, paste("Données traitées :", Sys.time()))
      }
    }, error = function(e) {
      showNotification(paste("Erreur :", e$message), type = "error")
    })
  })
  
  
  # Initialisation robuste des reactiveValues
  rv <- reactiveValues(
    data = list(placeholder = TRUE),  # Liste initialisée avec un élément
    logs = character(0)               # Vecteur vide pour les logs
  )
  
  observeEvent(input$process, {
    # Créez toujours des listes complètes
    safe_list <- list(
      part1 = if(exists("input$value1")) input$value1 else "default1",
      part2 = if(!is.null(input$value2)) input$value2 else "default2"
    )
    
    # Filtrage des éléments vides
    rv$data <- safe_list[lengths(safe_list) > 0]
  })
  # Fonctions utilitaires
  showLoader <- function() {
    shinyjs::html(id = "loading-content", 
                  html = '<div class="loader"></div><p>Chargement en cours...</p>', 
                  add = FALSE
    )
    shinyjs::show(id = "loading-overlay")
  }
  
  hideLoader <- function() {
    shinyjs::hide(id = "loading-overlay")
  }
  
  # Observers et reactive expressions
  observeEvent(input$file_input, {
    req(input$file_input)
    
    tryCatch({
      showLoader()
      
      ext <- tolower(tools::file_ext(input$file_input$name))
      path <- input$file_input$datapath
      
      rv$raw_data <- switch(ext,
                            csv = if(input$sep == "\t") {
                              data.table::fread(path, header = input$header)
                            } else {
                              readr::read_delim(path, delim = input$sep, locale = readr::locale(decimal_mark = input$dec))
                            },
                            xlsx = readxl::read_excel(path),
                            xls = readxl::read_excel(path),
                            tsv = readr::read_tsv(path, col_names = input$header),
                            txt = readr::read_delim(path, delim = input$sep, locale = readr::locale(decimal_mark = input$dec)),
                            sav = {
                              df <- haven::read_sav(path)
                              haven::as_factor(df) %>% as.data.frame()
                              df <- foreign::read.spss(path, to.data.frame = TRUE, use.value.labels = TRUE) 
                              
                            },
                            dta = {
                              df <- haven::read_dta(path)
                              haven::as_factor(df) %>% as.data.frame()
                            },
                            sas7bdat = {
                              df <- haven::read_sas(path)
                              haven::as_factor(df) %>% as.data.frame()
                            },
                            json = {
                              json_data <- jsonlite::fromJSON(path, flatten = TRUE)
                              if(!is.data.frame(json_data)) as.data.frame(json_data) else json_data
                            },
                            rds = readRDS(path),
                            {
                              df <- rio::import(path)
                              if(!is.data.frame(df)) as.data.frame(df) else df
                            }
      )
      
      # Conversion en dataframe standard si ce n'est pas déjà le cas
      if(!is.data.frame(rv$raw_data)) {
        rv$raw_data <- as.data.frame(rv$raw_data)
      }
      
      # Conversion des facteurs haven en facteurs R standard
      rv$raw_data <- rv$raw_data %>%
        mutate(across(where(~inherits(., "haven_labelled")), 
                      as_factor))
      
      rv$clean_data <- rv$raw_data
      
      # Mise à jour des sélecteurs
      updateSelectInput(session, "plot_var", choices = names(rv$clean_data))
      updateSelectInput(session, "datetime_col", choices = names(rv$clean_data))
      updateSelectizeInput(session, "dupe_cols", choices = names(rv$clean_data))
      updateSelectizeInput(session, "tabyl_vars", choices = names(rv$clean_data))
      
      beepr::beep(10)
    }, error = function(e) {
      showNotification(paste("Erreur lors du chargement:", e$message), type = "error", duration = 10)
      rv$raw_data <- NULL
      rv$clean_data <- NULL
    }, finally = {
      hideLoader()
    })
  })
  
  # Nettoyage des noms de colonnes
  observeEvent(input$clean_names, {
    req(rv$clean_data)
    tryCatch({
      rv$clean_data <- rv$clean_data %>% janitor::clean_names()
      showNotification("Noms de colonnes nettoyés avec succès", type = "message")
      beepr::beep(10)
      
      # Mise à jour des sélecteurs
      updateSelectInput(session, "plot_var", choices = names(rv$clean_data))
      updateSelectInput(session, "datetime_col", choices = names(rv$clean_data))
      updateSelectizeInput(session, "dupe_cols", choices = names(rv$clean_data))
      updateSelectizeInput(session, "tabyl_vars", choices = names(rv$clean_data))
    }, error = function(e) {
      showNotification(paste("Erreur:", e$message), type = "error")
    })
  })
  
  # Suppression des lignes/colonnes vides
  observeEvent(input$remove_empty, {
    req(rv$clean_data)
    tryCatch({
      rv$clean_data <- rv$clean_data %>% janitor::remove_empty(c("rows", "cols"))
      showNotification("Lignes et colonnes vides supprimées", type = "message")
      beepr::beep(10)
      
      # Mise à jour des sélecteurs
      updateSelectInput(session, "plot_var", choices = names(rv$clean_data))
      updateSelectInput(session, "datetime_col", choices = names(rv$clean_data))
      updateSelectizeInput(session, "dupe_cols", choices = names(rv$clean_data))
      updateSelectizeInput(session, "tabyl_vars", choices = names(rv$clean_data))
    }, error = function(e) {
      showNotification(paste("Erreur:", e$message), type = "error")
    })
  })
  
  # Suppression des colonnes constantes
  observeEvent(input$remove_constant, {
    req(rv$clean_data)
    tryCatch({
      rv$clean_data <- rv$clean_data %>% janitor::remove_constant()
      showNotification("Colonnes constantes supprimées", type = "message")
      beepr::beep(10)
      
      # Mise à jour des sélecteurs
      updateSelectInput(session, "plot_var", choices = names(rv$clean_data))
      updateSelectInput(session, "datetime_col", choices = names(rv$clean_data))
      updateSelectizeInput(session, "dupe_cols", choices = names(rv$clean_data))
      updateSelectizeInput(session, "tabyl_vars", choices = names(rv$clean_data))
    }, error = function(e) {
      showNotification(paste("Erreur:", e$message), type = "error")
    })
  })
  
  # Nettoyage des facteurs
  observeEvent(input$clean_factors, {
    req(rv$clean_data)
    tryCatch({
      rv$clean_data <- rv$clean_data %>%
        mutate(across(where(is.factor), janitor::make_clean_names))
      showNotification("Facteurs nettoyés avec succès", type = "message")
      beepr::beep(10)
    }, error = function(e) {
      showNotification(paste("Erreur:", e$message), type = "error")
    })
  })
  
  # Arrondi des nombres
  observeEvent(input$round_numeric, {
    req(rv$clean_data)
    tryCatch({
      digits <- input$round_digits
      rv$clean_data <- rv$clean_data %>%
        mutate(across(where(is.numeric), ~round(., digits)))
      showNotification(paste("Nombres arrondis à", digits, "décimales"), type = "message")
      beepr::beep(10)
    }, error = function(e) {
      showNotification(paste("Erreur:", e$message), type = "error")
    })
  })
  
  # Conversion en NA
  observeEvent(input$convert_to_na, {
    req(rv$clean_data)
    tryCatch({
      na_strings <- strsplit(input$na_strings, ",\\s*")[[1]] %>% 
        trimws() %>% 
        .[. != ""]
      
      rv$clean_data <- rv$clean_data %>%
        mutate(across(everything(), ~replace(., . %in% na_strings, NA)))
      
      showNotification("Valeurs converties en NA", type = "message")
      beepr::beep(10)
    }, error = function(e) {
      showNotification(paste("Erreur:", e$message), type = "error")
    })
  })    
  
  # Suppression colonnes avec NA
  observeEvent(input$remove_na_cols, {
    req(rv$clean_data)
    tryCatch({
      rv$clean_data <- rv$clean_data %>% janitor::remove_empty("cols")
      showNotification("Colonnes avec NA supprimées", type = "message")
      beepr::beep(10)
      
      # Mise à jour des sélecteurs
      updateSelectInput(session, "plot_var", choices = names(rv$clean_data))
      updateSelectInput(session, "datetime_col", choices = names(rv$clean_data))
      updateSelectizeInput(session, "dupe_cols", choices = names(rv$clean_data))
      updateSelectizeInput(session, "tabyl_vars", choices = names(rv$clean_data))
    }, error = function(e) {
      showNotification(paste("Erreur:", e$message), type = "error")
    })
  })
  
  # Suppression colonnes à valeur unique
  observeEvent(input$remove_single_value, {
    req(rv$clean_data)
    tryCatch({
      rv$clean_data <- rv$clean_data %>% janitor::remove_constant()
      showNotification("Colonnes à valeur unique supprimées", type = "message")
      beepr::beep(10)
      
      # Mise à jour des sélecteurs
      updateSelectInput(session, "plot_var", choices = names(rv$clean_data))
      updateSelectInput(session, "datetime_col", choices = names(rv$clean_data))
      updateSelectizeInput(session, "dupe_cols", choices = names(rv$clean_data))
      updateSelectizeInput(session, "tabyl_vars", choices = names(rv$clean_data))
    }, error = function(e) {
      showNotification(paste("Erreur:", e$message), type = "error")
    })
  })
  
  # Nettoyage datetime
 
  observeEvent(input$clean_datetime, {
    req(rv$clean_data, input$datetime_col)
    tryCatch({
      col_name <- input$datetime_col
      date_data <- rv$clean_data[[col_name]]
      
      # Essai de plusieurs formats de date
      parsed <- parse_date_time(date_data, 
                                orders = c("ymd HMS", "dmy HMS", "mdy HMS", "ymd", "dmy", "mdy"))
      
      if(any(is.na(parsed))) {
        showNotification("Certaines dates n'ont pas pu être converties", type = "warning")
      }
      
      rv$clean_data[[col_name]] <- parsed
      showNotification("Conversion de date effectuée", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Erreur conversion date:", e$message), type = "error")
    })
  })
  
  # Uniformisation de la casse
  observeEvent(input$clean_case, {
    req(rv$clean_data)
    tryCatch({
      rv$clean_data <- rv$clean_data %>%
        mutate(across(where(is.character), ~stringr::str_to_lower(.)))
      showNotification("Casse uniformisée", type = "message")
      beepr::beep(10)
    }, error = function(e) {
      showNotification(paste("Erreur:", e$message), type = "error")
    })
  })
  
  # Conversion des dates Excel
  observeEvent(input$convert_date, {
    tryCatch({
      excel_date <- input$excel_date
      converted_date <- janitor::excel_numeric_to_date(excel_date)
      output$excel_date_result <- renderText({
        paste("Date convertie:", as.character(converted_date))
      })
      beepr::beep(10)
    }, error = function(e) {
      showNotification(paste("Erreur:", e$message), type = "error")
    })
  })
  
  # Conversion colonne Excel en dates
  observeEvent(input$excel_numeric, {
    req(rv$clean_data, input$datetime_col)
    tryCatch({
      col_name <- input$datetime_col
      
      numeric_values <- suppressWarnings(as.numeric(rv$clean_data[[col_name]]))
      
      if(all(is.na(numeric_values))) {
        stop("La colonne sélectionnée ne contient pas de valeurs numériques valides")
      }
      
      rv$clean_data <- rv$clean_data %>%
        mutate(!!sym(col_name) := janitor::excel_numeric_to_date(numeric_values))
      
      showNotification(
        paste("Colonne", col_name, "convertie en dates avec succès"), 
        type = "message"
      )
      beepr::beep(10)
    }, error = function(e) {
      showNotification(paste("Erreur:", e$message), type = "error")
    })
  })
  
  # Identification des doublons
  observeEvent(input$find_dupes, {
    req(rv$clean_data, input$dupe_cols)
    tryCatch({
      if(length(input$dupe_cols) > 0) {
        dupes <- rv$clean_data %>% 
          janitor::get_dupes(!!!syms(input$dupe_cols))
        
        if(nrow(dupes) > 0) {
          rv$dupes_data <- dupes %>% arrange(desc(dupe_count))
        } else {
          showNotification("Aucun doublon trouvé pour les colonnes sélectionnées", type = "message")
          rv$dupes_data <- NULL
        }
      }
    }, error = function(e) {
      showNotification(paste("Erreur recherche doublons:", e$message), type = "error")
    })
  })
  
  # Comparaison de dataframes
  observeEvent(input$compare_df, {
    req(rv$raw_data, rv$clean_data)
    tryCatch({
      rv$comparison_data <- janitor::compare_df_cols(rv$raw_data, rv$clean_data)
      showNotification("Comparaison effectuée", type = "message")
      beepr::beep(10)
    }, error = function(e) {
      showNotification(paste("Erreur:", e$message), type = "error")
    })
  })
  
  # Création de tableaux croisés
  observeEvent(input$create_tabyl, {
    req(rv$clean_data, input$tabyl_vars)
    tryCatch({
      # Vérifier qu'on a au moins une variable sélectionnée
      if(length(input$tabyl_vars) == 0) {
        stop("Veuillez sélectionner au moins une variable")
      }
      
      # Création du tabyl avec gestion dynamique des variables
      if(length(input$tabyl_vars) == 1) {
        # Cas d'une seule variable (fréquences simples)
        rv$tabyl_data <- rv$clean_data %>%
          janitor::tabyl(!!sym(input$tabyl_vars[1]))
      } else {
        # Cas de deux variables (tableau croisé)
        rv$tabyl_data <- rv$clean_data %>%
          janitor::tabyl(!!sym(input$tabyl_vars[1]), !!sym(input$tabyl_vars[2]))
      }
      
      # Application des options de formatage
      if("totals" %in% input$adorn_funcs) {
        rv$tabyl_data <- rv$tabyl_data %>% 
          janitor::adorn_totals()
      }
      if("percent" %in% input$adorn_funcs) {
        rv$tabyl_data <- rv$tabyl_data %>% 
          janitor::adorn_percentages(input$percent_axis)
      }
      if("pct_format" %in% input$adorn_funcs) {
        rv$tabyl_data <- rv$tabyl_data %>% 
          janitor::adorn_pct_formatting()
      }
      if("ns" %in% input$adorn_funcs) {
        rv$tabyl_data <- rv$tabyl_data %>% 
          janitor::adorn_ns()
      }
      
      beepr::beep(10)
      showNotification("Tableau croisé généré avec succès", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Erreur:", e$message), type = "error")
      beepr::beep(10)  # Son d'erreur
    })
  })
  
  # Définitions des sorties
  output$raw_table <- renderDT({
    req(rv$raw_data)
    datatable(rv$raw_data, 
              options = list(
                scrollX = TRUE, 
                pageLength = 5,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel')
              ),
              extensions = 'Buttons',
              rownames = FALSE
    )
  })
  
  output$clean_table <- renderDT({
    req(rv$clean_data)
    datatable(rv$clean_data, 
              options = list(
                scrollX = TRUE, 
                pageLength = 5,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel')
              ),
              extensions = 'Buttons',
              rownames = FALSE
    )
  })
  
  output$dupes_table <- renderDT({
    req(rv$dupes_data)
    datatable(rv$dupes_data, 
              options = list(
                scrollX = TRUE, 
                pageLength = 5,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel')
              ),
              extensions = 'Buttons',
              rownames = FALSE
    ) %>% 
      formatStyle("dupe_count", backgroundColor = styleInterval(c(1, 2), c("white", "#fff3cd", "#f8d7da")))
  })
  
  output$tabyl_table <- renderDT({
    req(rv$tabyl_data)
    datatable(rv$tabyl_data, 
              options = list(
                scrollX = TRUE, 
                pageLength = 5,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel')
              ),
              extensions = 'Buttons',
              rownames = FALSE
    ) %>%
      formatPercentage(columns = which(sapply(rv$tabyl_data, is.numeric)), 1)
  })
  
  output$comparison_table <- renderDT({
    req(rv$comparison_data)
    datatable(rv$comparison_data, 
              options = list(
                scrollX = TRUE, 
                pageLength = 5,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel')
              ),
              extensions = 'Buttons',
              rownames = FALSE
    )
  })
  
  output$data_summary <- renderPrint({
    req(rv$raw_data)
    cat("Dimensions :", dim(rv$raw_data)[1], "lignes x", dim(rv$raw_data)[2], "colonnes\n")
    cat("\nStructure :\n")
    str(rv$raw_data)
  })
  
  output$data_types <- renderPrint({
    req(rv$raw_data)
    cat("\nTypes de données :\n")
    sapply(rv$raw_data, class) %>% print()
  })
  
  output$data_plot <- renderPlotly({
    req(rv$clean_data, input$plot_var)
    
    if(is.numeric(rv$clean_data[[input$plot_var]])) {
      # Histogramme pour les variables numériques
      p <- ggplot(rv$clean_data, aes(x = !!sym(input$plot_var))) +
        geom_histogram(fill = "#3498db", bins = 30, alpha = 0.8) +
        labs(title = paste("Distribution de", input$plot_var), 
             x = input$plot_var, y = "Fréquence") +
        theme_minimal()
    } else {
      # Diagramme en barres pour les variables catégorielles
      plot_data <- rv$clean_data %>% 
        count(!!sym(input$plot_var)) %>% 
        mutate(!!sym(input$plot_var) := fct_reorder(!!sym(input$plot_var), n))
      
      p <- ggplot(plot_data, aes(x = !!sym(input$plot_var), y = n)) +
        geom_col(fill = "#3498db", alpha = 0.8) +
        labs(title = paste("Distribution de", input$plot_var), 
             x = input$plot_var, y = "Fréquence") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    
    ggplotly(p) %>% 
      config(displayModeBar = FALSE) %>%
      layout(title = list(text = paste0("Distribution de ", input$plot_var,
                                        "<br><sup>", nrow(rv$clean_data), " observations</sup>")))
  })
  
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("rapport-", Sys.Date(), ".html")
    },
    content = function(file) {
      # Préparation sécurisée des paramètres
      params <- safe_list(
        logs = rv$logs,
        date = Sys.Date(),
        data_sample = if(!is.null(rv$clean_data)) head(rv$clean_data, 10)
      )
      
      rmarkdown::render(
        input = "rapport.Rmd",
        output_file = file,
        params = params,
        envir = new.env()
      )
    }
  )
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste(input$export_filename, 
            switch(input$export_format,
                   "csv" = ".csv",
                   "xlsx" = ".xlsx",
                   "rds" = ".rds",
                   "sav" = ".sav",
                   "json" = ".json"),
            sep = "")
    },
    content = function(file) {
      req(rv$clean_data)
      
      tryCatch({
        switch(input$export_format,
               "csv" = write.csv(rv$clean_data, file, row.names = FALSE),
               "xlsx" = writexl::write_xlsx(list("Data" = rv$clean_data), file),
               "rds" = saveRDS(rv$clean_data, file),
               "sav" = haven::write_sav(rv$clean_data, file),
               "json" = jsonlite::write_json(rv$clean_data, file)
        )
        showNotification("Export réussi!", type = "message")
      }, error = function(e) {
        showNotification(paste("Erreur :", e$message), type = "error")
      })
    }
  )
  
  # Réinitialisation
  observeEvent(input$reset_data, {
    rv$raw_data <- NULL
    rv$clean_data <- NULL
    rv$tabyl_data <- NULL
    rv$dupes_data <- NULL
    rv$comparison_data <- NULL
    reset("file_input")
    updateSelectInput(session, "plot_var", choices = character(0))
    updateSelectInput(session, "datetime_col", choices = character(0))
    updateSelectizeInput(session, "dupe_cols", choices = character(0))
    updateSelectizeInput(session, "tabyl_vars", choices = character(0))
    beepr::beep(10)
  })
  
  # Nettoyage des noms
  # Nettoyage des noms
  observeEvent(input$clean_names, {
    req(rv$clean_data)
    tryCatch({
      rv$clean_data <- janitor::clean_names(rv$clean_data)
      log_action <- function(message) {
        if (!is.null(message) && is.character(message) && nzchar(message)) {
          timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
          rv$logs <- c(rv$logs, paste0(timestamp, " - ", message))
        }
      }
      showNotification("✔ Noms nettoyés", type = "message")
      beepr::beep(10)
    }, error = function(e) {
      showNotification(paste("Erreur :", e$message), type = "error")
    })
  })
  
  # Suppression des lignes/colonnes vides
  observeEvent(input$remove_empty, {
    req(rv$clean_data)
    tryCatch({
      rv$clean_data <- janitor::remove_empty(rv$clean_data, c("rows", "cols"))
      log_action("🗑️ Lignes et colonnes vides supprimées (janitor::remove_empty)")
      showNotification("✔ Suppression des vides effectuée", type = "message")
      beepr::beep(10)
    }, error = function(e) {
      showNotification(paste("Erreur :", e$message), type = "error")
    })
  })
  
  # Colonnes constantes
  observeEvent(input$remove_constant, {
    req(rv$clean_data)
    tryCatch({
      rv$clean_data <- janitor::remove_constant(rv$clean_data)
      log_action("🔁 Colonnes constantes supprimées (janitor::remove_constant)")
      showNotification("✔ Suppression des constantes effectuée", type = "message")
      beepr::beep(10)
    }, error = function(e) {
      showNotification(paste("Erreur :", e$message), type = "error")
    })
  })
  
  # Nettoyage des facteurs
  observeEvent(input$clean_factors, {
    req(rv$clean_data)
    tryCatch({
      rv$clean_data <- rv$clean_data %>%
        mutate(across(where(is.factor), janitor::make_clean_names))
      log_action("🏷️ Facteurs nettoyés (make_clean_names sur facteurs)")
      showNotification("✔ Facteurs nettoyés", type = "message")
      beepr::beep(10)
    }, error = function(e) {
      showNotification(paste("Erreur :", e$message), type = "error")
    })
  })
  
  # Arrondi numérique
  observeEvent(input$round_numeric, {
    req(rv$clean_data)
    tryCatch({
      digits <- input$round_digits
      rv$clean_data <- rv$clean_data %>%
        mutate(across(where(is.numeric), ~round(., digits)))
      log_action(paste0("🧮 Nombres arrondis à ", digits, " décimales"))
      showNotification("✔ Nombres arrondis", type = "message")
      beepr::beep(10)
    }, error = function(e) {
      showNotification(paste("Erreur :", e$message), type = "error")
    })
  })
  
  # Conversion en NA
  observeEvent(input$convert_to_na, {
    req(rv$clean_data)
    tryCatch({
      na_strings <- strsplit(input$na_strings, ",\\s*")[[1]] %>% trimws()
      rv$clean_data <- rv$clean_data %>%
        mutate(across(everything(), ~replace(., . %in% na_strings, NA)))
      log_action(paste("🔄 Valeurs converties en NA :", paste(na_strings, collapse = ", ")))
      showNotification("✔ Valeurs converties en NA", type = "message")
      beepr::beep(10)
    }, error = function(e) {
      showNotification(paste("Erreur :", e$message), type = "error")
    })
  })
  
  # Supprimer colonnes avec NA
  observeEvent(input$remove_na_cols, {
    req(rv$clean_data)
    tryCatch({
      rv$clean_data <- janitor::remove_empty(rv$clean_data, "cols")
      log_action("❌ Colonnes avec NA supprimées (via remove_empty cols)")
      showNotification("✔ Colonnes avec NA supprimées", type = "message")
      beepr::beep(10)
    }, error = function(e) {
      showNotification(paste("Erreur :", e$message), type = "error")
    })
  })
  
  # Colonnes à valeur unique
  observeEvent(input$remove_single_value, {
    req(rv$clean_data)
    tryCatch({
      rv$clean_data <- janitor::remove_constant(rv$clean_data)
      log_action("🔂 Colonnes à valeur unique supprimées (remove_constant)")
      showNotification("✔ Colonnes à valeur unique supprimées", type = "message")
      beepr::beep(10)
    }, error = function(e) {
      showNotification(paste("Erreur :", e$message), type = "error")
    })
  })
  
  # Nettoyage datetime
  observeEvent(input$clean_datetime, {
    req(rv$clean_data, input$datetime_col)
    
    tryCatch({
      col_name <- input$datetime_col
      date_data <- rv$clean_data[[col_name]]
      
      # Essai progressif des formats
      parsed <- parse_date_time(
        date_data,
        orders = c("ymd HMS", "ymd HM", "ymd", "dmy HMS", "dmy HM", "dmy"),
        quiet = TRUE
      )
      
      if(all(is.na(parsed))) {
        showNotification("Aucun format de date reconnu. Conservation des valeurs originales.", 
                         type = "warning")
      } else {
        rv$clean_data[[col_name]] <- parsed
        showNotification("Conversion de date réussie", type = "message")
      }
      
    }, error = function(e) {
      showNotification(paste("Erreur conversion date:", e$message), type = "error")
    })
  })
  
  # Uniformisation de la casse
  observeEvent(input$clean_case, {
    req(rv$clean_data)
    tryCatch({
      rv$clean_data <- rv$clean_data %>%
        mutate(across(where(is.character), ~stringr::str_to_lower(.)))
      log_action("🔤 Casse uniformisée (str_to_lower sur chaînes)")
      showNotification("✔ Casse uniformisée", type = "message")
      beepr::beep(10)
    }, error = function(e) {
      showNotification(paste("Erreur :", e$message), type = "error")
    })
  })
  
}

# 4. Lancement de l'application ----
shinyApp(ui = ui, server = server)