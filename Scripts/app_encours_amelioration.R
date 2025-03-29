# Installer les packages si nécessaire
required_packages <- c(
  "shiny", "shinyjs", "janitor", "DT", "ggplot2", 
  "plotly", "beepr", "magrittr", "tidyr", "forcats",
  "readxl", "dplyr"
)

new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Charger les packages
lapply(required_packages, library, character.only = TRUE)

# Interface utilisateur
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      :root {
        --primary: #2c3e50;
        --secondary: #3498db;
        --success: #2ecc71;
        --danger: #e74c3c;
        --light: #ecf0f1;
      }
      body {
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
        background-color: #f9f9f9;
      }
      .well {
        background-color: white;
        border-radius: 5px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        padding: 20px;
        margin-bottom: 20px;
      }
      .tab-content {
        background-color: white;
        padding: 20px;
        border-radius: 0 0 5px 5px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      .btn {
        margin-right: 5px;
        margin-bottom: 5px;
      }
      .data-header {
        background-color: var(--primary);
        color: white;
        padding: 10px;
        border-radius: 5px 5px 0 0;
        margin-bottom: 0;
      }
      .nav-tabs > li > a {
        color: var(--primary);
      }
    "))
  ),
  
  titlePanel(h1("Netoiyage automatique avec Janitor", style = "color: var(--primary);")),
  p("Exploration toutes les fonctions du package janitor de manière interactive", 
    style = "font-size: 1.1em;"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h3("Données d'entrée"),
      fileInput("file_input", "Uploader un fichier",
                accept = c(".csv", ".xlsx", ".tsv")),
      
      hr(),
      
      h3("Options d'affichage"),
      checkboxInput("show_summary", "Afficher le résumé", value = TRUE),
      checkboxInput("show_structure", "Afficher la structure", value = TRUE),
      
      hr(),
      
      h3("Exporter les résultats"),
      downloadButton("download_data", "Télécharger les données"),
      actionButton("reset_data", "Réinitialiser", class = "btn btn-danger")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "main_tabs",
        
        tabPanel(
          "Nettoyage de Base",
          div(class = "well",
              fluidRow(
                column(6,
                       h4("clean_names()"),
                       actionButton("clean_names", "Appliquer", class = "btn btn-primary"),
                       br(), br(),
                       h4("remove_empty()"),
                       radioButtons("empty_type", "Type:",
                                    choices = c("Lignes" = "rows",
                                                "Colonnes" = "cols",
                                                "Les deux" = "both")),
                       actionButton("remove_empty", "Appliquer", class = "btn btn-primary")
                ),
                column(6,
                       h4("remove_constant()"),
                       actionButton("remove_constant", "Appliquer", class = "btn btn-primary"),
                       br(), br(),
                       h4("excel_numeric_to_date()"),
                       numericInput("excel_date", "Valeur Excel:", 44197),
                       actionButton("convert_date", "Convertir", class = "btn btn-primary"),
                       textOutput("excel_date_result")
                )
              )
          )
        ),
        
        tabPanel(
          "Gestion des Doublons",
          div(class = "well",
              fluidRow(
                column(6,
                       h4("get_dupes()"),
                       selectizeInput("dupe_cols", "Colonnes à vérifier:", 
                                      choices = character(0), 
                                      multiple = TRUE),
                       actionButton("find_dupes", "Trouver les doublons", class = "btn btn-primary")
                ),
                column(6,
                       h4("compare_df_cols()"),
                       fileInput("compare_file", "Uploader un 2ème fichier", 
                                 accept = c(".csv", ".xlsx")),
                       actionButton("compare_dfs", "Comparer", class = "btn btn-primary")
                )
              )
          )
        ),
        
        tabPanel(
          "Fonctions Tabyl",
          div(class = "well",
              fluidRow(
                column(4,
                       h4("tabyl()"),
                       selectInput("tabyl_var1", "Variable 1:", choices = character(0)),
                       selectInput("tabyl_var2", "Variable 2:", choices = character(0)),
                       actionButton("create_tabyl", "Créer tabyl", class = "btn btn-primary")
                ),
                column(4,
                       h4("Adorn functions"),
                       checkboxGroupInput("adorn_funcs", "Fonctions:",
                                          choices = c("Totals" = "totals",
                                                      "Percentages" = "percent",
                                                      "Formatting" = "pct_format",
                                                      "NS" = "ns"))
                ),
                column(4,
                       h4("Options"),
                       radioButtons("tabyl_axis", "Axe:",
                                    choices = c("row", "col", "all")),
                       textInput("tabyl_title", "Titre:")
                )
              )
          )
        ),
        
        tabPanel(
          "Visualisation",
          div(class = "well",
              plotlyOutput("data_plot"),
              DTOutput("clean_table")
          )
        )
      )
    )
  )
)

# Serveur
server <- function(input, output, session) {
  # Initialisation des sélecteurs
  updateSelectInput(session, "tabyl_var1", choices = character(0))
  updateSelectInput(session, "tabyl_var2", choices = character(0))
  updateSelectizeInput(session, "dupe_cols", choices = character(0))
  
  # Données réactives
  rv <- reactiveValues(
    raw_data = NULL,
    clean_data = NULL,
    tabyl_data = NULL
  )
  
  # Chargement des données
  observeEvent(input$file_input, {
    req(input$file_input)
    
    tryCatch({
      ext <- tools::file_ext(input$file_input$name)
      if(ext == "csv") {
        rv$raw_data <- read.csv(input$file_input$datapath)
      } else if(ext %in% c("xlsx", "xls")) {
        rv$raw_data <- readxl::read_excel(input$file_input$datapath)
      } else if(ext == "tsv") {
        rv$raw_data <- read.delim(input$file_input$datapath)
      }
      
      rv$clean_data <- rv$raw_data
      
      # Mise à jour des sélecteurs
      if(!is.null(rv$clean_data) && ncol(rv$clean_data) > 0) {
        updateSelectInput(session, "tabyl_var1", choices = names(rv$clean_data))
        updateSelectInput(session, "tabyl_var2", choices = names(rv$clean_data))
        updateSelectizeInput(session, "dupe_cols", choices = names(rv$clean_data))
      }
      
      beep(1)
    }, error = function(e) {
      showNotification(paste("Erreur:", e$message), type = "error")
    })
  })
  
  # Fonctions de nettoyage
  observeEvent(input$clean_names, {
    req(rv$clean_data)
    tryCatch({
      rv$clean_data <- rv$clean_data %>% clean_names()
      beep(2)
    }, error = function(e) {
      showNotification(paste("Erreur:", e$message), type = "error")
    })
  })
  
  observeEvent(input$remove_empty, {
    req(rv$clean_data)
    tryCatch({
      rv$clean_data <- rv$clean_data %>% remove_empty(which = input$empty_type)
      beep(3)
    }, error = function(e) {
      showNotification(paste("Erreur:", e$message), type = "error")
    })
  })
  
  # ... (ajouter toutes les autres fonctions de la même manière)
  
  # Affichage des données
  output$clean_table <- renderDT({
    req(rv$clean_data)
    datatable(rv$clean_data, options = list(scrollX = TRUE))
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("data_cleaned_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(rv$clean_data, file, row.names = FALSE)
    }
  )
  
  observeEvent(input$reset_data, {
    rv$raw_data <- NULL
    rv$clean_data <- NULL
    reset("file_input")
    beep(10)
  })
}

# Lancer l'application
shinyApp(ui = ui, server = server)