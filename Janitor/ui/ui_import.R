# ui/ui_import

div(class = "container-fluid",
    div(class = "row",
        column(4,
               div(class = "well-panel",
                   h4(icon("file-import"), "Importer des données"),
                   fileInput("file_input", NULL, 
                             accept = c(".csv", ".xlsx", ".xls", ".tsv", ".txt", ".sav", ".dta", ".sas7bdat", ".json", ".rds")
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
                                class = "btn btn-danger btn-action")
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
