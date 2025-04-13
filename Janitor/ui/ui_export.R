# ui/ui_export.R

div(class = "container-fluid",
    div(class = "row",
        column(6,
               div(class = "well-panel",
                   h4(icon("download"), "Exporter les données"),
                   selectInput("export_format", "Format:",
                               c("CSV" = "csv", "Excel" = "xlsx", "RDS" = "rds", "SPSS" = "sav", "JSON" = "json")),
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
