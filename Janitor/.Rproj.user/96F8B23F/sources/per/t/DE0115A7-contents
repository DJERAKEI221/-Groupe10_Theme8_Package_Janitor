# ui/ui_clean_base.R

div(class = "container-fluid",
    div(class = "row",
        column(4,
               div(class = "well-panel",
                   h4(icon("magic"), "Nettoyage automatique"),
                   actionButton("clean_names", "Nettoyer les noms", icon = icon("pencil-alt"), class = "btn btn-primary btn-action"),
                   actionButton("remove_empty", "Supprimer les vides", icon = icon("trash-alt"), class = "btn btn-primary btn-action"),
                   actionButton("remove_constant", "Supprimer constantes", icon = icon("filter"), class = "btn btn-primary btn-action"),
                   actionButton("clean_factors", "Nettoyer les facteurs", icon = icon("tags"), class = "btn btn-primary btn-action")
               ),
               div(class = "well-panel",
                   h4(icon("calendar-alt"), "Conversion de dates"),
                   numericInput("excel_date", "Valeur Excel:", 44197),
                   actionButton("convert_date", "Convertir en date", icon = icon("exchange-alt"), class = "btn btn-warning btn-action"),
                   verbatimTextOutput("excel_date_result")
               )
        ),
        column(8,
               div(class = "well-panel",
                   h4(icon("clone"), "Gestion des doublons"),
                   selectizeInput("dupe_cols", "Colonnes à vérifier:", choices = NULL, multiple = TRUE),
                   actionButton("find_dupes", "Identifier les doublons", icon = icon("search"), class = "btn btn-warning"),
                   hr(),
                   withSpinner(DTOutput("dupes_table"), type = 6, color = "#3498db")
               )
        )
    )
)
