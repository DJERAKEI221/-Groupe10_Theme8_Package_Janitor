# ui/ui_clean_advanced.R

div(class = "container-fluid",
    div(class = "row",
        column(4,
               div(class = "well-panel",
                   h4(icon("calculator"), "Manipulation numérique"),
                   numericInput("round_digits", "Décimales:", 2, min = 0, max = 10),
                   actionButton("round_numeric", "Arrondir les nombres", icon = icon("calculator"), class = "btn btn-info btn-action"),
                   hr(),
                   h4(icon("question-circle"), "Gestion des NA"),
                   textInput("na_strings", "Valeurs à convertir en NA:", "NA, N/A, null, missing"),
                   actionButton("convert_to_na", "Convertir en NA", icon = icon("ban"), class = "btn btn-warning btn-action"),
                   actionButton("remove_na_cols", "Supprimer colonnes avec NA", icon = icon("trash"), class = "btn btn-danger btn-action")
               )
        ),
        column(4,
               div(class = "well-panel",
                   h4(icon("columns"), "Gestion des colonnes"),
                   actionButton("remove_single_value", "Supprimer colonnes à valeur unique", icon = icon("filter"), class = "btn btn-warning btn-action"),
                   actionButton("clean_datetime", "Nettoyer les dates/heures", icon = icon("clock"), class = "btn btn-info btn-action"),
                   selectInput("datetime_col", "Colonne datetime:", choices = NULL),
                   hr(),
                   h4(icon("table"), "Formatage"),
                   actionButton("clean_case", "Uniformiser la casse", icon = icon("font"), class = "btn btn-primary btn-action")
               )
        ),
        column(4,
               div(class = "well-panel",
                   h4(icon("not-equal"), "Comparaison"),
                   actionButton("compare_df", "Comparer avec données brutes", icon = icon("code-compare"), class = "btn btn-info btn-action"),
                   hr(),
                   h4(icon("file-excel"), "Fonctions Excel"),
                   actionButton("excel_numeric", "Convertir dates Excel", icon = icon("calendar"), class = "btn btn-success btn-action"),
                   numericInput("excel_col", "Colonne numérique:", value = 1, min = 1)
               )
        )
    )
)
