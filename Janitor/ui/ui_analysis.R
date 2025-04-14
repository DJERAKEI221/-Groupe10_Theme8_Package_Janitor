# ui/ui_analysis.R

div(class = "container-fluid",
    div(class = "row",
        column(4,
               div(class = "well-panel",
                   h4(icon("table"), "Tableaux croisés"),
                   selectizeInput("tabyl_vars", "Variables pour tabyl:", choices = NULL, multiple = TRUE, options = list(maxItems = 3)),
                   actionButton("create_tabyl", "Générer le tableau", icon = icon("calculator"), class = "btn btn-success btn-action"),
                   hr(),
                   awesomeCheckboxGroup("adorn_funcs", "Options de formatage:",
                                        c("Ajouter totaux" = "totals",
                                          "Calculer pourcentages" = "percent",
                                          "Formater pourcentages" = "pct_format",
                                          "Afficher effectifs" = "ns")),
                   selectInput("percent_axis", "Axe pour les %:", choices = c("ligne" = "row", "colonne" = "col"))
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
