# ui/ui_search.R

div(class = "container-fluid",
    div(class = "row",
        column(4,
               div(class = "well-panel",
                   h4(icon("search"), "Recherche des fonctions Janitor"),
                   selectizeInput("janitor_functions", "Fonctions Janitor:", choices = janitor_functions,
                                  multiple = TRUE,
                                  options = list(
                                    placeholder = 'Sélectionnez une ou plusieurs fonctions',
                                    maxOptions = length(janitor_functions)
                                  )),
                   actionButton("search_google", "Rechercher sur Google", icon = icon("search"), class = "btn btn-primary btn-action")
               )
        ),
        column(8,
               div(class = "well-panel",
                   h4(icon("search"), "Résultats de recherche"),
                   uiOutput("google_results"),
                   tags$div(id = "loading_message", style = "display: none;",
                            p("Redirection vers Google..."))
               )
        )
    )
)
