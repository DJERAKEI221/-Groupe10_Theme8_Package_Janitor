# ui/ui_report.R

div(class = "container-fluid",
    div(class = "row",
        column(6,
               div(class = "well-panel",
                   h4(icon("file-alt"), "Générer un rapport HTML"),
                   textInput("report_title", "Titre du rapport", "Rapport de nettoyage"),
                   textAreaInput("report_notes", "Notes additionnelles", "", rows = 4),
                   downloadButton("download_report", "Télécharger le rapport")
               )
        )
    )
)
