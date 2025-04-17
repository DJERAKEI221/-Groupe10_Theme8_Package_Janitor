output$download_data <- downloadHandler(
  filename = function() {
    paste(input$export_filename,
          switch(input$export_format,
                 "csv" = ".csv",
                 "xlsx" = ".xlsx",
                 "rds" = ".rds",
                 "sav" = ".sav",
                 "dta" = ".dta",
                 "feather" = ".feather",
                 "fst" = ".fst",
                 "parquet" = ".parquet",
                 "json" = ".json"),
          sep = "")
  },
  content = function(file) {
    req(rv$clean_data)
    tryCatch({
      # Créer une copie des données pour modification
      export_data <- rv$clean_data
      
      # Nettoyer les noms de variables pour SPSS/Stata si nécessaire
      if(input$export_format %in% c("sav", "dta")) {
        # Remplacer les espaces et caractères spéciaux
        names(export_data) <- gsub("[^[:alnum:]]", "_", names(export_data))
        
        # S'assurer que les noms commencent par une lettre
        names(export_data) <- gsub("^([^[:alpha:]])", "var_\\1", names(export_data))
        
        # Tronquer les noms trop longs (SPSS max 64, Stata max 32)
        max_len <- ifelse(input$export_format == "sav", 64, 32)
        names(export_data) <- substr(names(export_data), 1, max_len)
        
        # S'assurer que les noms sont uniques
        names(export_data) <- make.unique(names(export_data), sep = "_")
      }
      
      # Exporter selon le format
      switch(input$export_format,
             "csv" = write.csv(export_data, file, row.names = FALSE, fileEncoding = "UTF-8"),
             "xlsx" = writexl::write_xlsx(list("Data" = export_data), file),
             "rds" = saveRDS(export_data, file),
             "sav" = haven::write_sav(export_data, file),
             "dta" = haven::write_dta(export_data, file),
             "feather" = arrow::write_feather(export_data, file),
             "fst" = fst::write_fst(export_data, file, compress = 100),
             "parquet" = arrow::write_parquet(export_data, file),
             "json" = jsonlite::write_json(export_data, file, pretty = TRUE))
      
      showNotification(
        tags$span(icon("check-circle"), "Export réussi"),
        type = "message", 
        duration = 5
      )
    }, error = function(e) {
      showNotification(
        tags$span(icon("exclamation-triangle"), paste("Erreur:", e$message)),
        type = "error",
        duration = NULL
      )
    })
  }
)