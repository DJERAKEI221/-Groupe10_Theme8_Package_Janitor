# server/server_export.R

output$clean_table <- renderDT({
  req(rv$clean_data)
  datatable(rv$clean_data,
            options = list(scrollX = TRUE, pageLength = 5, dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel')),
            extensions = 'Buttons', rownames = FALSE)
})

output$comparison_table <- renderDT({
  req(rv$comparison_data)
  datatable(rv$comparison_data,
            options = list(scrollX = TRUE, pageLength = 5, dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel')),
            extensions = 'Buttons', rownames = FALSE)
})

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
             "json" = jsonlite::write_json(rv$clean_data, file))
      showNotification("✔ Export réussi", type = "message")
    }, error = function(e) {
      showNotification(paste("Erreur:", e$message), type = "error")
    })
  }
)
