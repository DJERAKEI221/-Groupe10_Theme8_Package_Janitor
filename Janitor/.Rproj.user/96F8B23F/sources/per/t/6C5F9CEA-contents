# server/server_report.R

output$download_report <- downloadHandler(
  filename = function() {
    paste0("rapport-", Sys.Date(), ".html")
  },
  content = function(file) {
    params <- list(
      logs = rv$logs,
      date = Sys.Date(),
      data_sample = if (!is.null(rv$clean_data)) head(rv$clean_data, 10),
      title = input$report_title,
      notes = input$report_notes
    )
    
    rmarkdown::render(
      input = "rmd/rapport.Rmd",
      output_file = file,
      params = params,
      envir = new.env()
    )
  }
)
