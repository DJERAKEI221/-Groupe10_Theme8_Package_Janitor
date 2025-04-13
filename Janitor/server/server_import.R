# server/server_import.R

observeEvent(input$file_input, {
  req(input$file_input)
  
  tryCatch({
    showLoader()
    
    ext <- tolower(tools::file_ext(input$file_input$name))
    path <- input$file_input$datapath
    
    rv$raw_data <- switch(ext,
                          csv = if (input$sep == "\t") {
                            data.table::fread(path, header = input$header, encoding = "Latin-1") %>% as_tibble()
                          } else {
                            readr::read_delim(path, delim = input$sep, locale = readr::locale(decimal_mark = input$dec))
                          },
                          xlsx = readxl::read_excel(path),
                          xls = readxl::read_excel(path),
                          tsv = readr::read_tsv(path, col_names = input$header),
                          txt = readr::read_delim(path, delim = input$sep, locale = readr::locale(decimal_mark = input$dec)),
                          sav = haven::read_sav(path),
                          dta = haven::read_dta(path),
                          sas7bdat = haven::read_sas(path),
                          json = jsonlite::fromJSON(path, flatten = TRUE),
                          rds = readRDS(path),
                          rio::import(path)
    )
    
    if (!is.data.frame(rv$raw_data)) {
      rv$raw_data <- as.data.frame(rv$raw_data)
    }
    
    rv$raw_data <- rv$raw_data %>%
      mutate(across(where(~inherits(., "haven_labelled")), haven::as_factor))
    
    rv$clean_data <- rv$raw_data
    
    updateSelectInput(session, "plot_var", choices = names(rv$clean_data))
    updateSelectInput(session, "datetime_col", choices = names(rv$clean_data))
    updateSelectizeInput(session, "dupe_cols", choices = names(rv$clean_data))
    updateSelectizeInput(session, "tabyl_vars", choices = names(rv$clean_data))
    
    beepr::beep(10)
  }, error = function(e) {
    showNotification(paste("Erreur lors du chargement:", e$message), type = "error")
    rv$raw_data <- NULL
    rv$clean_data <- NULL
  }, finally = {
    hideLoader()
  })
})

output$raw_table <- renderDT({
  req(rv$raw_data)
  datatable(rv$raw_data,
            options = list(scrollX = TRUE, pageLength = 5, dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel')),
            extensions = 'Buttons', rownames = FALSE)
})

output$data_summary <- renderPrint({
  req(rv$raw_data)
  cat("Dimensions:", dim(rv$raw_data)[1], "lignes x", dim(rv$raw_data)[2], "colonnes\n")
  cat("\nStructure:\n")
  str(rv$raw_data)
})

output$data_types <- renderPrint({
  req(rv$raw_data)
  cat("\nTypes de données:\n")
  sapply(rv$raw_data, class) %>% print()
})

observeEvent(input$reset_data, {
  rv$raw_data <- NULL
  rv$clean_data <- NULL
  rv$tabyl_data <- NULL
  rv$dupes_data <- NULL
  rv$comparison_data <- NULL
  reset("file_input")
  updateSelectInput(session, "plot_var", choices = character(0))
  updateSelectInput(session, "datetime_col", choices = character(0))
  updateSelectizeInput(session, "dupe_cols", choices = character(0))
  updateSelectizeInput(session, "tabyl_vars", choices = character(0))
  beepr::beep(10)
})
