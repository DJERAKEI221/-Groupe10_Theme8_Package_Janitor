# server/server_search.R

observe({
  updateSelectizeInput(
    session,
    "janitor_functions",
    choices = janitor_functions,
    server = TRUE
  )
})

observeEvent(input$search_google, {
  req(input$janitor_functions)
  
  showNotification("Préparation de la recherche Google...", duration = 2)
  
  search_terms <- paste0("package+janitor+", paste(input$janitor_functions, collapse = "+"))
  google_url <- paste0("https://www.google.com/search?q=", search_terms)
  
  output$google_results <- renderUI({
    tagList(
      p("Recherche Google pour:", paste(input$janitor_functions, collapse = ", ")),
      tags$a(href = google_url, target = "_blank", 
             "Cliquez ici pour voir les résultats sur Google",
             class = "btn btn-default")
    )
  })
  
  runjs(paste0("window.open('", google_url, "', '_blank');"))
})
