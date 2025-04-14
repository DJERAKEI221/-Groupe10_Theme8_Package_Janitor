# server/server_analysis.R

# Server
observeEvent(input$create_tabyl, {
  req(rv$clean_data, input$tabyl_vars)
  
  tryCatch({
    vars <- input$tabyl_vars
    
    if (length(vars) == 1) {
      rv$tabyl_data <- janitor::tabyl(rv$clean_data, !!sym(vars[1]))
    } else if (length(vars) == 2) {
      rv$tabyl_data <- janitor::tabyl(rv$clean_data, !!sym(vars[1]), !!sym(vars[2]))
    } else if (length(vars) == 3) {
      rv$tabyl_data <- janitor::tabyl(rv$clean_data, !!sym(vars[1]), !!sym(vars[2]), !!sym(vars[3]))
    }
    
    # Force l'actualisation de l'affichage
    output$tabyl_table <- renderDT({
      req(rv$tabyl_data)
      datatable(rv$tabyl_data)
    })
    
    output$data_plot <- renderPlotly({
      req(rv$clean_data)
      plot_ly(rv$clean_data, x = ~get(names(rv$clean_data)[1]), type = 'histogram')
    })
    
  }, error = function(e) showNotification(paste("Erreur:", e$message), type = "error"))
})



output$data_plot <- renderPlotly({
  req(rv$clean_data, input$plot_var)
  
  if (is.numeric(rv$clean_data[[input$plot_var]])) {
    p <- ggplot(rv$clean_data, aes(x = !!sym(input$plot_var))) +
      geom_histogram(fill = "#3498db", bins = 30, alpha = 0.8) +
      labs(title = paste("Distribution de", input$plot_var), x = input$plot_var, y = "Fréquence") +
      theme_minimal()
  } else {
    plot_data <- rv$clean_data %>% count(!!sym(input$plot_var)) %>% mutate(!!sym(input$plot_var) := fct_reorder(!!sym(input$plot_var), n))
    p <- ggplot(plot_data, aes(x = !!sym(input$plot_var), y = n)) +
      geom_col(fill = "#3498db", alpha = 0.8) +
      labs(title = paste("Distribution de", input$plot_var), x = input$plot_var, y = "Fréquence") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
  ggplotly(p) %>%
    config(displayModeBar = FALSE) %>%
    layout(title = list(text = paste0("Distribution de ", input$plot_var,
                                      "<br><sup>", nrow(rv$clean_data), " observations</sup>")))
})
