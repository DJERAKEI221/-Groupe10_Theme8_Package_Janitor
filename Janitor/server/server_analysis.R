# server/server_analysis.R

observeEvent(input$create_tabyl, {
  req(rv$clean_data, input$tabyl_vars)
  tryCatch({
    if (length(input$tabyl_vars) == 1) {
      rv$tabyl_data <- janitor::tabyl(rv$clean_data[[input$tabyl_vars[1]]])
    } else {
      rv$tabyl_data <- janitor::tabyl(rv$clean_data[[input$tabyl_vars[1]]], rv$clean_data[[input$tabyl_vars[2]]])
    }
    
    if ("totals" %in% input$adorn_funcs) {
      rv$tabyl_data <- rv$tabyl_data %>% janitor::adorn_totals()
    }
    if ("percent" %in% input$adorn_funcs) {
      rv$tabyl_data <- rv$tabyl_data %>% janitor::adorn_percentages(input$percent_axis)
    }
    if ("pct_format" %in% input$adorn_funcs) {
      rv$tabyl_data <- rv$tabyl_data %>% janitor::adorn_pct_formatting()
    }
    if ("ns" %in% input$adorn_funcs) {
      rv$tabyl_data <- rv$tabyl_data %>% janitor::adorn_ns()
    }
    
    showNotification("✔ Tableau croisé généré", type = "message")
    beepr::beep(10)
  }, error = function(e) {
    showNotification(paste("Erreur:", e$message), type = "error")
  })
})

output$tabyl_table <- renderDT({
  req(rv$tabyl_data)
  datatable(rv$tabyl_data,
            options = list(scrollX = TRUE, pageLength = 5, dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel')),
            extensions = 'Buttons', rownames = FALSE) %>%
    formatPercentage(columns = which(sapply(rv$tabyl_data, is.numeric)), 1)
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
