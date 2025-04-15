# server/server_clean_advanced.R

# Arrondi numérique
observeEvent(input$round_numeric, {
  req(rv$clean_data)
  tryCatch({
    digits <- input$round_digits
    rv$clean_data <- rv$clean_data %>%
      mutate(across(where(is.numeric), ~round(., digits)))
    log_action(paste0("🧮 Nombres arrondis à ", digits, " décimales"))
    showNotification("✔ Nombres arrondis", type = "message")
    beepr::beep(10)
  }, error = function(e) {
    showNotification(paste("Erreur :", e$message), type = "error")
  })
})



# Convertir des chaînes spécifiques en NA
observeEvent(input$convert_to_na, {
  req(rv$clean_data)
  tryCatch({
    na_strings <- strsplit(input$na_strings, ",\\s*")[[1]] %>% trimws()
    rv$clean_data <- rv$clean_data %>%
      mutate(across(everything(), ~replace(., . %in% na_strings, NA)))
    log_action(paste("🔄 Valeurs converties en NA :", paste(na_strings, collapse = ", ")))
    showNotification("✔ Valeurs converties en NA", type = "message")
    beepr::beep(10)
  }, error = function(e) {
    showNotification(paste("Erreur :", e$message), type = "error")
  })
})



# Supprimer colonnes avec NA
observeEvent(input$remove_na_cols, {
  req(rv$clean_data)
  tryCatch({
    rv$clean_data <- janitor::remove_empty(rv$clean_data, "cols")
    log_action("❌ Colonnes avec NA supprimées (via remove_empty cols)")
    showNotification("✔ Colonnes avec NA supprimées", type = "message")
    beepr::beep(10)
  }, error = function(e) {
    showNotification(paste("Erreur :", e$message), type = "error")
  })
})

# Colonnes à valeur unique
observeEvent(input$remove_single_value, {
  req(rv$clean_data)
  tryCatch({
    rv$clean_data <- janitor::remove_constant(rv$clean_data)
    log_action("🔂 Colonnes à valeur unique supprimées (remove_constant)")
    showNotification("✔ Colonnes à valeur unique supprimées", type = "message")
    beepr::beep(10)
  }, error = function(e) {
    showNotification(paste("Erreur :", e$message), type = "error")
  })
})

# Nettoyage datetime

observeEvent(input$clean_datetime, {
  req(rv$clean_data, input$datetime_col)
  tryCatch({
    col_name <- input$datetime_col
    date_data <- rv$clean_data[[col_name]]
    
    # Essai de plusieurs formats de date
    parsed <- parse_date_time(date_data, 
                              orders = c("ymd HMS", "dmy HMS", "mdy HMS", "ymd", "dmy", "mdy"))
    
    if(any(is.na(parsed))) {
      showNotification("Certaines dates n'ont pas pu être converties", type = "warning")
    }
    
    rv$clean_data[[col_name]] <- parsed
    showNotification("Conversion de date effectuée", type = "message")
    
  }, error = function(e) {
    showNotification(paste("Erreur conversion date:", e$message), type = "error")
  })
})


# Conversion colonne Excel en dates
observeEvent(input$excel_numeric, {
  req(rv$clean_data, input$datetime_col)
  tryCatch({
    col_name <- input$datetime_col
    
    numeric_values <- suppressWarnings(as.numeric(rv$clean_data[[col_name]]))
    
    if(all(is.na(numeric_values))) {
      stop("La colonne sélectionnée ne contient pas de valeurs numériques valides")
    }
    
    rv$clean_data <- rv$clean_data %>%
      mutate(!!sym(col_name) := janitor::excel_numeric_to_date(numeric_values))
    
    showNotification(
      paste("Colonne", col_name, "convertie en dates avec succès"), 
      type = "message"
    )
    beepr::beep(10)
  }, error = function(e) {
    showNotification(paste("Erreur:", e$message), type = "error")
  })
})


# Uniformisation de la casse
observeEvent(input$clean_case, {
  req(rv$clean_data)
  tryCatch({
    rv$clean_data <- rv$clean_data %>%
      mutate(across(where(is.character), ~stringr::str_to_lower(.)))
    log_action("🔤 Casse uniformisée (str_to_lower sur chaînes)")
    showNotification("✔ Casse uniformisée", type = "message")
    beepr::beep(10)
  }, error = function(e) {
    showNotification(paste("Erreur :", e$message), type = "error")
  })
})



# Comparaison de dataframes
observeEvent(input$compare_df, {
  req(rv$raw_data, rv$clean_data)
  tryCatch({
    rv$comparison_data <- janitor::compare_df_cols(rv$raw_data, rv$clean_data)
    showNotification("Comparaison effectuée", type = "message")
    beepr::beep(10)
  }, error = function(e) {
    showNotification(paste("Erreur:", e$message), type = "error")
  })
})