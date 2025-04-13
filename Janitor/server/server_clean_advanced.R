# server/server_clean_advanced.R

# Arrondir les valeurs numériques
observeEvent(input$round_numeric, {
  req(rv$clean_data)
  tryCatch({
    digits <- input$round_digits
    rv$clean_data <- rv$clean_data %>% mutate(across(where(is.numeric), ~round(., digits)))
    showNotification("✔ Nombres arrondis", type = "message")
    beepr::beep(10)
  }, error = function(e) {
    showNotification(paste("Erreur:", e$message), type = "error")
  })
})

# Convertir des chaînes spécifiques en NA
observeEvent(input$convert_to_na, {
  req(rv$clean_data)
  tryCatch({
    na_strings <- strsplit(input$na_strings, ",\\s*")[[1]] %>% trimws()  # Correction ici
    rv$clean_data <- rv$clean_data %>% mutate(across(everything(), ~replace(., . %in% na_strings, NA)))
    showNotification("✔ Valeurs converties en NA", type = "message")
    beepr::beep(10)
  }, error = function(e) {
    showNotification(paste("Erreur:", e$message), type = "error")
  })
})

# Supprimer les colonnes vides
observeEvent(input$remove_na_cols, {
  req(rv$clean_data)
  tryCatch({
    rv$clean_data <- janitor::remove_empty(rv$clean_data, "cols")
    showNotification("✔ Colonnes avec NA supprimées", type = "message")
    beepr::beep(10)
  }, error = function(e) {
    showNotification(paste("Erreur:", e$message), type = "error")
  })
})

# Supprimer les colonnes à valeur unique
observeEvent(input$remove_single_value, {
  req(rv$clean_data)
  tryCatch({
    rv$clean_data <- janitor::remove_constant(rv$clean_data)
    showNotification("✔ Colonnes à valeur unique supprimées", type = "message")
    beepr::beep(10)
  }, error = function(e) {
    showNotification(paste("Erreur:", e$message), type = "error")
  })
})

# Nettoyer les colonnes de date/heure
observeEvent(input$clean_datetime, {
  req(rv$clean_data, input$datetime_col)
  tryCatch({
    col_name <- input$datetime_col
    date_data <- rv$clean_data[[col_name]]
    parsed <- parse_date_time(date_data, orders = c("ymd HMS", "ymd", "dmy HMS", "dmy", "mdy HMS", "mdy"), quiet = TRUE)
    rv$clean_data[[col_name]] <- parsed
    showNotification("✔ Conversion de date réussie", type = "message")
  }, error = function(e) {
    showNotification(paste("Erreur conversion date:", e$message), type = "error")
  })
})

# Convertir les nombres Excel en dates
observeEvent(input$excel_numeric, {
  req(rv$clean_data, input$datetime_col)
  tryCatch({
    col_name <- input$datetime_col
    numeric_values <- suppressWarnings(as.numeric(rv$clean_data[[col_name]]))
    if (all(is.na(numeric_values))) stop("Colonne invalide pour conversion.")
    rv$clean_data[[col_name]] <- janitor::excel_numeric_to_date(numeric_values)
    showNotification("✔ Conversion Excel en dates réussie", type = "message")
    beepr::beep(10)
  }, error = function(e) {
    showNotification(paste("Erreur:", e$message), type = "error")
  })
})

# Uniformiser la casse des caractères
observeEvent(input$clean_case, {
  req(rv$clean_data)
  tryCatch({
    rv$clean_data <- rv$clean_data %>% mutate(across(where(is.character), ~stringr::str_to_lower(.)))
    showNotification("✔ Casse uniformisée", type = "message")
    beepr::beep(10)
  }, error = function(e) {
    showNotification(paste("Erreur:", e$message), type = "error")
  })
})

# Comparer les dataframes avant/après
observeEvent(input$compare_df, {
  req(rv$raw_data, rv$clean_data)
  tryCatch({
    rv$comparison_data <- janitor::compare_df_cols(rv$raw_data, rv$clean_data)
    showNotification("✔ Comparaison effectuée", type = "message")
    beepr::beep(10)
  }, error = function(e) {
    showNotification(paste("Erreur:", e$message), type = "error")
  })
})