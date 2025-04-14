# server/server_main.R

function(input, output, session) {
  source("server/server_import.R", local = TRUE)
  source("server/server_clean_base.R", local = TRUE)
  source("server/server_clean_advanced.R", local = TRUE)
  source("server/server_analysis.R", local = TRUE)
  source("server/server_export.R", local = TRUE)
  source("server/server_report.R", local = TRUE)
  source("server/server_search.R", local = TRUE)
}
