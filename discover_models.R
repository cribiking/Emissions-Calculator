# Script para descubrir qué modelos están realmente disponibles en Gemini API

library(httr)
library(jsonlite)

# Cargar variables de entorno
if (file.exists(".env")) {
  env_file <- readLines(".env")
  env_file <- env_file[!grepl("^#|^$", env_file)]
  for (line in env_file) {
    parts <- strsplit(line, "=")[[1]]
    if (length(parts) == 2) {
      var_name <- trimws(parts[1])
      var_value <- trimws(parts[2])
      do.call(Sys.setenv, setNames(list(var_value), var_name))
    }
  }
}

api_key <- Sys.getenv("GEMINI_API_KEY", unset = "AIzaSyBX7n_r_DkiZbkfaPzYoxG0370SeVi1KOI")

cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║  DISCOVERY: Listar modelos disponibles en Gemini API       ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

# Endpoint para listar modelos
list_models_url <- paste0(
  "https://generativelanguage.googleapis.com/v1/models?key=",
  api_key
)

cat("🔍 Llamando a ListModels endpoint...\n")
cat("URL:", gsub(api_key, "***API_KEY***", list_models_url), "\n\n")

response <- httr::GET(list_models_url)

cat("Response Status:", httr::status_code(response), "\n\n")

response_text <- httr::content(response, as = "text", encoding = "UTF-8")

if (httr::http_error(response)) {
  cat("❌ Error en la respuesta:\n")
  cat(response_text, "\n")
} else {
  cat("✅ Respuesta recibida exitosamente\n\n")

  tryCatch({
    response_json <- jsonlite::fromJSON(response_text)

    if (!is.null(response_json$models)) {
      cat("📊 MODELOS DISPONIBLES:\n")
      cat("════════════════════════════════════════════════════\n\n")

      models_df <- data.frame()

      for (i in seq_along(response_json$models)) {
        model <- response_json$models[[i]]
        model_name <- model$name
        display_name <- if (!is.null(model$displayName)) model$displayName else "N/A"
        version <- if (!is.null(model$version)) model$version else "N/A"

        cat("Modelo", i, ":\n")
        cat("  Name:        ", model_name, "\n")
        cat("  Display:     ", display_name, "\n")
        cat("  Version:     ", version, "\n")

        # Mostrar métodos soportados
        if (!is.null(model$supportedGenerationMethods)) {
          cat("  Methods:     ", paste(model$supportedGenerationMethods, collapse=", "), "\n")
        }
        cat("\n")
      }

      cat("════════════════════════════════════════════════════\n\n")
      cat("✅ Total de modelos disponibles:", length(response_json$models), "\n")

      # Extraer nombres de modelos para usar
      model_names <- sapply(response_json$models, function(m) {
        # Extraer el nombre del modelo (la parte después de "models/")
        gsub("models/", "", m$name)
      })

      cat("\n📋 NOMBRES DE MODELOS DISPONIBLES (para usar en API):\n")
      for (i in seq_along(model_names)) {
        cat("  ", i, ".", model_names[i], "\n")
      }

      # Sugerir cuál usar
      cat("\n💡 RECOMENDACIÓN:\n")
      if (any(grepl("gemini-1.5-flash", model_names))) {
        cat("  Usa: gemini-1.5-flash\n")
      } else if (any(grepl("gemini-pro", model_names))) {
        cat("  Usa: gemini-pro\n")
      } else if (any(grepl("gemini-1.5-pro", model_names))) {
        cat("  Usa: gemini-1.5-pro\n")
      } else if (length(model_names) > 0) {
        cat("  Usa:", model_names[1], "\n")
      }

    } else {
      cat("⚠️ No se encontró la lista de modelos en la respuesta\n")
      cat("Respuesta completa:\n")
      cat(response_text, "\n")
    }
  }, error = function(e) {
    cat("❌ Error parseando JSON:", e$message, "\n")
    cat("Respuesta raw:\n")
    cat(response_text, "\n")
  })
}
