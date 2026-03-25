# Script para ver exactamente qué responde Gemini

library(httr)
library(jsonlite)
library(base64enc)

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

api_key <- Sys.getenv("GEMINI_API_KEY", unset = "GEMINI_API_KEY_REMOVED")

cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║  DEBUG: Inspeccionar respuesta completa de Gemini          ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

# Crear una imagen de prueba simple
temp_img <- tempfile(fileext = ".png")
png(temp_img, width = 400, height = 300)
plot(1:10, main = "Test Chart", xlab = "X", ylab = "Y")
dev.off()

cat("✅ Imagen creada\n\n")

# Leer y codificar la imagen
img_raw <- readBin(temp_img, "raw", file.info(temp_img)$size)
img_base64 <- base64enc::base64encode(img_raw)

# Preparar request
body_json <- list(
  contents = list(
    list(
      parts = list(
        list(text = "Describe this chart briefly."),
        list(
          inline_data = list(
            mime_type = "image/png",
            data = img_base64
          )
        )
      )
    )
  ),
  generationConfig = list(
    temperature = 0.7,
    maxOutputTokens = 200
  )
)

body_json_str <- jsonlite::toJSON(body_json, auto_unbox = TRUE)

# Hacer la llamada
api_url <- paste0(
  "https://generativelanguage.googleapis.com/v1/models/gemini-2.5-flash:generateContent?key=",
  api_key
)

cat("🚀 Llamando API...\n\n")

response <- httr::POST(
  url = api_url,
  body = body_json_str,
  httr::add_headers("Content-Type" = "application/json"),
  encode = "raw"
)

status <- httr::status_code(response)
cat("Status:", status, "\n\n")

response_text <- httr::content(response, as = "text", encoding = "UTF-8")

cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║  RESPUESTA COMPLETA (RAW):                                 ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")
cat(response_text)
cat("\n\n")

if (status == 200) {
  cat("╔════════════════════════════════════════════════════════════╗\n")
  cat("║  ANALIZANDO ESTRUCTURA JSON:                               ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")

  tryCatch({
    response_json <- jsonlite::fromJSON(response_text)

    cat("Estructura del JSON:\n")
    cat("  - Campos principales:", paste(names(response_json), collapse = ", "), "\n\n")

    if (!is.null(response_json$candidates)) {
      cat("✅ Campo 'candidates' ENCONTRADO\n")
      cat("   - Número de candidatos:", length(response_json$candidates), "\n\n")

      if (length(response_json$candidates) > 0) {
        first_candidate <- response_json$candidates[[1]]
        cat("  Primer candidato contiene:\n")
        cat("    - Campos:", paste(names(first_candidate), collapse = ", "), "\n\n")

        if (!is.null(first_candidate$content)) {
          cat("  ✅ Campo 'content' ENCONTRADO\n")
          cat("     - Campos:", paste(names(first_candidate$content), collapse = ", "), "\n\n")

          if (!is.null(first_candidate$content$parts)) {
            cat("  ✅ Campo 'parts' ENCONTRADO\n")
            cat("     - Número de parts:", length(first_candidate$content$parts), "\n\n")

            if (length(first_candidate$content$parts) > 0) {
              first_part <- first_candidate$content$parts[[1]]
              cat("  Primer 'part' contiene:\n")
              cat("    - Campos:", paste(names(first_part), collapse = ", "), "\n\n")

              if (!is.null(first_part$text)) {
                cat("  ✅ Campo 'text' ENCONTRADO!\n\n")
                cat("TEXTO EXTRAÍDO:\n")
                cat("════════════════════════════════════════════════════\n")
                cat(first_part$text)
                cat("\n════════════════════════════════════════════════════\n")
              } else {
                cat("❌ Campo 'text' NO ENCONTRADO en el primer part\n")
                cat("   Contenido del part:\n")
                str(first_part)
              }
            }
          } else {
            cat("❌ Campo 'parts' NO ENCONTRADO\n")
          }
        } else {
          cat("❌ Campo 'content' NO ENCONTRADO\n")
        }
      }
    } else {
      cat("❌ Campo 'candidates' NO ENCONTRADO\n")
    }

  }, error = function(e) {
    cat("❌ Error parseando JSON:", e$message, "\n")
  })
}

unlink(temp_img)
cat("\n✅ Debug completado\n")
