################################################################################
# INTEGRACIÓN CON GOOGLE GEMINI PARA ANÁLISIS DE GRÁFICOS
# Funciones para enviar imágenes a Gemini y obtener explicaciones
################################################################################

# Cargar variables de entorno desde .env
if (file.exists(".env")) {
  tryCatch({
    # Intentar usar dotenv si está instalado
    if (requireNamespace("dotenv", quietly = TRUE)) {
      dotenv::load_dot_env(".env")
      cat("✅ Variables de entorno cargadas con dotenv\n")
    } else {
      # Fallback: cargar manualmente si dotenv no está instalado
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
      cat("✅ Variables de entorno cargadas manualmente desde .env\n")
    }
  }, error = function(e) {
    cat("⚠️ Error cargando .env:", e$message, "\n")
  })
} else {
  cat("⚠️ Archivo .env no encontrado. Usando valores por defecto.\n")
}

# Librerías necesarias - Usar namespaces explícitos para evitar conflictos
# library(httr)           # Para llamadas HTTP a la API de Gemini
# library(jsonlite)       # Para procesar respuestas JSON
# library(base64enc)      # Para codificar imágenes en base64
# library(markdown)       # Para renderizar markdown en las explicaciones

cat("✅ gemini_functions.R cargado - Funciones disponibles\n")

################################################################################
# FUNCIÓN DIAGNÓSTICO: Listar modelos disponibles en Gemini API
################################################################################

gemini_list_models <- function(api_key = NULL) {
  # Obtener API key
  if (is.null(api_key) || api_key == "") {
    env_key <- Sys.getenv("GEMINI_API_KEY", unset = "")
    if (env_key != "") {
      api_key <- env_key
    } else {
      api_key <- "GEMINI_API_KEY_REMOVED"
    }
  }

  resultado <- tryCatch({
    # Intentar con v1 endpoint primero
    api_url <- paste0(
      "https://generativelanguage.googleapis.com/v1/models?key=",
      api_key
    )

    response <- httr::GET(api_url)

    if (httr::http_error(response)) {
      error_content <- httr::content(response, as = "text", encoding = "UTF-8")
      cat("⚠️ Error obteniendo modelos (v1):", httr::status_code(response), "\n")
      cat(error_content, "\n")
      return(NULL)
    }

    response_text <- httr::content(response, as = "text", encoding = "UTF-8")
    content <- jsonlite::fromJSON(response_text)

    if (!is.null(content$models)) {
      models_list <- sapply(content$models, function(m) m$name)
      cat("✅ Modelos disponibles en Gemini API:\n")
      for (model in models_list) {
        cat("  -", model, "\n")
      }
      return(models_list)
    } else {
      cat("⚠️ No se encontraron modelos en la respuesta\n")
      return(NULL)
    }
  }, error = function(e) {
    cat("❌ Error:", as.character(e), "\n")
    return(NULL)
  })

  return(resultado)
}

################################################################################
# FUNCIÓN 1: Llamada básica a la API de Gemini
################################################################################

gemini_api_call <- function(image_path, prompt_text, api_key = NULL, model_name = "gemini-1.5-flash") {
  # Obtener API key en este orden de prioridad:
  # 1. Si se proporciona como parámetro
  # 2. Desde variable de entorno GEMINI_API_KEY
  # 3. Valor por defecto de fallback

  if (is.null(api_key) || api_key == "") {
    # Intentar desde variable de entorno
    env_key <- Sys.getenv("GEMINI_API_KEY", unset = "")
    if (env_key != "") {
      api_key <- env_key
      cat("✅ API key cargada desde variables de entorno\n")
    } else {
      # Fallback a valor por defecto
      api_key <- "GEMINI_API_KEY_REMOVED"
      cat("⚠️ Usando API key por defecto (considera usar .env)\n")
    }
  }

  # Validar que el prompt_text no esté vacío
  if (is.null(prompt_text) || prompt_text == "" || nchar(trimws(as.character(prompt_text))) == 0) {
    return("Error: El prompt está vacío. Por favor proporciona una descripción para el análisis.")
  }

  prompt_text <- as.character(prompt_text)  # Asegurar que es character

  # Modelos a intentar en orden de preferencia
  # Basado en modelos realmente disponibles en tu API key (Gemini 2.5 y 2.0)
  modelos_disponibles <- c(
    "gemini-2.5-flash",      # ✅ Modelo más nuevo, excelente balance
    "gemini-2.5-pro",        # ✅ Modelo más poderoso
    "gemini-2.5-flash-lite", # ✅ Versión lite actualizada
    "gemini-2.0-flash",      # ✅ Modelo anterior pero rápido
    "gemini-2.0-flash-001",  # ✅ Versión estable de 2.0
    "gemini-2.0-flash-lite"  # ✅ Versión lite de 2.0
  )

  #  Lista de modelos válidos - intentaremos con cada uno si falla
  for (modelo in modelos_disponibles) {
    resultado <- tryCatch({
      # Llegir i codificar la imatge en base64
      img_raw <- readBin(image_path, "raw", file.info(image_path)$size)
      img_base64 <- base64enc::base64encode(img_raw)

      # URL de l'API de Gemini - usar v1 endpoint
      api_url <- paste0(
        "https://generativelanguage.googleapis.com/v1/models/",
        modelo,
        ":generateContent?key=",
        api_key
      )

      # Preparar el cos de la petició con estructura correcta para Gemini API
      body_json <- list(
        contents = list(
          list(
            parts = list(
              list(text = as.character(prompt_text)),
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
          temperature = 0.4,
          topK = 32,
          topP = 1
        )
      )

      # Fer la crida a l'API - convertir a JSON manualmente para más control
      body_json_str <- jsonlite::toJSON(body_json, auto_unbox = TRUE)

      response <- httr::POST(
        url = api_url,
        body = body_json_str,
        httr::add_headers("Content-Type" = "application/json"),
        encode = "raw"
      )

      # Processar la resposta
      if (httr::http_error(response)) {
        error_content <- httr::content(response, as = "text", encoding = "UTF-8")
        # Si es un 404 (modelo no encontrado), intentar con el siguiente
        if (httr::status_code(response) == 404) {
          if (modelo == modelos_disponibles[length(modelos_disponibles)]) {
            # Si este era el último modelo, devolver el error
            return(paste("Error API Gemini:", httr::status_code(response), "-", error_content))
          } else {
            # Si no, continuar con el siguiente modelo
            return(NULL)
          }
        } else {
          return(paste("Error API Gemini:", httr::status_code(response), "-", error_content))
        }
      }

      # Obtener el texto de la respuesta y parsearlo manualmente con manejo de errores
      response_text <- httr::content(response, as = "text", encoding = "UTF-8")

      # Validar que tenemos texto antes de intentar parsear
      if (is.null(response_text) || nchar(response_text) == 0) {
        return("Error: La resposta de Gemini està buida.")
      }

      # Intentar parsear el JSON con manejo de errores
      content <- tryCatch({
        jsonlite::fromJSON(response_text)
      }, error = function(e) {
        return(paste("Error parseant la resposta de Gemini:", e$message))
      })

      # Si el parsing falló, content será un string de error
      if (is.character(content) && grepl("^Error parseant", content)) {
        return(content)
      }

      # Extreure el text de la resposta
      # Nota: La estructura de Gemini 2.5 es: candidates[].parts[].text
      # (NO tiene el nodo 'content' intermedio)
      if (!is.null(content$candidates) && length(content$candidates) > 0) {
        # Intentar obtener el texto del primer candidato, primer part
        first_candidate <- content$candidates[[1]]

        # Buscar en parts directamente (estructura de Gemini 2.5)
        if (!is.null(first_candidate$parts) && length(first_candidate$parts) > 0) {
          text_response <- first_candidate$parts[[1]]$text

          if (!is.null(text_response) && is.character(text_response)) {
            return(as.character(text_response))
          }
        }
      }

      return("No s'ha pogut obtenir una resposta vàlida de Gemini.")

    }, error = function(e) {
      # Si cualquier cosa falla, retornar NULL para intentar siguiente modelo
      return(NULL)
    })

    # Si obtuvimos una respuesta válida (no NULL), retornarla
    if (!is.null(resultado)) {
      return(resultado)
    }

    # Si no fue un error (NULL), continuar con el siguiente modelo
    if (modelo != modelos_disponibles[length(modelos_disponibles)]) {
      cat("⚠️ Modelo", modelo, "no disponible, intentando con", modelos_disponibles[grep(modelo, modelos_disponibles) + 1], "\n")
    }
  }

  # Si llegamos aquí, nada funcionó
  return("Error: No se pudo conectar con ningún modelo de Gemini disponible")
}

################################################################################
# PROMPT DE CONTEXT PER A GEMINI
################################################################################

gemini_prompt_context <- "Ets un expert en anàlisi d'impacte ambiental i sostenibilitat en la indústria ramadera.
El gràfic que analitzes forma part d'un dashboard que calcula la petjada ambiental de diferents dietes animals.

Context del dashboard:
- Analitza múltiples categories d'impacte ambiental: canvi climàtic (kg CO2 eq), ús del sòl, ús d'aigua,
  eutrofització marina, acidificació i matèria particulada.
- Compara diferents dietes (receptes alimentàries) per animals.
- Considera l'impacte dels ingredients i del seu transport.
- Rastreja l'origen geogràfic dels ingredients.

La teva tasca és:
1. Descriure el que mostra el gràfic de manera clara i concisa.
2. Identificar els patrons més rellevants (dietes amb més/menys impacte, ingredients clau, etc.).
3. Proporcionar insights accionables sobre com millorar la sostenibilitat.
4. Explicar les dades en un llenguatge accessible però tècnicament precís.
5. Si hi ha comparacions entre dietes (Step A vs Step B), destacar les diferències més significatives.

Respon en català i sigues directe i informatiu."

################################################################################
# FUNCIÓN 2: Preparar gráficos para enviar a Gemini
################################################################################

preparar_grafics_per_gemini <- function(llista_plots, context_adicional = "", n_cols = 3, base_height = 5, base_width = 15) {
  # Validar que tenemos gráficos
  if (is.null(llista_plots) || length(llista_plots) == 0) {
    stop("No hi ha gràfics per enviar a Gemini")
  }

  # 1. Calculem el nombre de files necessàries
  n_plots <- length(llista_plots)
  n_rows <- ceiling(n_plots / n_cols)

  # 2. Creem el collage amb ggarrange
  collage <- ggpubr::ggarrange(
    plotlist = llista_plots,
    ncol = n_cols,
    nrow = n_rows,
    common.legend = TRUE,
    legend = "bottom"
  )

  # 3. Crear un fitxer temporal per guardar la imatge
  temp_file <- tempfile(fileext = ".png")

  # 4. Guardar la imatge temporalment
  ggsave(
    filename = temp_file,
    plot = collage,
    width = base_width,
    height = base_height * n_rows,
    units = "in",
    dpi = 300
  )

  # 5. Preparar el prompt complet
  # Asegurar que el contexto adicional no esté vacío
  if (is.null(context_adicional) || context_adicional == "") {
    context_adicional <- "Analitza i descriu els gràfics mostrats amb detall."
  }

  prompt_complete <- paste0(
    gemini_prompt_context,
    "\n\n",
    context_adicional,
    "\n\n",
    "Analitza els gràfics proporcionats i proporciona un análisis detallat."
  )

  # 6. Enviar a Gemini
  resposta <- gemini_api_call(temp_file, prompt_complete)

  # 7. Eliminar el fitxer temporal
  unlink(temp_file)

  return(resposta)
}

################################################################################
# FUNCIÓN 3: Función específica per explicar gràfics amb context
################################################################################

gemini_explicar_grafic <- function(llista_plots, tipus_grafic = "general", detalls_adicionals = NULL) {
  # Context específic segons el tipus de gràfic
  context_map <- list(
    composicio = "Aquest gràfic mostra la composició percentual dels ingredients en cada dieta.
                  Cada barra representa una dieta i els colors representen diferents ingredients.",

    origen = "Aquest gràfic mostra la contribució de cada país d'origen als impactes ambientals totals.
              Permet identificar quins orígens geogràfics tenen major impacte.",

    top_ingredients = "Aquest gràfic mostra els ingredients amb major impacte ambiental en cada categoria d'impacte.
                       Identifica quins ingredients són els principals contribuents a cada tipus d'impacte.",

    mapa = "Aquest mapa mostra la distribució geogràfica dels orígens dels ingredients utilitzats en les dietes.
            Els colors o la intensitat indiquen els impactes o la quantitat d'ingredients procedents de cada país.",

    distribucio = "Aquest gràfic mostra la distribució estadística (boxplot) dels impactes ambientals entre les diferents dietes.
                   Permet comparar la variabilitat i els valors típics entre dietes.",

    diferencies_ab = "Aquest gràfic mostra les diferències d'impacte entre dos escenaris (Step A i Step B).
                      Els valors positius indiquen un augment d'impacte en Step B, els negatius una reducció.",

    desglossament = "Aquest gràfic desglossa l'impacte total en dues components: l'impacte dels ingredients
                     (producció) i l'impacte del transport. Permet veure quina part és més significativa.",

    general = "Aquest és un gràfic d'impacte ambiental de dietes animals."
  )

  # Seleccionar el context adequat
  context <- context_map[[tipus_grafic]]
  if (is.null(context)) {
    context <- context_map[["general"]]
  }

  # Afegir detalls addicionals si n'hi ha
  if (!is.null(detalls_adicionals)) {
    context <- paste(context, "\n\nInformació addicional:", detalls_adicionals)
  }

  # Cridar la funció general
  preparar_grafics_per_gemini(llista_plots, context_adicional = context)
}
