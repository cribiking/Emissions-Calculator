################################################################################
# INTEGRACIÓN CON GOOGLE GEMINI PARA ANÁLISIS DE GRÁFICOS
# Funciones para enviar imágenes a Gemini y obtener explicaciones
################################################################################

# Librerías necesarias - Usar namespaces explícitos para evitar conflictos
# library(httr)           # Para llamadas HTTP a la API de Gemini
# library(jsonlite)       # Para procesar respuestas JSON
# library(base64enc)      # Para codificar imágenes en base64
# library(markdown)       # Para renderizar markdown en las explicaciones

cat("✅ gemini_functions.R cargado - Funciones disponibles\n")

################################################################################
# FUNCIÓN 1: Llamada básica a la API de Gemini
################################################################################

gemini_api_call <- function(image_path, prompt_text, api_key = NULL) {
  # Obtenir API key desde variable d'entorn si no es proporciona
  if (is.null(api_key)) {
    api_key <- Sys.getenv("GEMINI_API_KEY")
    if (api_key == "") {
      return("Error: No s'ha trobat la clau API de Gemini. Configura GEMINI_API_KEY.")
    }
  }

  # Envolver toda la función en tryCatch para máxima seguridad
  resultado <- tryCatch({
    # Llegir i codificar la imatge en base64
    img_raw <- readBin(image_path, "raw", file.info(image_path)$size)
    img_base64 <- base64enc::base64encode(img_raw)

    # URL de l'API de Gemini
    api_url <- paste0(
      "https://generativelanguage.googleapis.com/v1beta/models/gemini-1.5-pro-latest:generateContent?key=",
      api_key
    )

    # Preparar el cos de la petició
    body_json <- list(
      contents = list(
        list(
          parts = list(
            list(text = prompt_text),
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
        topP = 1,
        maxOutputTokens = 2048
      )
    )

    # Fer la crida a l'API
    response <- httr::POST(
      url = api_url,
      body = jsonlite::toJSON(body_json, auto_unbox = TRUE),
      httr::add_headers("Content-Type" = "application/json"),
      encode = "json"
    )

    # Processar la resposta
    if (httr::http_error(response)) {
      error_content <- httr::content(response, as = "text", encoding = "UTF-8")
      return(paste("Error API Gemini:", httr::status_code(response), "-", error_content))
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
    if (!is.null(content$candidates) && length(content$candidates) > 0) {
      if (!is.null(content$candidates[[1]]$content$parts[[1]]$text)) {
        text_response <- content$candidates[[1]]$content$parts[[1]]$text
        # Garantir que es un character válido
        if (is.character(text_response)) {
          return(as.character(text_response))
        }
      }
    }

    return("No s'ha pogut obtenir una resposta vàlida de Gemini.")

  }, error = function(e) {
    # Si cualquier cosa falla, retornar mensaje de error
    return(paste("Error:", as.character(e)))
  })

  # Garantir que SIEMPRE devolvemos un character
  if (!is.character(resultado)) {
    resultado <- "Error: resposta no és character"
  }

  return(resultado)
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
  prompt_complete <- paste(gemini_prompt_context, "\n\n", context_adicional, "\n\nAnalitza aquest gràfic:")

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
