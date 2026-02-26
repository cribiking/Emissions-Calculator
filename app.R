# app.R (versió amb reassignació d'origen per ingredient + transport impacts + mapa)
library(shiny)
library(tidyverse)
library(readxl)
library(janitor)
library(DT)
library(plotly)
library(leaflet)

# ---------------------------
# Helpers i utilitats
# ---------------------------

carrega_dades_ambientals <- function(path) {
  df <- read_excel(path) %>% clean_names()
  # Acceptem que hi hagi múltiples files per mateix ingredient amb orígens alternatius.
  expect_cols <- c("ingredient", "group", "origen", "default_origen")
  # a més, busquem les columnes d'impacte (acceptem subset)
  impact_cols <- intersect(c("climate_change", "land_use", "water_use",
                             "eutrophication", "acidification", "particulate_matter"),
                           names(df))
  missing_cols <- setdiff(expect_cols, names(df))
  if(length(missing_cols) > 0) {
    stop("Falten columnes obligatòries a dades mediambientals: ", paste(missing_cols, collapse = ", "))
  }
  df %>% select(all_of(c(expect_cols, impact_cols)))
}

carrega_dades_dietes <- function(path) {
  df <- read_excel(path) %>% clean_names() %>%
    mutate(prop = as.numeric(prop))
  req_cols <- c("step", "ingredient", "diet", "prop")
  missing_cols <- setdiff(req_cols, names(df))
  if(length(missing_cols) > 0) {
    stop("Falten columnes a dades de dietes: ", paste(missing_cols, collapse = ", "))
  }
  df
}

carrega_transport <- function(path) {
  df <- read_excel(path) %>% clean_names()
  # s'espera col 'origen' i idealment les mateixes categories d'impacte + lat/lon opcional
  if(!"origen" %in% names(df)) stop("El fitxer de transport ha de contenir la columna 'origen'")
  df
}

# ---------------------------
# Funcions utilitàries per selecció d'origen d'un ingredient
# ---------------------------
# Obté la fila "default" d'un ingredient (default_origen == 1) si existeix
get_default_row_for_ingredient <- function(dades_env_df, ingredient_name) {
  df <- dades_env_df %>% filter(ingredient == ingredient_name)
  if(nrow(df) == 0) return(NULL)
  # preferim les files amb default_origen == 1
  if("default_origen" %in% names(df)) {
    def <- df %>% filter(default_origen == 1)
    if(nrow(def) >= 1) return(def[1, ])
  }
  # si no hi ha default, retornem la primera fila (primer origen llistat)
  df[1, ]
}

# Retorna vector d'orígens disponibles per un ingredient basat en dades_env
origens_per_ingredient <- function(dades_env_df, ingredient_name) {
  dados <- dades_env_df %>% filter(ingredient == ingredient_name)
  unique(dados$origen)
}

# Aplica overrides (ingredient -> origen seleccionat) sobre les dades ambientals:
# Retorna una taula amb, per cada ingredient i origen utilitzat, les columnes d'impacte
# Lògica: per a cada ingredient a usar, busquem la fila dades_env amb ingredient+origin_override.
# Si no existeix, fem servir la fila default (default_origen==1) i avisem.
apply_overrides_to_env <- function(dades_env_df, overrides_df) {
  # overrides_df: tibble(ingredient, origen_selected)
  # Per a cada override, busquem la fila corresponent i la retornem.
  out_rows <- list()
  msgs <- character()
  for(i in seq_len(nrow(overrides_df))) {
    ing <- overrides_df$ingredient[i]
    ori_sel <- overrides_df$origen_selected[i]
    # intentem trobar ingredient+ori_sel
    row_match <- dades_env_df %>% filter(ingredient == ing, origen == ori_sel)
    if(nrow(row_match) >= 1) {
      out_rows[[length(out_rows) + 1]] <- row_match[1, ]
    } else {
      # fallback a default
      defrow <- get_default_row_for_ingredient(dades_env_df, ing)
      if(!is.null(defrow)) {
        out_rows[[length(out_rows) + 1]] <- defrow
        msgs <- c(msgs, paste0("Per ingredient '", ing, "' no s'ha trobat l'origen '", ori_sel, "'. S'utilitza 'default'."))
      } else {
        msgs <- c(msgs, paste0("Per ingredient '", ing, "' no s'han trobat dades ambientals."))
      }
    }
  }
  out_df <- bind_rows(out_rows)
  list(df = out_df, msgs = msgs)
}

# ---------------------------
# Càlculs: integra transport impacts amb la fila d'ingredient seleccionada
# ---------------------------
# Aquesta funció construeix la taula d'ingredients "active" per una solució (step)
# Tenim en compte:
# - les files de dades_dietes per al step,
# - per a cada ingredient, triem la fila de dades_env segons override (si hi ha),
# - multipliquem impactes per prop,
# - sumem impactes de transport segons origen (si transport_df present)
calcula_solucio_amb_transport <- function(dietes_df, dades_env_df, step_sel, overrides_df = NULL,
                                          transport_df = NULL, ordre_dietes = NULL) {
  # Obtenim les files de dietes per step
  dietes <- dietes_df %>% filter(step == step_sel)
  if(nrow(dietes) == 0) return(tibble())
  # Determineix orígens efectius per cada ingredient:
  ing_list <- unique(dietes$ingredient)
  # Construïm una taula efetiva d'ingredients amb l'origen seleccionat o default
  if(is.null(overrides_df) || nrow(overrides_df) == 0) {
    # use default rows
    effective_rows <- map_dfr(ing_list, function(ing) {
      row <- get_default_row_for_ingredient(dades_env_df, ing)
      if(is.null(row)) {
        # crear fila amb NA
        tibble(ingredient = ing, group = NA, origen = NA, default_origen = NA)
      } else {
        row
      }
    })
  } else {
    # apply overrides
    res <- apply_overrides_to_env(dades_env_df, overrides_df)
    effective_rows <- res$df
    # avisos no es retornaran aquí (poden ser capturats si cal)
  }
  # effective_rows té una fila per ingredient amb origen i impactes base
  # Ara fem join amb dietes per obtenir prop i calcular impacte per kg pinso
  # Ens assegurem que tenim totes les columnes d'impacte presents:
  impact_cols <- intersect(c("climate_change", "land_use", "water_use",
                             "eutrophication", "acidification", "particulate_matter"),
                           names(effective_rows))
  
  # Si transport_df present, volem sumar impactes de transport per origen
  if(!is.null(transport_df)) {
    # prepare transport_df amb col names coincidents; si falten columnes d'impacte les ignorem
    transport_cols <- intersect(names(transport_df), impact_cols)
    # join per origen -> afegim les columnes de transport prefixades
    trans_sel <- transport_df %>% select(origen, all_of(transport_cols), lat = any_of("lat"), lon = any_of("lon"))
    effective_rows <- effective_rows %>% left_join(trans_sel, by = c("origen" = "origen"))
    # renombrem les columnes de transport perquè no sobreescriguin
    for(col in transport_cols) {
      if(col %in% names(effective_rows) && paste0(col, ".y") %in% names(effective_rows)) {
        # evitarem confusions: preferim col.x (ingredient base) + transport col (transport_)
        effective_rows <- effective_rows %>%
          mutate(!!paste0("transport_", col) := .data[[paste0(col, ".y")]]) %>%
          select(-all_of(paste0(col, ".y")))
      } else if(col %in% names(effective_rows) && !(paste0("transport_", col) %in% names(effective_rows))) {
        # if transport column was directly joined without name change, create transport_***
        if(col %in% names(effective_rows)) {
          effective_rows <- effective_rows %>%
            mutate(!!paste0("transport_", col) := .data[[col]])
        }
      }
    }
    # Now ensure transport_ columns exist (fill NA with 0)
    for(col in transport_cols) {
      tcol <- paste0("transport_", col)
      if(!tcol %in% names(effective_rows)) effective_rows[[tcol]] <- 0
      effective_rows[[tcol]][is.na(effective_rows[[tcol]])] <- 0
    }
  } else {
    # if no transport provided, add zero transport cols for consistency
    for(col in impact_cols) {
      effective_rows[[paste0("transport_", col)]] <- 0
    }
  }
  
  # Now join dietes (prop) with effective_rows by ingredient
  joined <- dietes %>% left_join(effective_rows, by = c("ingredient" = "ingredient"))
  # If lat/lon present in effective_rows (from transport_df), they will propagate
  # Compute per-ingredient impact per kg pinso: base_impact + transport_impact, then * prop
  # For columns present in impact_cols, create combined column = base + transport
  for(col in impact_cols) {
    tcol <- paste0("transport_", col)
    if(tcol %in% names(joined)) {
      joined[[paste0("impact_", col)]] <- (coalesce(joined[[col]], 0) + coalesce(joined[[tcol]], 0))
    } else {
      joined[[paste0("impact_", col)]] <- coalesce(joined[[col]], 0)
    }
    # multiply per prop to get per-kg contribution
    joined[[paste0("contrib_", col)]] <- joined[[paste0("impact_", col)]] * coalesce(joined$prop, 0)
  }
  
  # arrange by diet order if ordre_dietes given
  if(!is.null(ordre_dietes)) {
    joined <- joined %>% mutate(diet = factor(diet, levels = ordre_dietes)) %>% arrange(diet)
  } else {
    joined <- joined %>% arrange(diet)
  }
  
  joined
}

# Resum per dieta (suma de contrib_* cols)
resum_per_dieta_from_joined <- function(joined_df, per_animal = FALSE, kg_table = NULL) {
  # trobem quines cols contrib_*
  contrib_cols <- names(joined_df)[str_detect(names(joined_df), "^contrib_")]
  if(length(contrib_cols) == 0) return(tibble())
  # sum per diet
  res <- joined_df %>%
    group_by(diet) %>%
    summarise(across(all_of(contrib_cols), sum, na.rm = TRUE), .groups = "drop")
  # si per_animal multipliquem per kg_table
  if(per_animal) {
    kg_tbl <- kg_table %>% distinct(diet, kg_consum)
    res <- res %>% left_join(kg_tbl, by = "diet") %>%
      mutate(across(starts_with("contrib_"), ~ .x * kg_consum)) %>%
      select(-kg_consum)
  }
  # pivotar a impacte, valor
  res_long <- res %>%
    pivot_longer(-diet, names_to = "var", values_to = "valor") %>%
    mutate(impacte = str_remove(var, "^contrib_")) %>%
    select(diet, impacte, valor)
  res_long
}

# Contribució per origen (ja tenim origen_region en joined_df)
contribucio_per_origen_from_joined <- function(joined_df, per_animal = FALSE, kg_table = NULL) {
  contrib_cols <- names(joined_df)[str_detect(names(joined_df), "^contrib_")]
  if(length(contrib_cols) == 0) return(tibble())
  res <- joined_df %>%
    group_by(diet, origen) %>%
    summarise(across(all_of(contrib_cols), sum, na.rm = TRUE), .groups = "drop")
  if(per_animal) {
    kg_tbl <- kg_table %>% distinct(diet, kg_consum)
    res <- res %>% left_join(kg_tbl, by = "diet") %>%
      mutate(across(starts_with("contrib_"), ~ .x * kg_consum)) %>%
      select(-kg_consum)
  }
  res_long <- res %>%
    pivot_longer(-c(diet, origen), names_to = "var", values_to = "valor") %>%
    mutate(impacte = str_remove(var, "^contrib_")) %>%
    select(diet, origen, impacte, valor)
  res_long
}

# Contrib per ingredient (per dieta) ja disponible a joined_df
contribucio_per_ingredient_from_joined <- function(joined_df, per_animal = FALSE, kg_table = NULL) {
  contrib_cols <- names(joined_df)[str_detect(names(joined_df), "^contrib_")]
  df <- joined_df %>%
    select(diet, ingredient, all_of(contrib_cols), prop)
  if(per_animal) {
    kg_tbl <- kg_table %>% distinct(diet, kg_consum)
    df <- df %>% left_join(kg_tbl, by = "diet") %>%
      mutate(across(starts_with("contrib_"), ~ .x * kg_consum)) %>%
      select(-kg_consum)
  }
  df_long <- df %>%
    pivot_longer(cols = starts_with("contrib_"), names_to = "var", values_to = "valor") %>%
    mutate(impacte = str_remove(var, "^contrib_")) %>%
    select(diet, ingredient, impacte, prop, valor)
  df_long
}

# ---------------------------
# Plots i mapa
# ---------------------------

# Reutilitzem la composició simple (prop per ingredient per dieta)
plot_composicio <- function(joined_df, ordre_dietes = NULL) {
  df <- joined_df %>% group_by(diet, ingredient) %>% summarise(prop = sum(prop, na.rm = TRUE), .groups = "drop")
  if(!is.null(ordre_dietes)) df <- df %>% mutate(diet = factor(diet, levels = ordre_dietes))
  p <- df %>%
    ggplot(aes(x = diet, y = prop, fill = ingredient)) +
    geom_col() +
    coord_flip() +
    labs(title = "Composició de les dietes (per ingredient)", y = "Proporció", x = "Dieta") +
    theme_minimal() +
    theme(legend.position = "none")
  ggplotly(p, tooltip = c("y", "fill"))
}

# Plot impacte A vs B (resum format long)
plot_impacte_A_vs_B_generic <- function(resA_long, resB_long, impactes_sel = NULL, per_animal = FALSE) {
  A <- resA_long %>% mutate(solucio = "A")
  B <- resB_long %>% mutate(solucio = "B")
  both <- bind_rows(A, B)
  if(!is.null(impactes_sel)) both <- both %>% filter(impacte %in% impactes_sel)
  p <- both %>%
    ggplot(aes(x = diet, y = valor, fill = solucio)) +
    geom_col(position = position_dodge(width = 0.9)) +
    facet_wrap(~ impacte, scales = "free_y", ncol = 1) +
    coord_flip() +
    labs(title = ifelse(per_animal, "Comparació A vs B per dieta (per animal)", "Comparació A vs B per dieta (per kg pinso)"),
         y = "Valor", x = "Dieta") +
    theme_minimal()
  ggplotly(p)
}

# Plot origen per dieta
plot_origen_per_dieta_from_joined <- function(joined_df, impactes_sel = NULL, per_animal = FALSE, ordre_dietes = NULL) {
  df <- contribucio_per_origen_from_joined(joined_df, per_animal = per_animal)
  if(!is.null(impactes_sel)) df <- df %>% filter(impacte %in% impactes_sel)
  if(!is.null(ordre_dietes)) df <- df %>% mutate(diet = factor(diet, levels = ordre_dietes))
  p <- df %>%
    ggplot(aes(x = diet, y = valor, fill = origen)) +
    geom_col() +
    facet_wrap(~ impacte, scales = "free_y", ncol = 1) +
    coord_flip() +
    labs(title = ifelse(per_animal, "Contribució per origen (per animal)", "Contribució per origen (per kg pinso)"),
         y = "Valor", x = "Dieta") +
    theme_minimal()
  ggplotly(p)
}

# Top N ingredients
plot_topN_ingredients_from_joined <- function(joined_df, impacte_sel = "climate_change", n = 5, per_animal = FALSE) {
  df <- contribucio_per_ingredient_from_joined(joined_df, per_animal = per_animal)
  df_sel <- df %>% filter(impacte == impacte_sel)
  df_top <- df_sel %>% group_by(diet) %>% slice_max(order_by = valor, n = n, with_ties = FALSE) %>% ungroup()
  p <- df_top %>%
    ggplot(aes(x = reorder_within(ingredient, valor, diet), y = valor, fill = diet)) +
    geom_col() +
    facet_wrap(~ diet, scales = "free_y") +
    coord_flip() +
    labs(title = paste0("Top ", n, " ingredients per dieta (impacte: ", impacte_sel, ")"), y = "Valor") +
    theme_minimal() +
    scale_x_reordered()
  ggplotly(p)
}

# Mapa: punts per origen amb lat/lon
plot_map_origens <- function(joinedA, joinedB = NULL, transport_df = NULL) {
  # Construïm suma d'ingredients per origen per solució
  summarise_points <- function(joined) {
    if(nrow(joined) == 0) return(tibble())
    out <- joined %>% distinct(ingredient, origen, .keep_all = TRUE) %>%
      group_by(origen) %>% summarise(n_ingredients = n(), .groups = "drop") %>%
      left_join(transport_df %>% select(origen, lat = any_of("lat"), lon = any_of("lon")), by = "origen")
    out
  }
  A_pts <- summarise_points(joinedA)
  B_pts <- if(!is.null(joinedB)) summarise_points(joinedB) else tibble()
  
  # create leaflet map
  m <- leaflet() %>% addTiles()
  if(nrow(A_pts) > 0 && "lat" %in% names(A_pts) && "lon" %in% names(A_pts)) {
    m <- m %>% addCircleMarkers(data = A_pts, lng = ~lon, lat = ~lat,
                                radius = ~pmax(4, log1p(n_ingredients) * 4),
                                color = "blue", group = "Solució A",
                                label = ~paste0(origen, " (A) : ", n_ingredients, " ingredients"))
  }
  if(nrow(B_pts) > 0 && "lat" %in% names(B_pts) && "lon" %in% names(B_pts)) {
    m <- m %>% addCircleMarkers(data = B_pts, lng = ~lon, lat = ~lat,
                                radius = ~pmax(4, log1p(n_ingredients) * 4),
                                color = "red", group = "Solució B",
                                label = ~paste0(origen, " (B) : ", n_ingredients, " ingredients"))
  }
  if(nrow(A_pts) > 0 || nrow(B_pts) > 0) {
    m <- m %>% addLayersControl(overlayGroups = c("Solució A", "Solució B"), options = layersControlOptions(collapsed = FALSE))
  }
  m
}

# utilitats reorder_within
reorder_within <- function(x, by, within, fun = median, sep = "___") {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}
scale_x_reordered <- function(...) {
  scale_x_discrete(labels = function(x) gsub("___.*$", "", x))
}

# ---------------------------
# UI
# ---------------------------
ui <- fluidPage(
  titlePanel("Dashboard Impactes Ambientals — Solució A vs Solució B (multi-dietes)"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file_env", "Dades ambientals (dades_mediambientals.xlsx)", accept = c(".xlsx")),
      fileInput("file_diets", "Dades dietes (ingredients_Used_xDiet.xlsx)", accept = c(".xlsx")),
      fileInput("file_transport", "Fitxer transport (transport_impactes.xlsx) - opcional", accept = c(".xlsx")),
      hr(),
      uiOutput("steps_ui"),
      hr(),
      checkboxGroupInput("impactes_sel", "Impactes a mostrar",
                         choices = c("climate_change", "land_use", "water_use",
                                     "eutrophication", "acidification", "particulate_matter"),
                         selected = c("climate_change", "land_use", "particulate_matter")),
      checkboxInput("mostrar_per_animal", "Mostrar també per kg que alimenta 1 animal tipus", value = FALSE),
      conditionalPanel(
        condition = "input.mostrar_per_animal == true",
        helpText("A continuació edita els kg consumits per dieta (taula editable). Per defecte: 1 kg."),
        DTOutput("tbl_kg_edit")
      ),
      hr(),
      h4("Reassignar orígens per ingredient"),
      helpText("Tria un ingredient i escull, del desplegable, un origen disponible per a aquest ingredient."),
      selectInput("sel_ingredient", "Ingredient", choices = NULL),
      uiOutput("sel_origen_ui"),
      actionButton("apply_override", "Aplica override per ingredient"),
      br(), br(),
      actionButton("reset_overrides", "Reset all overrides"),
      hr(),
      downloadButton("download_summary", "Descarrega resum (CSV)")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Visió general",
                 h4("Resum de dades carregades"),
                 DTOutput("tbl_overview"),
                 hr(),
                 h4("Llistat de dietes per cada solució"),
                 fluidRow(
                   column(6, h5("Solució A (step seleccionat)"), DTOutput("tbl_dietes_A")),
                   column(6, h5("Solució B (step seleccionat)"), DTOutput("tbl_dietes_B"))
                 )
        ),
        tabPanel("Composició per dieta",
                 h4("Solució A"),
                 plotlyOutput("plot_comp_A", height = "600px"),
                 h4("Solució B"),
                 plotlyOutput("plot_comp_B", height = "600px")
        ),
        tabPanel("Impactes per dieta",
                 plotlyOutput("plot_impacte_A_vs_B", height = "900px")
        ),
        tabPanel("Contribució per origen",
                 h5("Solució A"),
                 plotlyOutput("plot_origen_A", height = "600px"),
                 h5("Solució B"),
                 plotlyOutput("plot_origen_B", height = "600px")
        ),
        tabPanel("Top ingredients per dieta",
                 selectInput("impacte_top", "Tria impacte per top ingredients:",
                             choices = c("climate_change", "land_use", "water_use",
                                         "eutrophication", "acidification", "particulate_matter"),
                             selected = "climate_change"),
                 plotlyOutput("plot_topA", height = "600px"),
                 plotlyOutput("plot_topB", height = "600px")
        ),
        tabPanel("Mapa d'orígens",
                 helpText("Mapa amb punts per origen segons orígens utilitzats per les solucions."),
                 leafletOutput("map_origens", height = "600px")
        ),
        tabPanel("Distribució per solució",
                 plotlyOutput("plot_box", height = "700px")
        ),
        tabPanel("Diferència A - B per dieta",
                 plotlyOutput("plot_diff", height = "700px")
        )
      )
    )
  )
)

# ---------------------------
# Server
# ---------------------------
server <- function(input, output, session) {
  
  # Carrega fitxers
  dades_env <- reactive({
    req(input$file_env)
    tryCatch({
      carrega_dades_ambientals(input$file_env$datapath)
    }, error = function(e) {
      showNotification(paste("Error fitxer ambientals:", e$message), type = "error")
      NULL
    })
  })
  
  dades_dietes <- reactive({
    req(input$file_diets)
    tryCatch({
      df <- carrega_dades_dietes(input$file_diets$datapath)
      # mantenim ordre d'aparició de dietes
      ordre <- df %>% distinct(diet) %>% pull(diet)
      df <- df %>% mutate(diet = factor(diet, levels = ordre))
      df
    }, error = function(e) {
      showNotification(paste("Error fitxer dietes:", e$message), type = "error")
      NULL
    })
  })
  
  transport_df <- reactive({
    if(is.null(input$file_transport)) return(NULL)
    tryCatch({
      carrega_transport(input$file_transport$datapath)
    }, error = function(e) {
      showNotification(paste("Error fitxer transport:", e$message), type = "error")
      NULL
    })
  })
  
  # UI per steps
  output$steps_ui <- renderUI({
    df <- dades_dietes()
    req(df)
    steps <- sort(unique(df$step))
    tagList(
      selectInput("stepA", "Step per Solució A", choices = steps, selected = steps[1]),
      selectInput("stepB", "Step per Solució B", choices = steps, selected = steps[min(2, length(steps))])
    )
  })
  
  # overview
  output$tbl_overview <- renderDT({
    env <- dades_env(); diets <- dades_dietes()
    req(env, diets)
    tibble(
      n_ingredients = length(unique(env$ingredient)),
      n_origens = length(unique(env$origen)),
      n_steps = length(unique(diets$step)),
      n_dietes = length(unique(diets$diet))
    ) %>% datatable()
  })
  
  # ordre de dietes (segons arxiu)
  ordre_dietes <- reactive({
    df <- dades_dietes()
    req(df)
    df %>% distinct(diet) %>% pull(diet)
  })
  
  # solucions: recalc segons overrides i transport
  # mantenim una taula reactiva d'overrides: tibble(ingredient, origen_selected)
  overrides <- reactiveVal(tibble(ingredient = character(0), origen_selected = character(0)))
  
  # Select ingredient UI options
  observe({
    env <- dades_env()
    req(env)
    ingredients <- unique(env$ingredient)
    updateSelectInput(session, "sel_ingredient", choices = ingredients)
  })
  
  # UI per seleccionar origen per l'ingredient seleccionat
  output$sel_origen_ui <- renderUI({
    req(input$sel_ingredient, dades_env())
    env <- dades_env()
    oris <- origens_per_ingredient(env, input$sel_ingredient)
    if(length(oris) == 0) {
      selectInput("sel_origen", "Orígens disponibles", choices = c("(cap)"), selected = "(cap)")
    } else {
      selectInput("sel_origen", "Orígens disponibles", choices = oris, selected = oris[1])
    }
  })
  
  # Aplica override quan s'apreta el botó
  observeEvent(input$apply_override, {
    req(input$sel_ingredient, input$sel_origen)
    tbl <- overrides()
    # si existeix ja, actualitza; si no, afegeix
    if(input$sel_ingredient %in% tbl$ingredient) {
      tbl$origen_selected[tbl$ingredient == input$sel_ingredient] <- input$sel_origen
    } else {
      tbl <- bind_rows(tbl, tibble(ingredient = input$sel_ingredient, origen_selected = input$sel_origen))
    }
    overrides(tbl)
    showNotification(paste0("Override aplicat: ", input$sel_ingredient, " -> ", input$sel_origen), type = "message")
  })
  
  # Reset overrides
  observeEvent(input$reset_overrides, { overrides(tibble(ingredient = character(0), origen_selected = character(0))); showNotification("Overrides reiniciats", type = "message") })
  
  # solucions calculades (joined) amb transport i overrides
  solA_joined <- reactive({
    req(dades_env(), dades_dietes(), input$stepA)
    calculate <- calcula_solucio_amb_transport(dades_dietes(), dades_env(), input$stepA,
                                               overrides_df = overrides(), transport_df = transport_df(),
                                               ordre_dietes = ordre_dietes())
    validate(need(nrow(calculate) > 0, "Solució A (step) no té dades o hi ha un error"))
    calculate
  })
  
  solB_joined <- reactive({
    req(dades_env(), dades_dietes(), input$stepB)
    calculate <- calcula_solucio_amb_transport(dades_dietes(), dades_env(), input$stepB,
                                               overrides_df = overrides(), transport_df = transport_df(),
                                               ordre_dietes = ordre_dietes())
    validate(need(nrow(calculate) > 0, "Solució B (step) no té dades o hi ha un error"))
    calculate
  })
  
  output$tbl_dietes_A <- renderDT({ solA_joined() %>% distinct(diet) %>% datatable(options = list(pageLength = 10)) })
  output$tbl_dietes_B <- renderDT({ solB_joined() %>% distinct(diet) %>% datatable(options = list(pageLength = 10)) })
  
  # Taula editable per kg per dieta (inicialitzada amb 1 per dieta)
  kg_table <- reactiveVal(NULL)
  observeEvent(ordre_dietes(), {
    ordre <- ordre_dietes()
    tbl <- tibble(diet = ordre, kg_consum = rep(1, length(ordre)))
    kg_table(tbl)
  }, once = FALSE)
  
  output$tbl_kg_edit <- renderDT({
    req(kg_table())
    datatable(kg_table(), editable = list(target = "cell", disable = list(columns = c(0))), options = list(dom = 't'))
  })
  
  observeEvent(input$tbl_kg_edit_cell_edit, {
    info <- input$tbl_kg_edit_cell_edit
    i <- info$row; j <- info$col; v <- info$value
    tbl <- kg_table()
    vnum <- suppressWarnings(as.numeric(v))
    if(is.na(vnum)) {
      showNotification("Valor no vàlid per kg. Introduïu un número.", type = "error")
      return()
    }
    tbl$kg_consum[i] <- vnum
    kg_table(tbl)
  })
  
  # Resum per dieta (per kg o per animal segons checkbox)
  resumA_kg <- reactive({ resum_per_dieta_from_joined(solA_joined(), per_animal = FALSE) })
  resumB_kg <- reactive({ resum_per_dieta_from_joined(solB_joined(), per_animal = FALSE) })
  resumA_animal <- reactive({ resum_per_dieta_from_joined(solA_joined(), per_animal = TRUE, kg_table = kg_table()) })
  resumB_animal <- reactive({ resum_per_dieta_from_joined(solB_joined(), per_animal = TRUE, kg_table = kg_table()) })
  
  # Plots
  output$plot_comp_A <- renderPlotly({ plot_composicio(solA_joined(), ordre_dietes = ordre_dietes()) })
  output$plot_comp_B <- renderPlotly({ plot_composicio(solB_joined(), ordre_dietes = ordre_dietes()) })
  
  output$plot_impacte_A_vs_B <- renderPlotly({
    if(input$mostrar_per_animal) {
      plot_impacte_A_vs_B_generic(resumA_animal(), resumB_animal(), impactes_sel = input$impactes_sel, per_animal = TRUE)
    } else {
      plot_impacte_A_vs_B_generic(resumA_kg(), resumB_kg(), impactes_sel = input$impactes_sel, per_animal = FALSE)
    }
  })
  
  output$plot_origen_A <- renderPlotly({
    if(input$mostrar_per_animal) {
      plot_origen_per_dieta_from_joined(solA_joined(), impactes_sel = input$impactes_sel, per_animal = TRUE, ordre_dietes = ordre_dietes())
    } else {
      plot_origen_per_dieta_from_joined(solA_joined(), impactes_sel = input$impactes_sel, per_animal = FALSE, ordre_dietes = ordre_dietes())
    }
  })
  output$plot_origen_B <- renderPlotly({
    if(input$mostrar_per_animal) {
      plot_origen_per_dieta_from_joined(solB_joined(), impactes_sel = input$impactes_sel, per_animal = TRUE, ordre_dietes = ordre_dietes())
    } else {
      plot_origen_per_dieta_from_joined(solB_joined(), impactes_sel = input$impactes_sel, per_animal = FALSE, ordre_dietes = ordre_dietes())
    }
  })
  
  output$plot_topA <- renderPlotly({
    if(input$mostrar_per_animal) {
      plot_topN_ingredients_from_joined(solA_joined(), impacte_sel = input$impacte_top, n = 5, per_animal = TRUE, kg_table = kg_table())
    } else {
      plot_topN_ingredients_from_joined(solA_joined(), impacte_sel = input$impacte_top, n = 5, per_animal = FALSE)
    }
  })
  output$plot_topB <- renderPlotly({
    if(input$mostrar_per_animal) {
      plot_topN_ingredients_from_joined(solB_joined(), impacte_sel = input$impacte_top, n = 5, per_animal = TRUE, kg_table = kg_table())
    } else {
      plot_topN_ingredients_from_joined(solB_joined(), impacte_sel = input$impacte_top, n = 5, per_animal = FALSE)
    }
  })
  
  output$plot_box <- renderPlotly({
    if(input$mostrar_per_animal) {
      dfA <- resumA_animal() %>% mutate(solucio = "A")
      dfB <- resumB_animal() %>% mutate(solucio = "B")
    } else {
      dfA <- resumA_kg() %>% mutate(solucio = "A")
      dfB <- resumB_kg() %>% mutate(solucio = "B")
    }
    both <- bind_rows(dfA, dfB) %>% filter(impacte %in% input$impactes_sel)
    p <- both %>%
      ggplot(aes(x = solucio, y = valor, fill = solucio)) +
      geom_boxplot() +
      facet_wrap(~ impacte, scales = "free_y", ncol = 1) +
      labs(title = "Distribució d'impactes per solució", y = "Valor") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$plot_diff <- renderPlotly({
    if(input$mostrar_per_animal) {
      A <- resumA_animal() %>% pivot_wider(names_from = impacte, values_from = valor) %>% mutate(solucio = "A")
      B <- resumB_animal() %>% pivot_wider(names_from = impacte, values_from = valor) %>% mutate(solucio = "B")
    } else {
      A <- resumA_kg() %>% pivot_wider(names_from = impacte, values_from = valor) %>% mutate(solucio = "A")
      B <- resumB_kg() %>% pivot_wider(names_from = impacte, values_from = valor) %>% mutate(solucio = "B")
    }
    both_wide <- full_join(A, B, by = "diet", suffix = c("_A", "_B"))
    impactes <- input$impactes_sel
    diff_df <- map_dfr(impactes, function(imp) {
      tibble(
        diet = both_wide$diet,
        impacte = imp,
        valA = both_wide[[paste0(imp, "_A")]],
        valB = both_wide[[paste0(imp, "_B")]],
        diff = both_wide[[paste0(imp, "_A")]] - both_wide[[paste0(imp, "_B")]]
      )
    })
    p <- diff_df %>%
      mutate(sign = ifelse(is.na(diff), "NA", ifelse(diff >= 0, "A pitjor", "B pitjor"))) %>%
      ggplot(aes(x = diet, y = diff, fill = sign)) +
      geom_col() +
      facet_wrap(~ impacte, scales = "free_y", ncol = 1) +
      coord_flip() +
      labs(title = ifelse(input$mostrar_per_animal, "Diferència A - B per dieta (per animal)", "Diferència A - B per dieta (per kg pinso)"),
           y = "A - B", x = "Dieta") +
      theme_minimal()
    ggplotly(p)
  })
  
  # Mapa
  output$map_origens <- renderLeaflet({
    req(solA_joined(), solB_joined())
    plot_map_origens(solA_joined(), solB_joined(), transport_df = transport_df())
  })
  
  # Download CSV
  output$download_summary <- downloadHandler(
    filename = function() { paste0("resum_impactes_solucions_", Sys.Date(), ".csv") },
    content = function(file) {
      A_kg <- resumA_kg() %>% mutate(solucio = "A")
      B_kg <- resumB_kg() %>% mutate(solucio = "B")
      out <- bind_rows(A_kg, B_kg)
      if(input$mostrar_per_animal) {
        A_a <- resumA_animal() %>% mutate(solucio = "A")
        B_a <- resumB_animal() %>% mutate(solucio = "B")
        out_animal <- bind_rows(A_a, B_a) %>% mutate(type = "per_animal")
        out <- out %>% mutate(type = "per_kg") %>% bind_rows(out_animal)
      } else {
        out <- out %>% mutate(type = "per_kg")
      }
      write_csv(out, file)
    }
  )
}

shinyApp(ui, server)

