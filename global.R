# app.R (versió amb reassignació d'origen per ingredient + transport impacts + mapa)
library(shiny)
library(tidyverse)
library(readxl)
library(janitor)
library(DT)
library(plotly)
library(leaflet)
library(shinydashboard)

library(bslib)
library(shinyjs) # Para funcionalidades extra

# ---------------------------
# Helpers i utilitats
# ---------------------------


#Carregar dades medioambientals
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

#Carregar dades de ingredients used
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

#Carregar dades de emissions de transport
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
  df <- joined_df %>% 
    group_by(diet, ingredient) %>% 
    summarise(prop = sum(prop, na.rm = TRUE), .groups = "drop") %>%
    mutate(label_ing = paste0(ingredient, " (", round(prop, 2), ")"))
  
  if(!is.null(ordre_dietes)) 
    df <- df %>% mutate(diet = factor(diet, levels = ordre_dietes))
  
  p <- ggplot(df, aes(x = diet, y = prop, fill = label_ing)) +
    geom_col() +
    coord_flip() +
    scale_y_continuous(
      breaks = seq(0, 1, by = 0.25),
      limits = c(0, 1),
    ) +
    labs(
      title = "Composició de les dietes (per ingredient)",
      y = "Proporció (%)",
      x = "Dieta",
      fill = "Ingredients Utilitzats"
    ) +
    guides(fill = guide_legend(nrow = 5, byrow = TRUE)) +
    theme_minimal() +
    theme(legend.position = "bottom",  
          
          legend.direction = "horizontal",
          legend.title = element_text(size = 10, face = "bold"),
          legend.text = element_text(size = 8),
          axis.text = element_text(size = 8),
          axis.title = element_text(size = 10),
          plot.title = element_text(size = 12))
  
  # Convertim a ggplotly
  ggplotly(p, tooltip = c("y", "fill"))
}

#hi havia el Impacte per Dieta aqui

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


#TOP INGREDIENTS
# Top N ingredients
plot_topN_ingredients_per_dieta <- function(joined_df, diet_sel, impacte_sel = "climate_change",
                                            n = 5, per_animal = FALSE, kg_table = NULL) {
  
  df <- contribucio_per_ingredient_from_joined(
    joined_df, per_animal = per_animal, kg_table = kg_table
  ) %>%
    filter(impacte == impacte_sel, diet == diet_sel) %>%
    slice_max(order_by = valor, n = n, with_ties = FALSE)
  
  p <- ggplot(df, aes(x = reorder(ingredient, valor), y = valor, fill = ingredient)) +
    geom_col() +
    coord_flip() +
    labs(
      title = paste0("Top ", n, " ingredients – ", diet_sel, " (", impacte_sel, ")"),
      y = "Valor",
      x = "Ingredient"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      text = element_text(face = "bold")
    )
  
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


#######
# Arrays de dades
#######


# 1. Array per creació de ValueBoxes a 'Visio General'
box_config <- list(
  list(id = "box_ingredients", title = "Ingredients",        color = "green",  var = "ingredient", source = "env"),
  list(id = "box_origins",     title = "Origins (Countries)", color = "blue",   var = "origen",     source = "env"),
  list(id = "box_diets",       title = "Total Diets",        color = "purple", var = "diet",       source = "diets"),
  list(id = "box_steps",       title = "Steps",              color = "orange", var = "step",       source = "diets")
)