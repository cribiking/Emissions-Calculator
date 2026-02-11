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

library(dplyr)#Mapa
library(countrycode)
library(highcharter)


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
  df <- read_excel(path, sheet=1) %>% clean_names()
  # s'espera col 'origen' i idealment les mateixes categories d'impacte + lat/lon opcional
  if(!"origen" %in% names(df)) {
    stop("El fitxer de transport ha de contenir la columna 'origen'")
  }
  
  return(df)
}

carregar_dades_externes <- function(path){
  df <- read_excel(path, sheet = 1) %>% 
    clean_names() %>% 
    
    select("ingredient", "emision_name" , "value", "unit")
  
  if(!"ingredient" %in% names(df)){
    stop("El fitxer de dades externes no te la columna 'ingredient'")
  }
  
  return(df)
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

#dades_dietes(), dades_env(), input$stepA, overrides_df = overrides(), transport_df = transport_df(),
#ordre_dietes = ordre_dietes()

calcula_solucio_amb_transport <- function(dietes_df, dades_env_df, step_sel, overrides_df, 
                                          transport_df, ordre_dietes) {
  
  
  View(dietes_df)
  
  View(step_sel)
  
  View(dades_env_df)
  
  # 1. Filtro y limpieza inicial de dietas (eliminamos columnas extrañas)
  #REVISIÓ : FUnciona okey, prevè de columnes inconssitens o inecessaries
  dietes <- dietes_df %>% 
    filter(step == step_sel) %>%
    select(step, ingredient, diet, prop) # Solo nos quedamos con lo necesario
    
  
  if(nrow(dietes) == 0) return(tibble())
  
  #Agafem columna d'ingredients
  ing_list <- unique(dietes$ingredient)
  
  # 2. Obtener filas efectivas (Default o Overrides)
  
  #Si no hi ha cap modificació per part de l'usuari
  #REVISIÓ: Sense overrides funciona be, amb overrides no ho se
  if(is.null(overrides_df) || nrow(overrides_df) == 0) {
    effective_rows <- map_dfr(ing_list, function(ing) {
      row <- get_default_row_for_ingredient(dades_env_df, ing)
      if(is.null(row)) {
        tibble(ingredient = ing, group = NA, origen = NA)
      } else { row }
    })
  } else {
    res <- apply_overrides_to_env(dades_env_df, overrides_df)
    effective_rows <- res$df
  }
  
  View(effective_rows)
  
  impact_cols <- intersect(c("climate_change", "land_use", "water_use", 
                             "eutrophication", "acidification", "particulate_matter"), 
                           names(effective_rows))
  #
  
  # 3. Join con Transporte (Usamos sufijos claros)
  if(!is.null(transport_df)) {
    
    #mateix resultat que a impact_cols 
    transport_cols <- intersect(names(transport_df), impact_cols)
    

    
    trans_sel <- transport_df %>% 
      select(ingredient,origen, all_of(impact_cols))%>%
      # Nos aseguramos de que los impactos sean números (por si hay "NA" escrito como texto)
      mutate(across(all_of(transport_cols), ~as.numeric(as.character(.))))
    
    #view(trans_sel)#aqui pasa algo raro
  
    
    effective_rows <- effective_rows %>% 
      left_join(trans_sel, by = "origen", suffix = c("", "_trans_raw"))
    
    for(col in impact_cols) {
      tcol_raw <- paste0(col, "_trans_raw")
      tcol_final <- paste0("transport_", col)
      
      if(tcol_raw %in% names(effective_rows)) {
        # Si el join funcionó, sumamos el valor. Si dio NA (origen no encontrado), ponemos 0.
        effective_rows[[tcol_final]] <- coalesce(effective_rows[[tcol_raw]], 0)
        effective_rows[[tcol_raw]] <- NULL # Limpiamos la columna temporal
      } else {
        effective_rows[[tcol_final]] <- 0
      }
    }
  }
  
    # 4. Join Final y CÁLCULO
    # Al haber limpiado 'dietes' arriba, ya no habrá duplicados de nombres (.x .y)
    joined <- dietes %>% left_join(effective_rows, by = "ingredient")
    
    for(col in impact_cols) {
      tcol <- paste0("transport_", col)
    # Impacto total = Producción + Transporte
    joined[[paste0("impact_", col)]] <- coalesce(joined[[col]], 0) + coalesce(joined[[tcol]], 0)
    # Contribución = Impacto total * proporción en la dieta
    joined[[paste0("contrib_", col)]] <- joined[[paste0("impact_", col)]] * coalesce(joined$prop, 0)
  }
  
  # 5. Ordenar
  if(!is.null(ordre_dietes)) {
    joined <- joined %>% mutate(diet = factor(diet, levels = ordre_dietes)) %>% arrange(diet)
  } else {
    joined <- joined %>% arrange(diet)
  }
  
  return(joined)
}

calcula_solucio_amb_transport_v2 <- function(ingr_used_df, dades_env_df, step_sel, overrides_df, 
                                          transport_df, ordre_dietes) {
  
  
  View(ingr_used_df)
  
  View(step_sel)
  
  View(dades_env_df)
  
  
  # 1. Filtro y limpieza inicial de dietas (eliminamos columnas extrañas)
  #REVISIÓ : FUnciona okey, prevè de columnes inconssitens o inecessaries
  ingr_used_step <- ingr_used_df %>% 
    filter(step == step_sel) %>%
    select(step, ingredient, diet, prop) # Solo nos quedamos con lo necesario
  
  
  
  
  return()
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


# Contribució per origen (ja tenim origen_region en joined_df)

contribucio_per_origen_from_joined <- function(
    joined_df, per_animal = FALSE, kg_table = NULL) {
  
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

#-------------------- CONTRIBUCIO PER ORIGEN

plot_origen_per_dieta_from_joined <- function(joined_df, impactes_sel = NULL, per_animal = FALSE, ordre_dietes = NULL) {

  #Comprovem si hi ha seleccio d'impactes, no volem que sigui null
  req(impactes_sel)
  
  # 1. Intentamos obtener los datos
  df <- contribucio_per_origen_from_joined(joined_df, per_animal = per_animal)
  print("COmprovació AAAAAAAAAAAAAAa")
  View(df)
  
  # 2. VALIDACIÓN CRÍTICA: ¿Hay datos para graficar?
  # Si el dataframe está vacío o el valor total es NA/0, mostramos un mensaje
  if (is.null(df) || nrow(df) == 0 || all(is.na(df$valor))) {
    return(
      plotly::plot_ly() %>% 
        add_annotations(
          text = "⚠️ No hi ha dades d'impacte per a aquest ingredient/selecció",
          x = 0.5, y = 0.5, 
          showarrow = FALSE, 
          font = list(size = 15, color = "red")
        ) %>% 
        config(displayModeBar = FALSE)
    )
  }
  
  # 3. Aplicamos filtros si existen
  if(!is.null(impactes_sel)) df <- df %>% filter(impacte %in% impactes_sel)
  if(!is.null(ordre_dietes)) df <- df %>% mutate(diet = factor(diet, levels = ordre_dietes))
  
  # 4. Creación del gráfico
  p <- df %>%
    ggplot(aes(x = diet, y = valor, fill = origen)) +
    geom_col(color = "white", size = 0.1) + # Añadimos borde fino para separar orígenes
    facet_wrap(~ impacte, scales = "free_y", ncol = 1) +
    coord_flip() +
    labs(
      title = ifelse(per_animal, "Contribució per origen (per animal)", "Contribució per origen (per kg pinso)"),
      y = "Valor (Impacte)", 
      x = "Dieta"
    ) +
    theme_minimal() +
    theme(strip.text = element_text(face = "bold")) # Resaltamos los nombres de los impactos
  
  ggplotly(p)
}

#----------------------------------------------------

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


#################MAPAAA

library(dplyr)
library(tidyr)
library(countrycode)

preparar_dades_mapa_full <- function(map_df) {
  
  # Definimos la lista de países que representarán a "RER" (Europa)
  paises_europa <- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "ES", 
                     "FI", "FR", "GR", "HU", "IE", "IT", "LT", "LU", "LV", 
                     "MT", "NL", "PL", "PT", "RO", "SE", "SI", "SK")
  
  map_df_processat <- map_df %>%
    # 1. Tratamos los códigos regionales como CA-ON -> CA
    mutate(origen_iso2 = substr(origen, 1, 2)) %>%
    
    # 2. Separamos RER del resto
    mutate(es_rer = (origen == "RER"))
  
  # Extraemos los datos normales
  normals <- map_df_processat %>% filter(!es_rer)
  
  # Extraemos RER y lo duplicamos para cada país europeo
  rer_expandidor <- map_df_processat %>% 
    filter(es_rer) %>%
    uncount(length(paises_europa)) %>%
    mutate(origen_iso2 = rep(paises_europa, length.out = n()))
  
  # Unimos todo y convertimos a ISO3 (que le gusta más a Highcharter)
  final_df <- bind_rows(normals, rer_expandidor) %>%
    mutate(iso3 = countrycode(origen_iso2, "iso2c", "iso3c")) %>%
    filter(!is.na(iso3)) %>%
    # Si un país tiene datos propios y también datos de RER, sumamos o promediamos
    group_by(iso3) %>%
    summarise(n_ingredients = sum(n_ingredients, na.rm = TRUE))
  
  return(final_df)
}


plot_map_solucio_highcharter <- function(joined_df, env_data, titol = "") {
  
  # 1. Extraemos ingredientes y hacemos el join con datos ambientales
  map_df_base <- joined_df %>%
    select(ingredient) %>%
    distinct() %>%
    left_join(env_data, by = "ingredient") %>%
    filter(!is.na(origen))
  
  # 2. IMPORTANTE: Necesitamos contar los ingredientes por origen 
  # antes de enviarlo a la función de expansión
  map_counts <- map_df_base %>%
    group_by(origen) %>%
    summarise(n_ingredients = n(), .groups = 'drop')
  
  # 3. Llamamos a la función de expansión pasando los datos calculados
  data_final <- preparar_dades_mapa_full(map_counts)

  
  hcmap(
    map = "custom/world-lowres", # Usamos lowres para mayor velocidad en Shiny
    data = data_final,
    joinBy = c("iso-a3", "iso3"),
    value = "n_ingredients",
    name = "Nombre d'ingredients",
    download_map_data = TRUE
  ) %>%
    hc_colorAxis(minColor = "#e6f2ff", maxColor = "#003366") %>%
    hc_title(text = titol) %>%
    hc_tooltip(pointFormat = "{point.name}: {point.value} ingredients") %>%
    hc_legend(layout = "vertical", align = "right", verticalAlign = "middle") %>%
    hc_mapNavigation(enabled = TRUE) # Permite hacer zoom
}

############################COMPROVACIO INGREDIENTS FALTANTS

comprovar_ingredients_faltants <- function(df_dietes, df_ambientals) {
  # 1. Obtenemos los ingredientes únicos de ambos archivos
  # Usamos clean_names() indirectamente o asumimos que ya vienen limpios
  ing_dietes <- unique(df_dietes$ingredient)
  ing_env <- unique(df_ambientals$ingredient)
  
  # 2. Encontramos qué hay en dietas que NO está en ambientales
  faltants <- setdiff(ing_dietes, ing_env)
  
  return(faltants)
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