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

calcula_solucio_amb_transport <- function(ingr_used_df, dades_env_df, step_sel, overrides_df, 
                                          transport_df, ordre_dietes) {
  
  #--------------------------------------------------------------------------
  
 #Tenim tots els ingredients que formen el step_sel, l'step actual
  #Seleccionem  tambe el pas de la dieta (Creixement, Engreix...)
  step_ing <- ingr_used_df %>%
    filter(step %in% step_sel) %>%
    select(step, ingredient, diet ,prop)
    
  #View(step_ing)
  
  # 2. Obtener filas efectivas (Default o Overrides)
  #REVISIÓ: Sense overrides funciona be, amb overrides no ho se
  #effective_rows = Contè els ingredients de step, amb els seus corresponents origens i valors d'emissions
  # ingredient, origen, emissions
  if(is.null(overrides_df) || nrow(overrides_df) == 0) {
    
    # Iteramos sobre cada fila de step_ing (que contiene la combinación ingrediente + dieta)
    effective_rows <- map_dfr(seq_len(nrow(step_ing)), function(i) {
      
      # Extraemos el ingrediente y la dieta de la fila actual
      ing_actual  <- step_ing$ingredient[i]
      diet_actual <- step_ing$diet[i]
      
      # Buscamos los datos ambientales para ese ingrediente
      row <- get_default_row_for_ingredient(dades_env_df, ing_actual)
      
      if(is.null(row)) {
        # Si no hay datos, creamos la fila vacía manteniendo la dieta
        tibble(
          ingredient = ing_actual, 
          diet = diet_actual, 
          group = NA, 
          origen = NA
        )
      } else { 
        # Si hay datos, añadimos la columna 'diet' a la fila encontrada
        row %>% mutate(diet = diet_actual)
      }
    })
    
  } else {
    
    # 1. Obtenemos las filas modificadas
    res_df <- apply_overrides_to_env(dades_env_df, overrides_df)$df
    
    # 2. Primero generamos la tabla por defecto para TODOS los ingredientes per si de cas
    effective_rows_default <- map_dfr(unique(ing_list_df$ingredient), function(ing) {
      get_default_row_for_ingredient(dades_env_df, ing)
    })
    
    # 3. Sustituimos los que tienen override
    effective_rows <- effective_rows_default %>%
      filter(!(ingredient %in% res_df$ingredient)) %>%
      bind_rows(res_df)
  }
  
  
  #View(effective_rows)
  
  #necessito adquirir del fitxer transport, els origens que existeixen a effective rows, conjuntament amb les emissions
  
  #obtenim  origens unics d'efective rows
  origen_list_effective <- effective_rows %>%
    select(origen) %>%
    distinct() %>%
    drop_na()
  
  #View(origen_list_effective)
  
  #obtenim emissions necessàries
  impact_cols_effective <- intersect(
    c("climate_change", "land_use", "water_use","eutrophication", "acidification", "particulate_matter"), 
    names(transport_df))
  
  #obtenim les emisions del fitxer transport, per obtenir un df i poder sumar amb effective rows
  # 1. Filtramos transport_df para quedarnos solo con los ingredientes que nos interesan
  # 1. Aseguramos que el transporte tenga nombres distintos para que no se mezcle
  # y nos quedamos solo con los países que necesitamos para esta llamada
  transporte_limpio <- transport_df %>%
    filter(origen %in% origen_list_effective$origen) %>%
    select(origen, all_of(impact_cols_effective)) %>%
    rename_with(~paste0("tr_", .), all_of(impact_cols_effective))
  
  #View(transporte_limpio)
  
  emissions_per_kg_ambTransport_df <- effective_rows %>%
    left_join(transporte_limpio, by = "origen") %>%
    mutate(
      # Sumem original + transport i DIVIDIM PER 1000 per passar de Tona a Kg
      across(all_of(impact_cols_effective), 
             ~ (.x + coalesce(get(paste0("tr_", cur_column())), 0)) / 1000)
    ) %>%
    select(-starts_with("tr_"))
    
  View( emissions_per_kg_ambTransport_df)  
  
    # 1. Aseguramos que tenemos la tabla de proporciones correcta
    # Nota: Usamos 'step_ing' que ya contiene 'diet', 'ingredient' y 'prop'
    proporcions <- step_ing %>%
      select(ingredient, diet, prop)
    
    # 2. Unimos y multiplicamos
    final_df <-  emissions_per_kg_ambTransport_df %>%
      # Unimos por el "paquete único" de ingrediente y dieta
      left_join(proporcions, by = c("ingredient", "diet")) %>%
      mutate(across(all_of(impact_cols_effective), 
                    # Multiplicamos la emisión total por la proporción (prop)
                    # Usamos coalesce(prop, 0) por seguridad
                    ~ .x * coalesce(prop, 0))) %>%
      # Opcional: Si quieres mantener la columna 'prop' en el resultado final, 
      # no la elimines del select.
      select(ingredient, diet, prop, all_of(impact_cols_effective), everything())
    
    View(final_df)
    
    return(final_df)
}


  #-------------------------------------------------------------------------

resum_per_dieta_from_joined <- function(joined_df, per_animal = FALSE, kg_table = NULL) {
  
  # 1. Definim els noms exactes que surten de 'calcula_solucio_amb_transport'
  impact_cols <- c("climate_change", "land_use", "water_use", 
                   "eutrophication", "acidification", "particulate_matter")
  
  # 2. Ens assegurem de seleccionar només les columnes que realment existeixen al DF
  target_cols <- intersect(impact_cols, names(joined_df))
  
  # Si no troba cap columna d'impacte, retornem un DF buit amb l'estructura correcta
  if(length(target_cols) == 0) {
    return(tibble(diet = character(), impacte = character(), valor = numeric()))
  }
  
  # 3. Sumem els valors per cada dieta
  res <- joined_df %>%
    group_by(diet) %>%
    summarise(across(all_of(target_cols), sum, na.rm = TRUE), .groups = "drop")
  
  #View(res)
  
  # 4. Multiplicació opcional per consum animal
  if(per_animal && !is.null(kg_table)) {
    kg_tbl <- kg_table %>% distinct(diet, kg_consum)
    res <- res %>% 
      left_join(kg_tbl, by = "diet") %>%
      mutate(across(all_of(target_cols), ~ .x * coalesce(kg_consum, 1))) %>%
      select(-kg_consum)
  }
  
  # 5. EL PAS CLAU: Pivotar per crear la columna 'impacte'
  # Això converteix les columnes (climate_change, land_use...) en files
  res_long <- res %>%
    pivot_longer(
      cols = all_of(target_cols), 
      names_to = "impacte",   # Aquí es crea la columna que et donava l'error
      values_to = "valor"
    )
  
  #View(res_long)
  
  return(res_long)
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

plot_composicio <- function(joined_df, ordre_dietes = NULL) {
  # 1. Agrupamos por dieta e ingrediente para sumar las proporciones
  df <- joined_df %>% 
    group_by(diet, ingredient) %>% 
    summarise(prop = sum(prop, na.rm = TRUE), .groups = "drop")
  
  # 2. Aplicamos el orden de las dietas si se proporciona
  if(!is.null(ordre_dietes)) 
    df <- df %>% mutate(diet = factor(diet, levels = ordre_dietes))
  
  # 3. Creamos el gráfico usando 'ingredient' para el color (fill)
  p <- ggplot(df, aes(x = diet, y = prop, fill = ingredient)) +
    geom_col(color = "white", size = 0.1) + # Línea fina para separar bloques
    coord_flip() +
    scale_y_continuous(
      breaks = seq(0, 1, by = 0.25),
      limits = c(0, 1)
    ) +
    labs(
      title = "Composició de les dietes (per ingredient)",
      y = "Proporció (%)",
      x = "Dieta",
      fill = "Ingredients Utilitzats"
    ) +
    theme_minimal() +
    theme(
      legend.position = "right", # Movido a la derecha para mejor lectura
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 8),
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 10),
      plot.title = element_text(size = 12)
    )
  
  # 4. Configuramos ggplotly para mostrar el porcentaje real al pasar el ratón
  # El tooltip mostrará el ingrediente y el valor de 'prop' formateado
  ggplotly(p, tooltip = c("fill", "y")) 
}


# Contribució per origen (ja tenim origen_region en joined_df)

contribucio_per_origen_from_joined <- function(joined_df, per_animal = FALSE, kg_table = NULL) {
  
  impact_cols <- c("climate_change", "land_use", "water_use", 
                   "eutrophication", "acidification", "particulate_matter")
  
  # 1. Agrupem per dieta i origen
  res <- joined_df %>%
    group_by(diet, origen) %>%
    summarise(across(all_of(intersect(impact_cols, names(.))), sum, na.rm = TRUE), .groups = "drop")
  
  View(res)
  
  # 2. Càlcul per animal (si escau)
  if(per_animal && !is.null(kg_table)) {
    kg_tbl <- kg_table %>% distinct(diet, kg_consum)
    res <- res %>% 
      left_join(kg_tbl, by = "diet") %>%
      # Important: Aquí les teves columnes NO tenen el prefix "contrib_" encara
      mutate(across(all_of(intersect(impact_cols, names(.))), ~ .x * coalesce(kg_consum, 1))) %>%
      select(-kg_consum)
  }
  
  # 3. Pivotatge a format llarg
  res_long <- res %>%
    pivot_longer(-c(diet, origen), names_to = "impacte", values_to = "valor") %>%
    # Eliminem el mutate de str_remove perquè els teus noms ja són nets
    select(diet, origen, impacte, valor)
  
  View(res_long)
  
  return(res_long) 
}
########################################### CONTRIBUCIO PER ORIGEN  ########################################### 


plot_origen_per_dieta_from_joined <- function(joined_df, impactes_sel = NULL, per_animal = FALSE, ordre_dietes = NULL) {
  
  req(impactes_sel) # Validació Shiny
  
  # 1. Obtenció de dades processades
  df <- contribucio_per_origen_from_joined(joined_df, per_animal = per_animal)
  
  # 2. Validació de contingut
  if (is.null(df) || nrow(df) == 0 || all(is.na(df$valor))) {
    return(
      plotly::plot_ly() %>% 
        add_annotations(
          text = "⚠️ No hi ha dades d'impacte per a aquesta selecció",
          showarrow = FALSE, font = list(size = 15, color = "red")
        )
    )
  }
  
  # 3. Filtres i ordenació
  df <- df %>% filter(impacte %in% impactes_sel)
  if(!is.null(ordre_dietes)) df <- df %>% mutate(diet = factor(diet, levels = ordre_dietes))
  
  # 4. Generació del Plot amb geom_col
  p <- ggplot(df, aes(x = diet, y = valor, fill = origen)) +
    # geom_col amb vora blanca per separar visualment els països
    geom_col(color = "white", size = 0.2) + 
    # Facet_wrap per separar cada categoria d'impacte amb la seva escala
    facet_wrap(~ impacte, scales = "free_y", ncol = 1) + 
    coord_flip() +
    labs(
      title = ifelse(per_animal, "Contribució per origen (per animal)", "Contribució per origen (per kg pinso)"),
      subtitle = "Distribució de la petjada ambiental segons l'origen geogràfic",
      y = "Impacte Ambiental", 
      x = "Dieta",
      fill = "País d'Origen"
    ) +
    theme_minimal() +
    theme(
      strip.text = element_text(face = "bold"), # Títols dels impactes més visibles
      panel.spacing = unit(1, "lines"), # Més espai entre gràfics
      legend.position = "bottom"
    )
  
  ggplotly(p)
}


#---

########################################### TOP INGREDIENTS  ########################################### 


contribucio_per_ingredient_from_joined <- function(joined_df, per_animal = FALSE, kg_table = NULL) {
  
  impact_cols <- c("climate_change", "land_use", "water_use", 
                   "eutrophication", "acidification", "particulate_matter")
  
  # 1. Seleccionamos las columnas necesarias y nos aseguramos de que existen
  target_cols <- intersect(impact_cols, names(joined_df))
  
  # 2. Calculamos la contribución real (Impacto * Proporción)
  # Esto es necesario si joined_df contiene el impacto por kg de ingrediente
  df <- joined_df %>%
    mutate(across(all_of(target_cols), ~ .x * coalesce(prop, 0), .names = "contrib_{.col}"))
  
  # 3. Si es por animal, multiplicamos por el consumo total
  if(per_animal && !is.null(kg_table)) {
    kg_tbl <- kg_table %>% distinct(diet, kg_consum)
    df <- df %>% 
      left_join(kg_tbl, by = "diet") %>%
      mutate(across(starts_with("contrib_"), ~ .x * coalesce(kg_consum, 1))) %>%
      select(-kg_consum)
  }
  
  # 4. Pivotamos usando el nuevo prefijo creado en el paso 2
  df_long <- df %>%
    pivot_longer(
      cols = starts_with("contrib_"), 
      names_to = "var", 
      values_to = "valor"
    ) %>%
    mutate(impacte = str_remove(var, "^contrib_")) %>%
    select(diet, ingredient, impacte, prop, valor)
  
  return(df_long)
}

plot_topN_ingredients_per_dieta <- function(joined_df, diet_sel, impacte_sel = "climate_change",
                                            n = 5, per_animal = FALSE, kg_table = NULL) {
  
  # Obtenemos los datos procesados
  df_all <- contribucio_per_ingredient_from_joined(
    joined_df, per_animal = per_animal, kg_table = kg_table
  )
  
  # Filtramos por dieta e impacto y seleccionamos los N mejores
  df <- df_all %>%
    filter(impacte == impacte_sel, diet == diet_sel) %>%
    slice_max(order_by = valor, n = n, with_ties = FALSE)
  
  # Validación: Si el filtro devuelve 0 filas, mostramos gráfico vacío
  if(nrow(df) == 0) return(plotly_empty() %>% layout(title = "Sense dades per aquesta selecció"))
  
  p <- ggplot(df, aes(x = reorder(ingredient, valor), y = valor, fill = ingredient)) +
    geom_col() +
    coord_flip() +
    labs(
      title = paste0("Top ", n, " ingredients – ", diet_sel, " (", impacte_sel, ")"),
      y = "Contribució (Impacte)",
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
  # Llista de països per a RER (Europa)
  paises_europa <- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "ES", 
                     "FI", "FR", "GR", "HU", "IE", "IT", "LT", "LU", "LV", 
                     "MT", "NL", "PL", "PT", "RO", "SE", "SI", "SK")
  
  map_df_processat <- map_df %>%
    mutate(origen_iso2 = substr(origen, 1, 2),
           es_rer = (origen == "RER"))
  
  normals <- map_df_processat %>% filter(!es_rer)
  
  # Expandim RER mantenint la llista d'ingredients
  rer_expandidor <- map_df_processat %>% 
    filter(es_rer) %>%
    uncount(length(paises_europa)) %>%
    mutate(origen_iso2 = rep(paises_europa, length.out = n()))
  
  final_df <- bind_rows(normals, rer_expandidor) %>%
    mutate(iso3 = countrycode(origen_iso2, "iso2c", "iso3c")) %>%
    filter(!is.na(iso3)) %>%
    group_by(iso3) %>%
    summarise(
      n_ingredients = n(),
      # Concatenem els noms per al tooltip
      llista_ingredients = paste(unique(ingredient), collapse = ", ")
    )
  
  return(final_df)
}

plot_map_solucio_highcharter <- function(joined_df, env_data, titol = "") {
  
  # 1. Preparem la base amb el nom de l'ingredient
  map_df_base <- joined_df %>%
    select(ingredient) %>%
    distinct() %>%
    left_join(env_data %>% select(ingredient, origen), by = "ingredient") %>%
    filter(!is.na(origen))
  
  # 2. Enviem la llista completa a processar
  data_final <- preparar_dades_mapa_full(map_df_base)
  
  hcmap(
    map = "custom/world-lowres", 
    data = data_final,
    joinBy = c("iso-a3", "iso3"),
    value = "n_ingredients",
    name = "Ingredients",
    download_map_data = TRUE
  ) %>%
    hc_colorAxis(minColor = "#e6f2ff", maxColor = "#003366") %>%
    hc_title(text = titol) %>%
    # Tooltip HTML per mostrar el nombre i la llista a sota
    hc_tooltip(
      useHTML = TRUE,
      headerFormat = "<b>{point.name}</b><br>",
      pointFormat = "<b>Total:</b> {point.value}<br><b>Quins:</b> {point.llista_ingredients}"
    ) %>%
    hc_legend(layout = "vertical", align = "right", verticalAlign = "middle") %>%
    hc_mapNavigation(enabled = TRUE)
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




#----------------------------------------- COmparacio A vs B


# Gráfico de Diferencias corregido (A - B)
plot_diferencies_AB <- function(A_data, B_data, imp) {
  A_f <- A_data %>% filter(impacte == imp) %>% select(diet, valor_A = valor)
  B_f <- B_data %>% filter(impacte == imp) %>% select(diet, valor_B = valor)
  
  df <- full_join(A_f, B_f, by = "diet") %>%
    mutate(
      diff = coalesce(valor_A, 0) - coalesce(valor_B, 0),
      sign = ifelse(diff > 0, "A té més impacte (Pitjor)", "B té més impacte (Pitjor)")
    )
  
  p <- ggplot(df, aes(x = diet, y = diff, fill = sign, text = diet)) +
    geom_col(color = "white") +
    geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
    coord_flip() +
    scale_fill_manual(values = c("A té més impacte (Pitjor)" = "#e74c3c", "B té més impacte (Pitjor)" = "#2ecc71")) +
    labs(title = paste("Diferència:", imp), y = "Valor A - B", x = "Dieta", fill = "Resultat") +
    theme_minimal()
  
  ggplotly(p, tooltip = c("text", "y"))
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