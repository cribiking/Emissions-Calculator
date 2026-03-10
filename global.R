# --- Paquets essencials per a la interfície i reactivitat ---
library(rlang)
library(shiny)
library(shinydashboard)
library(DT)             # Per a les taules interactives (tbl_kg_edit, etc.)
library(plotly)         # Per als gràfics dinàmics (barres, boxplots)
# 
# --- Paquets per al processament de dades ---
library(tidyverse)      # Inclou dplyr, tidyr, ggplot2, etc.
library(readxl)         # Per llegir els fitxers .xlsx
library(janitor)        # Per netejar noms de columnes (clean_names)

# --- Paquets per al Mapa ---
library(countrycode)    # Per convertir ISO2 a ISO3
library(highcharter)    # Per al mapa d'orígens (utilitza worldgeojson intern)

# --- Paquets per captures ggarrenge ---
library(ggpubr)


############ GLOBAL VARIABLES  #####################


IMPACT_NAMES <- c("climate_change", "land_use", "water_use", 
                  "eutrophication_marine", "acidification", "particulate_matter")


# Defineix-lo així per poder fer cerques per clau
UNITATS <- c(
  "climate_change"        = "kg CO2 eq", 
  "land_use"              = "dimensionless (pt)", 
  "water_use"             = "m3 world eq", 
  "eutrophication_marine" = "kg N eq", 
  "acidification"         = "mol H+ eq", 
  "particulate_matter"    = "disease incidence"
)

OVERRIDES <- reactiveVal(tibble(ingredient = character(0), origen_selected = character(0)))



# Al teu fitxer Global.R o a l'inici de l'app
colors_paisos <- c(
  "ES"  = "#2c3e50", # Blau fosc (Espanya / RER)
  "FR"  = "#3498db", # Blau clar
  "DE"  = "#e67e22", # Taronja
  "NL"  = "#f1c40f", # Groc
  "BR"  = "#27ae60", # Verd
  "USA" = "#c0392b", # Vermell
  "RER" = "#2c3e50", # Mateix que ES si vols coherència
  "Altres" = "#95a5a6" # Gris per a la resta
)




# ---------------------------
# Càrregar Fitxers
# ---------------------------


#Carregar dades medioambientals
carrega_dades_ambientals <- function(path) {
  
  df <- read_excel(path) %>% clean_names()
  # Acceptem que hi hagi múltiples files per mateix ingredient amb orígens alternatius.
  expect_cols <- c("ingredient", "group", "origen", "default_origen")
  # a més, busquem les columnes d'impacte (acceptem subset)
  impact_cols <- intersect(IMPACT_NAMES, names(df))
  
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



# Retorna un df amb cada ingredient i un vector (llista) dels seus orígens
origens_per_ingredient <- function(dades_env_df) {
  
  ing_amb_origen <- dades_env_df %>% 
    
    select(ingredient, origen) %>%
    distinct() %>%
    # Agrupem pel nom de l'ingredient
    group_by(ingredient) %>%
    # Creem la columna 'origen' com un vector de cadenes de text
    summarise(
      origen = list(origen),
      n_origens = n(), # Comptador útil per fer el filtre posterior
      .groups = 'drop'
    )
  
  return(ing_amb_origen)
}

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

# Aplica overrides (ingredient -> origen seleccionat) sobre les dades ambientals:
# Retorna una taula amb, per cada ingredient i origen utilitzat, les columnes d'impacte
# Lògica: per a cada ingredient a usar, busquem la fila dades_env amb ingredient+origin_override.
# Si no existeix, fem servir la fila default (default_origen==1) i avisem.

apply_overrides_to_env <- function(dades_env_df, overrides_df) {
  # 1. Si no hi ha overrides, retornem el df original sense canvis
  if (is.null(overrides_df) || nrow(overrides_df) == 0) {
    return(list(df = dades_env_df, msgs = character(0)))
  }
  
  # Creem una còpia de treball
  res_df <- dades_env_df
  msgs <- character()
  
  # 2. Iterem pels overrides seleccionats
  for(i in seq_len(nrow(overrides_df))) {
    
    ing <- overrides_df$ingredient[i]
    ori_sel <- overrides_df$origen_selected[i]
    
    # Intentem trobar la fila exacta (ingredient + nou origen)
    row_match <- dades_env_df %>% filter(ingredient == ing, origen == ori_sel)
    
    if(nrow(row_match) >= 1) {
      # Substitució: Eliminem totes les files d'aquest ingredient i posem la nova
      # (D'aquesta manera ens assegurem que l'ingredient només té 1 origen actiu en el càlcul)
      res_df <- res_df %>% 
        filter(ingredient != ing) %>% 
        bind_rows(row_match[1, ])
      
    } else {
      # Fallback si l'origen demanat no existeix a les dades
      msgs <- c(msgs, paste0("⚠️ '", ing, "': origen '", ori_sel, "' no trobat. Mantinguem dades actuals."))
    }
  }
  
  list(df = res_df, msgs = msgs)
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
    
  } else { #si hi ha overrides
    
    # 1. Obtenim la llista d'ingredients amb l'origen JA triat (el de override o el default)
    # Usarem la funció apply_overrides_to_env que hem arreglat abans
    env_preparat <- apply_overrides_to_env(dades_env_df, overrides_df)$df
    
    # 2. Ara "mapegem" cada ingredient de la dieta amb la seva fila corresponent a env_preparat
    # Així mantenim la columna 'diet' correctament per a cada combinació
    effective_rows <- map_dfr(seq_len(nrow(step_ing)), function(i) {
      
      ing_actual  <- step_ing$ingredient[i]
      diet_actual <- step_ing$diet[i]
      
      # Busquem la fila dins del nostre env ja filtrat/modificat
      row <- env_preparat %>% filter(ingredient == ing_actual)
      
      if (nrow(row) == 0) {
        # Fallback per si un ingredient de la dieta no existeix a l'ambiental
        tibble(ingredient = ing_actual, diet = diet_actual, group = NA, origen = NA)
      } else {
        # IMPORTANT: usem [1,] per si de cas hi hagués duplicats, i afegim la dieta
        row[1, ] %>% mutate(diet = diet_actual)
      }
    })
  }
  
  #View(OVERRIDES())
  
  #necessito adquirir del fitxer transport, els origens que existeixen a effective rows, conjuntament amb les emissions
  
  #obtenim  origens unics d'efective rows
  origen_list_effective <- effective_rows %>%
    select(origen) %>%
    distinct() %>%
    drop_na()
  
  #View(origen_list_effective)
  
  #obtenim emissions necessàries
  impact_cols_effective <- intersect( IMPACT_NAMES,names(transport_df))
  
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
      # Sumem original + transport i DIVIDIM PER 1000 per passar de Tona a Kg. Ja que els dos valor estan en TONES
      across(all_of(impact_cols_effective), 
             ~ (.x + coalesce(get(paste0("tr_", cur_column())), 0)) / 1000)
    ) %>%
    select(-starts_with("tr_"))
    
  #View( emissions_per_kg_ambTransport_df)  
  
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
    
    #View(final_df)
    
    return(final_df)
}
 #---

dades_consum_animals <- function(kg_table , final_df){
  #Reb el final_df amb tot calculat, i multiplica els 
}



  #-------------------------------------------------------------------------

resum_per_dieta_from_joined <- function(joined_df) {
  
  
  # 2. Ens assegurem de seleccionar només les columnes que realment existeixen al DF
  target_cols <- intersect(IMPACT_NAMES, names(joined_df))
  
  # Si no troba cap columna d'impacte, retornem un DF buit amb l'estructura correcta
  if(length(target_cols) == 0) {
    return(tibble(diet = character(), impacte = character(), valor = numeric()))
  }
  
  # 3. Sumem els valors per cada dieta
  res <- joined_df %>%
    group_by(diet) %>%
    summarise(across(all_of(target_cols), sum, na.rm = TRUE), .groups = "drop")
  
  #View(res)
  
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




################## 3###########################

calcul_contribucio_total_per_animal <- function(joined_df , kg_table = NULL){
  
  # 2. Ens assegurem de seleccionar només les columnes que realment existeixen al DF
  target_cols <- intersect(IMPACT_NAMES, names(joined_df))
  
  # Si no troba cap columna d'impacte, retornem un DF buit amb l'estructura correcta
  if(length(target_cols) == 0) {
    return(tibble(diet = character(), impacte = character(), valor = numeric()))
  }
  
  # 3. Sumem els valors per cada dieta
  res <- joined_df %>%
    group_by(diet) %>%
    summarise(across(all_of(target_cols), sum, na.rm = TRUE), .groups = "drop")
  
  #View(res)
  
  kg_tbl <- kg_table %>% distinct(diet, kg_consum)
  res <- res %>% 
    left_join(kg_tbl, by = "diet") %>%
    mutate(across(all_of(target_cols), ~ .x * coalesce(kg_consum, 1))) %>%
    select(-kg_consum)

  
  # 5. EL PAS CLAU: Pivotar per crear la columna 'impacte'
  # Això converteix les columnes (climate_change, land_use...) en files
  res_long_anim <- res %>%
    pivot_longer(
      cols = all_of(target_cols), 
      names_to = "impacte",   # Aquí es crea la columna que et donava l'error
      values_to = "valor"
    )
  
  #View(res_long_anim)
  
  return(res_long_anim)
  
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
    geom_col(color = "white", linewidth = 0.1, width = 0.5) + # Línea fina para separar bloques
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

contribucio_per_origen_from_joined <- function(joined_df) {
  
  # 1. Agrupem per dieta i origen
  res <- joined_df %>%
    group_by(diet, origen) %>%
    summarise(across(all_of(intersect(IMPACT_NAMES, names(.))), sum, na.rm = TRUE), .groups = "drop")
  
  #View(res)
  
  # 3. Pivotatge a format llarg
  res_long <- res %>%
    pivot_longer(-c(diet, origen), names_to = "impacte", values_to = "valor") %>%
    # Eliminem el mutate de str_remove perquè els teus noms ja són nets
    select(diet, origen, impacte, valor)
  
  #View(res_long)
  
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


plot_topN_ingredients_per_dieta <- function(joined_df, diet_sel, impacte_sel = "climate_change", 
                                            n = 5, per_animal = FALSE, kg_table = NULL, 
                                            bar_color = "#2c3e50" , unitat_text = "") {
  
  # 2. Transformem el dataframe de format "wide" a "long" per poder filtrar per impacte
  # També netegem possibles valors NA que apareixen a la imatge
  df_long <- joined_df %>%
    pivot_longer(
      cols = all_of(IMPACT_NAMES),
      names_to = "impacte",
      values_to = "valor"
    ) %>%
      # Filtrem la dieta i l'impacte primer
      filter(diet == diet_sel, impacte == impacte_sel) %>%
      # Eliminem valors buits i valors zero que poden falsejar el rànquing
      filter(!is.na(valor), valor > 0)
  
  # 2. SELECCIÓ REAL DEL TOP N
  # Ordenem de forma descendent i agafem els N primers
  df_top <- df_long %>%
    arrange(desc(valor)) %>%
    head(n) %>%
    # FORCEM l'ordre de l'ingredient com a factor basat en el valor 
    # (això evita que ggplot els torni a desordenar alfabèticament)
    mutate(ingredient = factor(ingredient, levels = ingredient[order(valor)]))
  
  # Validació: Si no hi ha dades després del filtre
  if(nrow(df_top) == 0) {
    return(plotly_empty() %>% layout(title = "Sense dades per aquesta selecció"))
  }
  
  
  # 4. Creació del títol de l'eix amb unitat
  label_eix_y <- if(unitat_text != "") paste0("Contribució (", unitat_text, ")") else "Contribució"
  
  # 3. Gràfic amb escala de 0.05 i ordre correcte
  p <- ggplot(df_top, aes(x = ingredient, y = valor)) +
    geom_col(fill = bar_color, width = 0.7) + 
    coord_flip() +
    scale_y_continuous(labels = scales::label_number(accuracy = 0.0001)) +
    labs(
      title = paste0("Top ", n, " ingredients – ", diet_sel),
      subtitle = paste("Impacte:", impacte_sel),
      y = label_eix_y,
      x = ""
    ) +
    theme_minimal() +
    theme(
      text = element_text(face = "bold"),
      # Afegim línies de graella més visibles per ajudar a la lectura dels 0.05
      panel.grid.major.x = element_line(color = "#e0e0e0"),
      panel.grid.minor.x = element_line(color = "#f0f0f0")
    )
  
  ggplotly(p) %>% 
    layout(margin = list(l = 150, r = 50, b = 50, t = 50)) 
}
#################MAPAAA



# Descarreguem el mapa del món una sola vegada al principi
#mapa_mundi <- download_map_data("custom/world")

# Funció per processar els orígens (agrupant Europa a Espanya)
preparar_dades_mapa_full <- function(map_df) {
  req(map_df)
  
  #View(map_df)
  
  df_resultat <- map_df %>%
    mutate(
      # Mantenim el codi de 2 lletres (ISO2) 
      iso2 = ifelse(origen == "RER", "ES", substr(origen, 1, 2))
    ) %>%
    mutate(iso2 = trimws(iso2)) %>%
    filter(!is.na(iso2), iso2 != "") %>%
    group_by(iso2) %>%
    summarise(
      value = n_distinct(ingredient),
      llista_ingredients = paste(unique(ingredient), collapse = ", "),
      .groups = "drop"
    )
  
  return(df_resultat)
}

# Funció per dibuixar el mapa sense dependre de cap descàrrega externa
plot_map_final <- function(df, titol) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  # Assegurem que el dataframe estigui en majúscules per fer el match
  df <- df %>% mutate(iso2 = toupper(trimws(iso2)))
  
  highchart(type = "map") %>%
    hc_add_series_map(
      map = worldgeojson, 
      df = df,
      value = "value",
      joinBy = c("iso2", "iso2"), 
      name = "Ingredients",
      borderWidth = 0.5,
      nullColor = "#f0f0f0"
    ) %>%
    hc_colorAxis(
      minColor = "#e8f4fd", 
      maxColor = "#2c3e50"
    ) %>%
    hc_mapNavigation(enabled = TRUE) %>%
    hc_tooltip(
       useHTML = TRUE,
       backgroundColor = "rgba(255, 255, 255, 0.95)",
       borderRadius = 8,
       borderWidth = 1,
       borderColor = "#2c3e50",
       shadow = TRUE,
       headerFormat = "
        <div style='background-color: #2c3e50; color: white; padding: 5px 10px; border-radius: 5px 5px 0 0; margin: -7px -7px 5px -7px;'>
          <span style='font-size: 14px; font-weight: bold;'>{point.name}</span>
        </div>",
               pointFormat = "
        <div style='padding: 5px;'>
          <span style='color: #7f8c8d; font-size: 11px; text-transform: uppercase; font-weight: bold;'>Total Ingredients</span><br>
          <span style='font-size: 18px; color: #2c3e50; font-weight: bold;'>{point.value} from {point.name}</span><br>
          <hr style='margin: 5px 0; border: 0; border-top: 1px solid #eee;'>
          <span style='color: #7f8c8d; font-size: 11px; text-transform: uppercase; font-weight: bold;'>Detall dels ingredients</span><br>
          <span style='font-size: 12px; color: #34495e; line-height: 1.4;'><i>{point.llista_ingredients}</i></span>
        </div>") %>%
    hc_title(text = titol) %>%
    hc_credits(enabled = TRUE, text = "Dades Europa (RER) assignades a Espanya")
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
    labs(title = paste("Diferència:", toupper(gsub("_"," ",imp))), y = "Valor A - B", x = "Dieta", fill = "Resultat") +
    theme_minimal()
  
  ggplotly(p, tooltip = c("text", "y"))
}


########################## PERCETNANGE EMISSIONS ##################################

plot_descomposicio_transport_ingredient <- function(df_dietes, transport_df, impacte_sel) {
  
  # 1. Unim dades 
  df_unificat <- df_dietes %>%
    left_join(transport_df, by = "origen", suffix = c(".diet", ".transp"))
  
  # 2. Identifiquem columnes
  col_diet <- paste0(impacte_sel, ".diet")
  col_transp <- paste0(impacte_sel, ".transp")
  
  # 3. Calculem descomposició
  df_resum <- df_unificat %>%
    group_by(diet) %>%
    summarise(
      # Operació: Sumatori(Impacte * Proporció) i després dividir tot el bloc per 1000
      Transport = sum(!!sym(col_transp) * prop, na.rm = TRUE) / 1000,
      
      # El total de la dieta es manté igual (ja ve calculat per ingredient al teu df)
      Total = sum(!!sym(col_diet), na.rm = TRUE),
      .groups = 'drop',
    
    ) %>%
    mutate(
      
      Ingredient = Total - Transport,
      
      # Seguretat: si el transport calculat és major al total per arrodoniment, posem 0
      Ingredient = pmax(0, Ingredient) 
    ) %>%
    select(diet, Ingredient, Transport) %>%
    pivot_longer(cols = c("Ingredient", "Transport"), 
                 names_to = "Origen", values_to = "Valor")
  
    unitat_actual <- UNITATS[impacte_sel]
  if(is.na(unitat_actual)) unitat_actual <- ""
  
  # 4. Gràfic de barres apilades
  p <- ggplot(df_resum, aes(x = diet, y = Valor, fill = Origen)) +
    geom_col(width = 0.6) +
    scale_fill_manual(values = c("Ingredient" = "#2c3e50", "Transport" = "#e67e22")) +
    scale_y_continuous(
      labels = scales::label_number(accuracy = 0.0001),
      # Escala dinàmica: posem marques cada 0.05 o 0.1 per evitar bloquejos si el valor és gran
      expand = expansion(mult = c(0, 0.1)) 
    ) +
    labs(
      title = paste("Desglossament:", toupper(gsub("_", " ", impacte_sel))),
      x = "", 
      y = paste("Impacte (",unitat_actual,")"), 
      fill = "Origen"
    ) +
    theme_minimal() +
    theme(text = element_text(face = "bold"), legend.position = "bottom")
  
  ggplotly(p)
}


######################## GRAFICAR FUNCIONS ###################################

# Funció per exportar llistes de gràfics de forma dinàmica
exportar_llista_grafics <- function(llista_plots, file_path, n_cols = 3, base_height = 5, base_width = 15) {
  req(length(llista_plots) > 0)
  
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
  
  # 3. Guardem el fitxer
  # L'alçada total dependrà de quantes files hi hagi realment
  ggsave(
    filename = file_path,
    plot = collage,
    width = base_width,
    height = base_height * n_rows,
    units = "in",
    dpi = 300
  )
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