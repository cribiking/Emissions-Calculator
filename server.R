
# ---------------------------
# Server
# ---------------------------

# ============================================================
# GUIA D'ORGANITZACIÓ (alineada amb ui.R)
# ============================================================
# 00. Càrrega de fitxers i validacions d'entrada
# 01. Controls de capçalera (steps, ordres, export resum)
# 01.B Configuració avançada superior (overrides)
# 01.C Nucli base de càlcul (solucions A/B amb transport)
# 02. Pestanya Visió general
# 03. Pestanya Composició per dieta
# 04. Pestanya Impactes per dieta
# 05. Pestanya Contribució per origen
# 06. Pestanya Top ingredients
# 07. Pestanya Mapa d'orígens
# 08. Pestanya Distribució
# 09. Pestanya Diferència A-B
# 10. Pestanya Desglossament impacte
# 11. Pestanya Contribució total (per animal)
# 13. Pestanya Environmental Footprint i comparatives globals
#
# Nota: els separadors "#---" marquen el final d'un bloc funcional.




server <- function(input, output, session) {

  # Cargar librerías necesarias para Gemini ANTES de cargar las funciones
  suppressPackageStartupMessages({
    require(httr, quietly = TRUE)
    require(jsonlite, quietly = TRUE)
    require(base64enc, quietly = TRUE)
    require(markdown, quietly = TRUE)
  })

  # Cargar funciones de Gemini
  source('gemini_functions.R', encoding = 'UTF-8')

  ############################## 00. CÀRREGA DE FITXERS ###############################################
  
  dades_env <- reactive({
    req(input$file_env)
    
    # CORRECCIÓN: Definir la ruta del archivo primero
    ruta_archivo <- input$file_env$datapath
    
    tryCatch({
      # 1. Verificar el número de hojas ANTES de cargar los datos
      hojas <- readxl::excel_sheets(ruta_archivo)
      
      if(length(hojas) > 1) {
        showNotification(
          paste("Avis: L'arxiu conté", length(hojas), "fulls. només es processarà la primera:", hojas[1]),
          type = "warning", 
          duration = 10
        )
      }
      
      # 2. Llamar a tu función de carga
      carrega_dades_ambientals(ruta_archivo)
      
    }, error = function(e) {
      # 3. Notificación de error si algo falla (ej. faltan columnas)
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
  
  #Rebem el dataframe correctament
  transport_df <- reactive({
    
    req(input$file_transport)
    
    tryCatch({
      carrega_transport(input$file_transport$datapath)
    }, error = function(e) {
      showNotification(paste("Error fitxer transport:", e$message), type = "error")
      NULL
    })
  })
  
  dades_env_totals <- reactive({
    req(input$file_env)
    
    ruta_archivo <- input$file_env$datapath
    
    tryCatch({
      hojas <- readxl::excel_sheets(ruta_archivo)
      
      if(length(hojas) > 1) {
        showNotification(
          paste("Avis: L'arxiu conté", length(hojas), "fulls. només es processarà la primera:", hojas[1]),
          type = "warning", 
          duration = 10
        )
      }
      
      carrega_dades_ambientals_totals(ruta_archivo)
      
    }, error = function(e) {
      showNotification(paste("Error fitxer ambientals:", e$message), type = "error")
      NULL
    })
  })
  
  environmental_df <- reactive({
    req(input$file_env)
    
    tryCatch({
      df <- carrega_emissions(input$file_env$datapath)
      
      missing_in_sheet2 <- setdiff(TOTAL_IMPACT_NAMES, df$impact_category)
      
      if(length(missing_in_sheet2) > 0) {
        showNotification(
          paste("Fulla 2: Falten factors per a:", paste(missing_in_sheet2, collapse = ", ")),
          type = "warning",
          duration = 15
        )
      }
      
      df
    }, error = function(e) {
      showNotification(paste("Error fitxer ambientals (Fulla 2):", e$message), type = "error")
      NULL
    })
  })
  
  #---
  
  ############################## 01. CONTROLS DE CAPÇALERA ############################################
  
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
  
  #---
 
  
  #################### 01.A CONTROLS TRANSVERSALS (ORDRE DIETES + EXPORT RESUM) #######################

  # ordre de dietes (segons arxiu)
  
  ordre_dietes <- reactive({
    df <- dades_dietes()
    req(df)#Si df esta vuit o es NULL, el programa es detè aqui a la espera
    
    # ' %>% ' farà la esxtracció del dataframe, eliminem duplicats i seleccionem valors unics amb 'distinct'
    #'pull' transofrma la columna de la taula en un vector convencional de R
    df %>% distinct(diet) %>% pull(diet)
  })

  ################### 01.B CONFIGURACIÓ AVANÇADA: OVERRIDES (UI SUPERIOR) #############################

  # --- 1. Selector d'Ingredients (Només els que tenen > 1 origen) ---
  output$sel_ingredient_ui <- renderUI({
    req(dades_env())

    df_resum_origens <- origens_per_ingredient(dades_env())

    # Filtrem ingredients amb múltiples opcions
    ingredients_amb_opcions <- df_resum_origens %>%
      filter(lengths(origen) > 1) %>%
      pull(ingredient) # Pull retorna el vector de noms

    selectInput("sel_ingredient",
                label = "Ingredient:",
                choices = ingredients_amb_opcions)
  })

  # --- 2. Selector d'Orígens (Dinàmic segons l'ingredient triat) ---
  output$sel_origen_ui <- renderUI({
    req(input$sel_ingredient, dades_env())

    # Tornem a cridar la funció resumida
    df_resum <- origens_per_ingredient(dades_env())

    # Busquem el vector d'orígens de l'ingredient seleccionat
    llista_origens <- df_resum %>%
      filter(ingredient == input$sel_ingredient) %>%
      pull(origen) %>%
      unlist() # Convertim la llista de la cel·la en un vector normal

    selectInput("sel_origen",
                label = paste("Orígens per a", input$sel_ingredient),
                choices = llista_origens)
  })

  # AL aplicar override, reiniciarem la taula que conte els calculs de les emissions totals amb el transport amb els nous paisos
  #El control de si apliquem o no overrides es fa dins de la funcio de global 'calcula_solucio_amb_transport'
  observeEvent(input$apply_override, {
    req(input$sel_ingredient, input$sel_origen)

    ing <- input$sel_ingredient
    orig <- input$sel_origen

    # 1. Llegim el contingut actual del reactiveVal
    current_overrides <- OVERRIDES()

    # 2. Creem la nova fila
    nova_fila <- tibble(ingredient = ing, origen_selected = orig)

    # 3. Lògica d'actualització:
    # Si l'ingredient ja existia, l'eliminem primer per evitar duplicats
    # i després afegim la nova selecció.
    updated_overrides <- current_overrides %>%
      filter(ingredient != ing) %>%
      bind_rows(nova_fila)

    # 4. Guardem el nou tibble dins del reactiveVal
    OVERRIDES(updated_overrides)

    # Opcional: Print per consola per verificar que s'ha guardat bé
    print(OVERRIDES())

    showNotification(
      paste0("Override aplicat: ", ing, " -> ", orig),
      type = "message"
    )
  })

  # Reset OVERRIDES
  observeEvent(input$reset_overrides, {

    # Tornem a posar el tibble buit amb l'estructura de columnes exacta
    OVERRIDES(tibble(ingredient = character(0), origen_selected = character(0)))

    showNotification("Overrides eliminats. Recalculant dades inicials...", type = "message")
  })

  #---

  ##################### 01.C NUCLI BASE: SOLUCIONS A/B AMB TRANSPORT ################################

  # solucions calculades (joined) amb transport i OVERRIDES

  # Aquesta funció s'executarà automàticament quan:
  # 1. Canviïn els fitxers carregats
  # 2. Canviï el 'step' al header
  # 3. EXECUTIS OVERRIDES(updated_overrides) en el teu observeEvent

  solA_joined_transport <- reactive({

    req(dades_env(), dades_dietes(), transport_df()  ,input$stepA)

    # 2. Fem una crida a OVERRIDES() aquí dins.
    # Això crea el "vincle". Quan facis el reset, aquesta funció es despertarà.
    actual_overrides <- OVERRIDES()


    calculate <- calcula_solucio_amb_transport(dades_dietes(),
                                               dades_env(),
                                               input$stepA,
                                               overrides_df = actual_overrides,
                                               transport_df = transport_df(),
                                               ordre_dietes = ordre_dietes()
    )

    # Verificar si calculate es valido y tiene datos
    if(is.null(calculate) || !is.data.frame(calculate) || nrow(calculate) == 0) {
      return(NULL)  # Retornar NULL, req() en los outputs manejará esto
    }

    return(calculate)
  })

  solB_joined_transport <- reactive({

    req(dades_env(), dades_dietes(), transport_df() ,input$stepB)

    # 2. Fem una crida a OVERRIDES() aquí dins.
    # Això crea el "vincle". Quan facis el reset, aquesta funció es despertarà.
    actual_overrides <- OVERRIDES()

    calculate <- calcula_solucio_amb_transport(dades_dietes(),
                                               dades_env(),
                                               input$stepB,
                                               overrides_df = actual_overrides,
                                               transport_df = transport_df(),
                                               ordre_dietes = ordre_dietes()
    )

    # Verificar si calculate es valido y tiene datos
    if(is.null(calculate) || !is.data.frame(calculate) || nrow(calculate) == 0) {
      return(NULL)  # Retornar NULL, req() en los outputs manejará esto
    }

    return(calculate)
  })

  #---------------------------------------

  ## no utilitzat, revisar per a que serveix 'plot_impacte_A_vs_B'
  output$plot_impacte_A_vs_B <- renderPlotly({
    if(input$mostrar_per_animal) {
      plot_impacte_A_vs_B_generic(resumA_animal(), resumB_animal(), impactes_sel = input$impactes_sel, per_animal = TRUE)
    } else {
      plot_impacte_A_vs_B_generic(resumA_kg(), resumB_kg(), impactes_sel = input$impactes_sel, per_animal = FALSE)
    }
  })
  
  

  # DOWNLOAD CSV
  
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
  
  #---
 
  ############################## 02. PESTANYA: VISIÓ GENERAL ###########################################
  
  #Creació valueBox per visualitzar dades a visio general
  
  lapply(box_config, function(config) {
    output[[config$id]] <- renderValueBox({
      # Acceder a los datos reactivos
      env <- dades_env()
      diets <- dades_dietes()
      
      # Seguridad: req() detiene la ejecución si no hay datos
      req(env, diets)
      
      # Lógica de extracción de datos
      datos <- if(config$source == "env") env else diets
      valor <- length(unique(datos[[config$var]]))
      
      valueBox(
        value = valor,
        subtitle = config$title,
        color = config$color
      )
    })
  })
  
  #---
  
  ############################## 03. PESTANYA: COMPOSICIÓ PER DIETA ####################################

  llista_plots_composicio <- reactive({
    # Verificar que ambos datasets tienen datos (no solo que existan)
    data_a <- solA_joined_transport()
    data_b <- solB_joined_transport()

    # Si alguno está vacío o es NULL, retornar lista vacía
    if(is.null(data_a) || nrow(data_a) == 0 || is.null(data_b) || nrow(data_b) == 0) {
      return(list())
    }

    list(
      plot_composicio_gg(data_a, ordre_dietes = ordre_dietes()) +
        labs(title = "Estructura Solucio A"),
      plot_composicio_gg(data_b, ordre_dietes = ordre_dietes()) +
        labs(title = "Estructura Solucio B")
    )
  })

  output$plot_comp_A <- renderPlotly({
    data_a <- solA_joined_transport()
    req(data_a)  # Detener si es NULL
    plot_composicio(data_a, ordre_dietes = ordre_dietes())
  })

  output$plot_comp_B <- renderPlotly({
    data_b <- solB_joined_transport()
    req(data_b)  # Detener si es NULL
    plot_composicio(data_b, ordre_dietes = ordre_dietes())
  })

  output$download_comp <- downloadHandler(
    filename = function() {
      paste0("report_composicio_", Sys.Date(), ".png")
    },
    content = function(file) {
      exportar_llista_grafics(
        llista_plots = llista_plots_composicio(),
        file_path = file,
        n_cols = 1
      )
    }
  )
  
  #---
  
  
  
  
  # Ara el resum DEPÈN directament de solA_joined_transport()
  resumA_kg <- reactive({
    req(solA_joined_transport()) # Ens assegurem que el càlcul ja està fet
    
    # Passem el dataframe JA CALCULAT a la funció de resum
    resum_per_dieta_from_joined(solA_joined_transport())
  })
  
  resumB_kg <- reactive({ 
    req(solB_joined_transport()) # Ens assegurem que el càlcul ja està fet
    
    resum_per_dieta_from_joined(solB_joined_transport())
  })
  
  
  ############################## 04. PESTANYA: IMPACTES PER DIETA ######################################
  
  # Aquesta llista s'actualitza sola quan canvien els inputs o les dades
  llista_plots_comparatius <- reactive({
    req(input$impactes_sel, resumA_kg(), resumB_kg())
    
    lapply(input$impactes_sel, function(imp) {
      unitat_actual <- UNITATS[imp]
      if(is.na(unitat_actual)) unitat_actual <- ""
      
      both <- bind_rows(
        resumA_kg() %>% mutate(solucio = "A"),
        resumB_kg() %>% mutate(solucio = "B")
      ) %>% filter(impacte == imp)
      
      # Creem l'objecte ggplot pur
      p <- ggplot(both, aes(x = diet, y = valor, fill = solucio)) +
        geom_col(position = position_dodge(width = 0.9)) +
        scale_fill_manual(values = c("A" = "#3498db", "B" = "#e67e22")) +
        coord_flip() +
        labs(
          title = paste("Comparació A vs B –", toupper(gsub("_"," ",imp))),
          y = paste("Valor (", unitat_actual, ")"),
          x = "Dieta"
        ) +
        theme_minimal() +
        theme(text = element_text(face = "bold"))
      
      return(p) # Guardem el ggplot a la llista
    })
  })
  
  output$plot_impacte_AB <- renderUI({
    req(llista_plots_comparatius())
    
    plots_obj <- llista_plots_comparatius()
    impactes <- input$impactes_sel
    
    plots_ui <- lapply(seq_along(plots_obj), function(i) {
      outputId <- paste0("plot_AB_", impactes[i])
      
      # Renderitzem per a la web (Plotly)
      output[[outputId]] <- renderPlotly({
        ggplotly(plots_obj[[i]])
      })
      
      tagList(
        plotlyOutput(outputId, height = "400px"),
        hr()
      )
    })
    
    do.call(tagList, plots_ui)
  })
  

  
  output$download_plots_AB <- downloadHandler(
    filename = function() { 
      paste0("report_impacte_AB_", Sys.Date(), ".png") 
    },
    content = function(file) {
      # Simplement cridem la funció genèrica!
      exportar_llista_grafics(
        llista_plots = llista_plots_comparatius(), # L'array dinàmic que ja tens
        file_path = file,
        n_cols = 3
      )
    }
  )
  
  #---
  
  ############################## 05. PESTANYA: CONTRIBUCIÓ PER ORIGEN ##################################
  
  # --- SOLUCIÓ A ---
  output$plot_origen_A_dinamic <- renderUI({
    req(input$impactes_sel)
    impactes <- input$impactes_sel
    
    plots_A <- lapply(impactes, function(imp) {
      # El 'local' és imprescindible quan usem renderPlotly dins de lapply
      local({
        current_imp <- imp
        plot_id <- paste0("plot_orig_A_", current_imp)
        
        output[[plot_id]] <- renderPlotly({
          df_plot <- contribucio_per_origen_from_joined(solA_joined_transport()) %>%
            filter(impacte == current_imp)
          
          if (nrow(df_plot) == 0 || all(is.na(df_plot$valor))) {
            return(plotly_empty() %>% layout(title = paste("Sense dades A:", current_imp)))
          }
          
          # Accés a la variable global UNITATS definida a global.R
          unitat_actual <- UNITATS[current_imp] 
          if(is.na(unitat_actual)) unitat_actual <- ""
          
          p <- ggplot(df_plot, aes(x = diet, y = valor, fill = origen)) +
            geom_col(color = "white", linewidth = 0.2) + # 'linewidth' en lloc de 'size'
            scale_fill_manual(values = colors_paisos) +
            coord_flip() +
            labs(
              title = paste("Solució A -", toupper(gsub("_"," ",current_imp))), 
              y = paste("Valor (", unitat_actual, ")"),
              x = "Etapa Dieta",
              fill = "Origen"
            ) +
            theme_minimal() +
            theme(text = element_text(face = "bold")) +
            scale_y_continuous(labels = scales::label_scientific(digits = 2))
          
          ggplotly(p)
        })
        
        tagList(plotlyOutput(plot_id, height = "350px"), br())
      })
    })
    do.call(tagList, plots_A)
  })
  
  # --- SOLUCIÓ B ---
  output$plot_origen_B_dinamic <- renderUI({
    req(input$impactes_sel)
    impactes <- input$impactes_sel
    
    plots_B <- lapply(impactes, function(imp) {
      local({
        current_imp <- imp
        plot_id <- paste0("plot_orig_B_", current_imp)
        
        output[[plot_id]] <- renderPlotly({
          df_plot <- contribucio_per_origen_from_joined(solB_joined_transport()) %>%
            filter(impacte == current_imp)
          
          if (nrow(df_plot) == 0 || all(is.na(df_plot$valor))) {
            return(plotly_empty() %>% layout(title = paste("Sense dades B:", current_imp)))
          }
          
          # També afegim les unitats a la Solució B
          unitat_actual <- UNITATS[current_imp] 
          if(is.na(unitat_actual)) unitat_actual <- ""
          
          p <- ggplot(df_plot, aes(x = diet, y = valor, fill = origen)) +
            geom_col(color = "white", linewidth = 0.2) +
            scale_fill_manual(values = colors_paisos) +
            coord_flip() +
            labs(
              title = paste("Solució B -", toupper(gsub("_"," ",current_imp))), 
              y = paste("Valor (", unitat_actual, ")"), 
              x = "Etapa Dieta",
              fill = "Origen"
            ) +
            theme_minimal() +
            theme(text = element_text(face = "bold")) +
            scale_y_continuous(labels = scales::label_scientific(digits = 2))
          
          ggplotly(p)
        })
        
        tagList(plotlyOutput(plot_id, height = "350px"), br())
      })
    })
    do.call(tagList, plots_B)
  })

  llista_plots_origen <- reactive({
    req(input$impactes_sel, solA_joined_transport(), solB_joined_transport())

    crear_plot_origen <- function(df_joined, impacte_sel, etiqueta_solucio) {
      df_plot <- contribucio_per_origen_from_joined(df_joined) %>%
        filter(impacte == impacte_sel)

      if (nrow(df_plot) == 0 || all(is.na(df_plot$valor))) return(NULL)

      unitat_actual <- UNITATS[impacte_sel]
      if (is.na(unitat_actual)) unitat_actual <- ""

      ggplot(df_plot, aes(x = diet, y = valor, fill = origen)) +
        geom_col(color = "white", linewidth = 0.2) +
        scale_fill_manual(values = colors_paisos) +
        coord_flip() +
        labs(
          title = paste(etiqueta_solucio, "-", toupper(gsub("_", " ", impacte_sel))),
          y = paste("Valor (", unitat_actual, ")"),
          x = "Etapa Dieta",
          fill = "Origen"
        ) +
        theme_minimal() +
        theme(text = element_text(face = "bold")) +
        scale_y_continuous(labels = scales::label_scientific(digits = 2))
    }

    plots_intercalats <- unlist(lapply(input$impactes_sel, function(imp) {
      list(
        crear_plot_origen(solA_joined_transport(), imp, "Solució A"),
        crear_plot_origen(solB_joined_transport(), imp, "Solució B")
      )
    }), recursive = FALSE)

    Filter(Negate(is.null), plots_intercalats)
  })

  output$download_origen <- downloadHandler(
    filename = function() {
      paste0("report_origen_", Sys.Date(), ".png")
    },
    content = function(file) {
      plots <- llista_plots_origen()
      validate(need(length(plots) > 0, "No hi ha gràfics disponibles per exportar."))

      exportar_llista_grafics(
        llista_plots = plots,
        file_path = file,
        n_cols = 2
      )
    }
  )
  
  #---
  
  ############################## 06. PESTANYA: TOP INGREDIENTS #########################################
  
  
  # --- TOP INGREDIENTS SOLUCIÓ A ---
  output$plots_topA <- renderUI({
    req(solA_joined_transport(), input$impacte_top)
    
    # Recuperem la unitat del vector global UNITATS segons l'impacte seleccionat
    u <- UNITATS[input$impacte_top]
    if(is.na(u)) u <- ""
    
    # Obtenim les dietes com a vector (imprescindible per al lapply)
    diets <- solA_joined_transport() %>%
      pull(diet) %>%
      unique()
    
    plots <- lapply(diets, function(d) {
      outputId <- paste0("topA_", d)
      
      output[[outputId]] <- renderPlotly({
        plot_topN_ingredients_per_dieta(
          joined_df = solA_joined_transport(),
          diet_sel = d,
          impacte_sel = input$impacte_top,
          n = 5,
          per_animal = input$mostrar_per_animal,
          kg_table = kg_table(),
          bar_color = "#2c3e50", # Blau fosc per a la Solució A,
          unitat_text = u  # <--- Passem la unitat aquí
        )
      })
      
      tagList(
        h5(paste("Dieta:", d), style = "color: #2c3e50; font-weight: bold; margin-top: 20px;"),
        plotlyOutput(outputId, height = "300px"),
        hr()
      )
    })
    do.call(tagList, plots)
  })
  
  # --- TOP INGREDIENTS SOLUCIÓ B ---
  output$plots_topB <- renderUI({
    req(solB_joined_transport(), input$impacte_top)
    
    # Recuperem la unitat del vector global UNITATS segons l'impacte seleccionat
    u <- UNITATS[input$impacte_top]
    if(is.na(u)) u <- ""
    
    # Obtenim les dietes com a vector
    diets <- solB_joined_transport() %>%
      pull(diet) %>%
      unique()
    
    plots <- lapply(diets, function(d) {
      outputId <- paste0("topB_", d)
      
      output[[outputId]] <- renderPlotly({
        plot_topN_ingredients_per_dieta(
          joined_df = solB_joined_transport(),
          diet_sel = d,
          impacte_sel = input$impacte_top,
          n = 5,
          per_animal = input$mostrar_per_animal,
          kg_table = kg_table(),
          bar_color = "#95a5a6",# Gris per a la Solució B
          unitat_text = u  
        )
      })
      
      tagList(
        h5(paste("Dieta:", d), style = "color: #7f8c8d; font-weight: bold; margin-top: 20px;"),
        plotlyOutput(outputId, height = "300px"),
        hr()
      )
    })
    do.call(tagList, plots)
  })

  llista_plots_top <- reactive({
    req(solA_joined_transport(), solB_joined_transport(), input$impacte_top, kg_table())

    u <- UNITATS[input$impacte_top]
    if (is.na(u)) u <- ""

    diets_A <- solA_joined_transport() %>% pull(diet) %>% unique()
    diets_B <- solB_joined_transport() %>% pull(diet) %>% unique()

    diets_comunes <- intersect(diets_A, diets_B)

    plots_intercalats <- unlist(lapply(diets_comunes, function(d) {
      list(
        plot_topN_ingredients_per_dieta_gg(
          joined_df = solA_joined_transport(),
          diet_sel = d,
          impacte_sel = input$impacte_top,
          n = 5,
          per_animal = input$mostrar_per_animal,
          kg_table = kg_table(),
          bar_color = "#2c3e50",
          unitat_text = u
        ),
        plot_topN_ingredients_per_dieta_gg(
          joined_df = solB_joined_transport(),
          diet_sel = d,
          impacte_sel = input$impacte_top,
          n = 5,
          per_animal = input$mostrar_per_animal,
          kg_table = kg_table(),
          bar_color = "#95a5a6",
          unitat_text = u
        )
      )
    }), recursive = FALSE)

    # Afegim dietes que només existeixin en una solució al final, mantenint consistència.
    diets_nom_A <- setdiff(diets_A, diets_comunes)
    diets_nom_B <- setdiff(diets_B, diets_comunes)

    plots_extra_A <- lapply(diets_nom_A, function(d) {
      plot_topN_ingredients_per_dieta_gg(
        joined_df = solA_joined_transport(),
        diet_sel = d,
        impacte_sel = input$impacte_top,
        n = 5,
        per_animal = input$mostrar_per_animal,
        kg_table = kg_table(),
        bar_color = "#2c3e50",
        unitat_text = u
      )
    })

    plots_extra_B <- lapply(diets_nom_B, function(d) {
      plot_topN_ingredients_per_dieta_gg(
        joined_df = solB_joined_transport(),
        diet_sel = d,
        impacte_sel = input$impacte_top,
        n = 5,
        per_animal = input$mostrar_per_animal,
        kg_table = kg_table(),
        bar_color = "#95a5a6",
        unitat_text = u
      )
    })

    Filter(Negate(is.null), c(plots_intercalats, plots_extra_A, plots_extra_B))
  })

  output$download_top <- downloadHandler(
    filename = function() {
      paste0("report_top_ingredients_", Sys.Date(), ".png")
    },
    content = function(file) {
      plots <- llista_plots_top()
      validate(need(length(plots) > 0, "No hi ha gràfics disponibles per exportar."))

      exportar_llista_grafics(
        llista_plots = plots,
        file_path = file,
        n_cols = 2
      )
    }
  )
  
  
  #---
  
  ############################## 07. PESTANYA: MAPA D'ORÍGENS ##########################################
  # Mapa Solució A
  output$map_solA <- renderHighchart({
    req(solA_joined_transport(), dades_env())

    map_df_base <- solA_joined_transport() %>%
      # 1. Normalitzem text per seguretat (Majúscules i sense espais)
      mutate(
        ingredient = toupper(trimws(ingredient)),
        origen = toupper(trimws(origen))
      ) %>%
      # 2. Seleccionem les dues columnes que t'interessen
      select(ingredient, origen) %>%
      # 3. Agafem les combinacions úniques (Distinct)
      distinct()
    
    
    df_mapa <- preparar_dades_mapa_full(map_df_base)
    plot_map_final(df_mapa, "Origen ingredients – Solució A")
  })
  
  # Mapa Solució B
  output$map_solB <- renderHighchart({
    req(solB_joined_transport(), dades_env())

    map_df_base <- solB_joined_transport() %>%
      # 1. Normalitzem text per seguretat (Majúscules i sense espais)
      mutate(
        ingredient = toupper(trimws(ingredient)),
        origen = toupper(trimws(origen))
      ) %>%
      # 2. Seleccionem les dues columnes que t'interessen
      select(ingredient, origen) %>%
      # 3. Agafem les combinacions úniques (Distinct)
      distinct()

    df_mapa <- preparar_dades_mapa_full(map_df_base)
    plot_map_final(df_mapa, "Origen ingredients – Solució B")
  })

  llista_plots_mapes <- reactive({
    req(solA_joined_transport(), solB_joined_transport(), dades_env())

    map_df_base_A <- solA_joined_transport() %>%
      mutate(
        ingredient = toupper(trimws(ingredient)),
        origen = toupper(trimws(origen))
      ) %>%
      select(ingredient, origen) %>%
      distinct()

    map_df_base_B <- solB_joined_transport() %>%
      mutate(
        ingredient = toupper(trimws(ingredient)),
        origen = toupper(trimws(origen))
      ) %>%
      select(ingredient, origen) %>%
      distinct()

    df_mapa_A <- preparar_dades_mapa_full(map_df_base_A)
    df_mapa_B <- preparar_dades_mapa_full(map_df_base_B)

    list(
      plot_map_static_gg(df_mapa_A, "Origen ingredients - Solució A"),
      plot_map_static_gg(df_mapa_B, "Origen ingredients - Solució B")
    )
  })

  output$download_mapes <- downloadHandler(
    filename = function() {
      paste0("report_mapes_", Sys.Date(), ".png")
    },
    content = function(file) {
      exportar_llista_grafics(
        llista_plots = llista_plots_mapes(),
        file_path = file,
        n_cols = 2
      )
    }
  )
  
  ############################## 08. PESTANYA: DISTRIBUCIÓ ##############################################

  llista_plots_distribucio <- reactive({
    req(input$impactes_sel, resumA_kg(), resumB_kg())

    dfA <- resumA_kg() %>% mutate(solucio = "A")
    dfB <- resumB_kg() %>% mutate(solucio = "B")

    lapply(input$impactes_sel, function(imp) {
      unitat_actual <- UNITATS[imp]
      if (is.na(unitat_actual)) unitat_actual <- ""

      both_filtered <- bind_rows(dfA, dfB) %>%
        filter(impacte == imp)

      ggplot(both_filtered, aes(x = solucio, y = valor, fill = solucio)) +
        geom_boxplot(alpha = 0.7, outlier.colour = "red") +
        geom_jitter(width = 0.1, alpha = 0.3) +
        scale_fill_manual(values = c("A" = "#f8766d", "B" = "#00bfc4")) +
        labs(
          title = paste("Distribució:", toupper(gsub("_", " ", imp))),
          y = paste("Valor (", unitat_actual, ")"),
          x = "Solució"
        ) +
        theme_minimal() +
        theme(legend.position = "none")
    })
  })
  
  output$plot_box_ui <- renderUI({
    req(llista_plots_distribucio(), input$impactes_sel)

    plots_obj <- llista_plots_distribucio()
    impactes <- input$impactes_sel

    plots_list <- lapply(seq_along(plots_obj), function(i) {
      imp <- impactes[i]
      plot_id <- paste0("box_", imp)

      output[[plot_id]] <- renderPlotly({
        ggplotly(plots_obj[[i]])
      })

      # Cada gràfic va dins d'un contenidor amb espaiat
      div(style = "margin-bottom: 30px; padding: 10px; border: 1px solid #eee; border-radius: 5px; background: white;",
          plotlyOutput(plot_id, height = "400px")
      )
    })
    
    do.call(tagList, plots_list)
  })

  output$download_distribucio <- downloadHandler(
    filename = function() {
      paste0("report_distribucio_", Sys.Date(), ".png")
    },
    content = function(file) {
      exportar_llista_grafics(
        llista_plots = llista_plots_distribucio(),
        file_path = file,
        n_cols = 2
      )
    }
  )
  
  #---
  
  
  ############################## 09. PESTANYA: DIFERÈNCIA A-B ###########################################

  llista_plots_diff <- reactive({
    req(input$impactes_sel, resumA_kg(), resumB_kg())
    validate(need(input$stepA != input$stepB, "No hi ha diferències a exportar si compares la mateixa etapa."))

    A <- resumA_kg()
    B <- resumB_kg()

    lapply(input$impactes_sel, function(imp) {
      plot_diferencies_AB_gg(A, B, imp)
    })
  })
  
  # --- COMPARATIVA DE DIFERENCIAS (A - B) ---
  output$plot_diff <- renderUI({
    req(input$impactes_sel)
    
    # 1. Validació: Si les etapes són idèntiques, mostrem un avís i aturem l'execució
    if(input$stepA == input$stepB) {
      return(
        div(style = "padding: 20px; text-align: center;",
            h4(icon("exclamation-triangle"), "No hi ha diferències a mostrar", 
               style = "color: #e67e22; font-weight: bold;"),
            p("Estàs comparant la mateixa etapa (", strong(input$stepA), "). 
             Selecciona etapes o orígens diferents per veure la comparativa.")
        )
      )
    }
    
    # 2. Generem la llista de gràfics si les etapes són diferents
    plots <- lapply(input$impactes_sel, function(imp) {
      plot_id <- paste0("diff_", imp)
      
      output[[plot_id]] <- renderPlotly({
        # Ens assegurem que tenim les dades resumides
        A <- resumA_kg()
        B <- resumB_kg()
        
        # Cridem a la teva funció de global
        plot_diferencies_AB(A, B, imp)
      })
      
      # Embolcall de cada gràfic per mantenir l'estètica de targetes
      div(style = "margin-bottom: 30px; padding: 15px; background: white; border-radius: 8px; box-shadow: 0 2px 5px rgba(0,0,0,0.1);",
          plotlyOutput(plot_id, height = "450px")
      )
    })
    
    do.call(tagList, plots)
  })

  output$download_diff <- downloadHandler(
    filename = function() {
      paste0("report_diferencies_AB_", Sys.Date(), ".png")
    },
    content = function(file) {
      plots <- llista_plots_diff()
      validate(need(length(plots) > 0, "No hi ha gràfics disponibles per exportar."))

      exportar_llista_grafics(
        llista_plots = plots,
        file_path = file,
        n_cols = 2
      )
    }
  )
  
  #---
  
  ######################### 09.A SUPORT: VERIFICACIÓ D'INGREDIENTS FALTANTS ############################
  
  
  # --- Lògica d'ingredients faltants al Server ---
  
  # 1. Reactiu que calcula la llista d'ingredients faltants
  # Es dispararà automàticament quan canviïn els fitxers de dietes o ambientals
  ingredients_no_trobats <- reactive({
    req(dades_dietes(), dades_env()) # Espera que els fitxers estiguin carregats
    
    # Cridem a la teva funció definida al Global.R
    faltants <- comprovar_ingredients_faltants(dades_dietes(), dades_env())
    return(faltants)
  })
  
  # 2. Observador per enviar una notificació emergent (toast) si hi ha errors
  observeEvent(ingredients_no_trobats(), {
    faltants <- ingredients_no_trobats()
    if(length(faltants) > 0) {
      showNotification(
        paste("Atenció: Falten dades ambientals per a", length(faltants), "ingredients."),
        type = "warning",
        id = "aviso_faltants",
        duration = 10
      )
    }
  })
  
  # 3. Renderització de la interfície d'avís (per a la pestanya de Verificació)
  output$aviso_faltantes_ui <- renderUI({
    faltants <- ingredients_no_trobats()
    
    # Si tot és correcte, mostrem un missatge positiu
    if (length(faltants) == 0) {
      return(
        div(style = "color: #155724; background-color: #d4edda; padding: 15px; border-radius: 5px; border: 1px solid #c3e6cb;",
            icon("check-circle"), " Tots els ingredients de la dieta han estat trobats a la base de dades ambiental.")
      )
    }
    
    # Si falten dades, mostrem el panell d'alerta vermell
    wellPanel(
      style = "background-color: #f8d7da; border-color: #f5c6cb; color: #721c24; margin-top: 15px;",
      h4(icon("exclamation-triangle"), "Atenció: Ingredients no trobats"),
      p("Els següents ingredients apareixen a les dietes però no tenen valors d'impacte assignats:"),
      tableOutput("tabla_faltantes")
    )
  })
  
  # 4. Taula detallada d'ingredients faltants
  output$tabla_faltantes <- renderTable({
    req(ingredients_no_trobats())
    data.frame(Ingredients_Faltants = ingredients_no_trobats())
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  
  #---
  
  
  ############################## 10. PESTANYA: DESGLOSSAMENT IMPACTE ###################################

  llista_plots_desglossament <- reactive({
    req(solA_joined_transport(), solB_joined_transport(), transport_df(), input$impactes_sel)

    unlist(lapply(input$impactes_sel, function(imp) {
      list(
        plot_descomposicio_transport_ingredient_gg(
          df_dietes = solA_joined_transport(),
          transport_df = transport_df(),
          impacte_sel = imp
        ) + labs(title = paste("Solució A -", toupper(gsub("_", " ", imp)))),
        plot_descomposicio_transport_ingredient_gg(
          df_dietes = solB_joined_transport(),
          transport_df = transport_df(),
          impacte_sel = imp
        ) + labs(title = paste("Solució B -", toupper(gsub("_", " ", imp))))
      )
    }), recursive = FALSE)
  })
  
  # --- Representació dels Totals per Emissió (A vs B) ---
  # --- Representació dels Totals per Emissió (A vs B) ---
  output$plots_totals_emissio <- renderUI({
    # Validem que totes les dades necessàries estiguin carregades
    req(solA_joined_transport(), solB_joined_transport(), transport_df(), input$impactes_sel)
    
    # Generem la llista de gràfics comparatius
    plots_list <- lapply(input$impactes_sel, function(imp) {
      
      # IDs únics per a cada gràfic
      id_A <- paste0("total_bar_A_", imp)
      id_B <- paste0("total_bar_B_", imp)
      
      # Renderitzem Gràfic A
      output[[id_A]] <- renderPlotly({
        plot_descomposicio_transport_ingredient(
          df_dietes = solA_joined_transport(), 
          transport_df = transport_df(), 
          impacte_sel = imp
        )
      })
      
      # Renderitzem Gràfic B
      output[[id_B]] <- renderPlotly({
        plot_descomposicio_transport_ingredient(
          df_dietes = solB_joined_transport(), 
          transport_df = transport_df(), 
          impacte_sel = imp
        )
      })
      
      # Estructura de la UI: Títol de l'impacte i dues columnes
      tagList(
        div(style = "margin-bottom: 50px; padding: 20px; border: 1px solid #ddd; border-radius: 12px; background: #ffffff; box-shadow: 0 4px 6px rgba(0,0,0,0.05);",
            h3(paste("Impacte:", toupper(gsub("_", " ", imp))), 
               style = "text-align: center; color: #2c3e50; font-weight: bold; margin-bottom: 25px; border-bottom: 2px solid #eee; padding-bottom: 10px;"),
            
            fluidRow(
              column(6, 
                     h4("Solució A", style = "text-align: center; color: #2c3e50; font-weight: bold;"),
                     plotlyOutput(id_A, height = "400px")
              ),
              column(6, 
                     h4("Solució B", style = "text-align: center; color: #7f8c8d; font-weight: bold;"),
                     plotlyOutput(id_B, height = "400px")
              )
            )
        )
      )
    })
    
    do.call(tagList, plots_list)
  })

  output$download_desglossament <- downloadHandler(
    filename = function() {
      paste0("report_desglossament_", Sys.Date(), ".png")
    },
    content = function(file) {
      exportar_llista_grafics(
        llista_plots = llista_plots_desglossament(),
        file_path = file,
        n_cols = 2
      )
    }
  )
   
  
 
  
  
  #Outputs VISIO GENERAL amb transport
  
  output$tbl_dietes_A <- renderDT({ solA_joined_transport() %>% distinct(diet) %>% datatable(options = list(pageLength = 10)) })
  output$tbl_dietes_B <- renderDT({ solB_joined_transport() %>% distinct(diet) %>% datatable(options = list(pageLength = 10)) })
  
  #---
  

  ############################## 11. PESTANYA: CONTRIBUCIÓ TOTAL (PER ANIMAL) ##########################
  
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
  #---
  
  
  ###### Resum per Animal #######
  
  #df multiplicat per kg que es consumeix en cada etapa (entrada, creixement....)
  
  resumA_animal <- reactive({ calcul_contribucio_total_per_animal(solA_joined_transport(), kg_table()) })
  resumB_animal <- reactive({ calcul_contribucio_total_per_animal(solB_joined_transport(), kg_table()) })
  
  #---
  
  ########################## 11.A GRÀFICS CONTRIBUCIÓ TOTAL A/B ########################################

  llista_plots_contrib_total <- reactive({
    req(input$impactes_sel, resumA_animal(), resumB_animal())

    lapply(input$impactes_sel, function(current_imp) {
      unitat_actual <- UNITATS[current_imp]
      if (is.na(unitat_actual)) unitat_actual <- ""

      both <- bind_rows(
        resumA_animal() %>% mutate(solucio = "Solució A"),
        resumB_animal() %>% mutate(solucio = "Solució B")
      ) %>%
        filter(impacte == current_imp)

      both$diet <- factor(both$diet, levels = c("ENTRADA", "CREIXEMENT", "ENGREIX", "ACABAT"))

      totals <- both %>%
        group_by(solucio) %>%
        summarise(total_val = sum(valor, na.rm = TRUE), .groups = "drop")

      ggplot(both, aes(x = solucio, y = valor)) +
        geom_col(aes(fill = diet), width = 0.7, color = "white") +
        geom_text(
          data = totals,
          aes(x = solucio, y = total_val, label = round(total_val, 2)),
          hjust = -0.2,
          fontface = "bold",
          inherit.aes = FALSE
        ) +
        scale_fill_brewer(palette = "Set2") +
        coord_flip() +
        scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
        labs(
          title = paste("Comparació d'Impacte:", toupper(gsub("_", " ", current_imp))),
          y = paste("Valor Acumulat (", unitat_actual, ")"),
          x = "",
          fill = "Etapa Dieta"
        ) +
        theme_minimal() +
        theme(
          text = element_text(face = "bold"),
          axis.text = element_text(face = "bold", size = 10),
          plot.title = element_text(hjust = 0.5)
        )
    })
  })
  
  output$plot_contribucio_total_AB <- renderUI({
    req(input$impactes_sel)
    impactes <- input$impactes_sel
    
    plots <- lapply(impactes, function(imp) {
      local({
        current_imp <- imp
        outputId <- paste0("total_plot_AB", current_imp)
        
        # Busquem la unitat corresponent
        unitat_actual <- UNITATS[current_imp]
        if(is.na(unitat_actual)) unitat_actual <- ""
        
        output[[outputId]] <- renderPlotly({
          A <- resumA_animal()
          B <- resumB_animal()
          
          both <- bind_rows(
            A %>% mutate(solucio = "Solució A"),
            B %>% mutate(solucio = "Solució B")
          ) %>% 
            filter(impacte == current_imp)
          
          # Mantenir l'ordre correcte de les etapes
          both$diet <- factor(both$diet, levels = c("ENTRADA", "CREIXEMENT", "ENGREIX", "ACABAT"))
          
          # Calculem el total per a cada solució per poder posar l'etiqueta de text
          totals <- both %>%
            group_by(solucio) %>%
            summarise(total_val = sum(valor, na.rm = TRUE))
          
          p <- ggplot(both, aes(x = solucio, y = valor)) +
            # Barres apilades per etapa
            geom_col(aes(fill = diet), width = 0.7, color = "white") + 
            # Afegim el text del TOTAL
            geom_text(
              data = totals, 
              aes(x = solucio, y = total_val, label = round(total_val, 2)),
              hjust = -0.2, # Ajust horitzontal per separar-lo de la barra (ja que hi ha coord_flip)
              fontface = "bold",
              inherit.aes = FALSE # Evitem que busqui la variable 'fill' en el dataframe de totals
            ) +
            scale_fill_brewer(palette = "Set2") +
            coord_flip() +
            # Expandim l'eix Y (que és l'horitzontal per coord_flip) perquè el text no quedi tallat
            scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
            labs(
              title = paste("Comparació d'Impacte:", toupper(gsub("_"," ",current_imp))),
              y = paste("Valor Acumulat (", unitat_actual, ")"),
              x = "",
              fill = "Etapa Dieta"
            ) +
            theme_minimal() +
            theme(
              text = element_text(face = "bold"),
              axis.text = element_text(face = "bold", size = 10),
              plot.title = element_text(hjust = 0.5)
            )
          
          ggplotly(p) %>% layout(legend = list(orientation = "h", x = 0, y = -0.2))
        })
        
        tagList(
          plotlyOutput(outputId, height = "350px"),
          hr()
        )
      })
    })
    
    do.call(tagList, plots)
  })

  output$download_contrib_total <- downloadHandler(
    filename = function() {
      paste0("report_contribucio_total_", Sys.Date(), ".png")
    },
    content = function(file) {
      exportar_llista_grafics(
        llista_plots = llista_plots_contrib_total(),
        file_path = file,
        n_cols = 2
      )
    }
  )

  
  #---
  
  
  
  ############################## 13. PESTANYA: ENVIRONMENTAL FOOTPRINT ##################################
  
  rv_verify_impacts <- reactive({
    req(dades_env_totals(), environmental_df())
    fn_verify_impacts(dades_env_totals(), environmental_df())
  })
  
  rv_verify_ingredients <- reactive({
    req(dades_dietes(), dades_env_totals())
    fn_verify_ingredients(dades_dietes(), dades_env_totals())
  })
  
  output$verify_footprint_panel <- renderUI({
    
    req(rv_verify_impacts(), rv_verify_ingredients())
    
    v_imp <- rv_verify_impacts()
    v_ing <- rv_verify_ingredients()
    
    tagList(
      h3(icon("leaf"), " Verificació de Categories d'Impacte"),
      p("Comparació entre les columnes de la Fulla 1 i les categories de la Fulla 2"),
      
      fluidRow(
        column(4,
               div(style = "background: #d4edda; padding: 15px; border-radius: 8px; text-align: center;",
                   h4(style = "color: #155724; margin: 0;", length(v_imp$coincidents)),
                   p(style = "color: #155724; margin: 5px 0 0 0;", "Coincidents")
               )
        ),
        column(4,
               div(style = "background: #fff3cd; padding: 15px; border-radius: 8px; text-align: center;",
                   h4(style = "color: #856404; margin: 0;", length(v_imp$only_sheet1)),
                   p(style = "color: #856404; margin: 5px 0 0 0;", "Només a Fulla 1")
               )
        ),
        column(4,
               div(style = "background: #cce5ff; padding: 15px; border-radius: 8px; text-align: center;",
                   h4(style = "color: #004085; margin: 0;", length(v_imp$only_sheet2)),
                   p(style = "color: #004085; margin: 5px 0 0 0;", "Només a Fulla 2")
               )
        )
      ),
      
      br(),
      
      if(length(v_imp$only_sheet1) > 0 || length(v_imp$only_sheet2) > 0) {
        wellPanel(
          style = "background: #f8f9fa;",
          if(length(v_imp$only_sheet1) > 0) {
            tagList(
              strong("⚠️ Columnes a Fulla 1 sense factor a Fulla 2:"),
              br(),
              tags$code(paste(v_imp$only_sheet1, collapse = ", ")),
              br(), br()
            )
          },
          if(length(v_imp$only_sheet2) > 0) {
            tagList(
              strong("ℹ️ Categories a Fulla 2 sense columna a Fulla 1:"),
              br(),
              tags$code(paste(v_imp$only_sheet2, collapse = ", "))
            )
          }
        )
      },
      
      hr(),
      
      h3(icon("carrot"), " Verificació d'Ingredients"),
      p("Comparació entre els ingredients del fitxer de dietes i la Fulla 1 d'ambientals"),
      
      fluidRow(
        column(4,
               div(style = "background: #d4edda; padding: 15px; border-radius: 8px; text-align: center;",
                   h4(style = "color: #155724; margin: 0;", length(v_ing$coincidents)),
                   p(style = "color: #155724; margin: 5px 0 0 0;", "Coincidents")
               )
        ),
        column(4,
               div(style = paste0("background: ", 
                                  ifelse(length(v_ing$falten_a_ambientals) > 0, "#f8d7da", "#d4edda"), 
                                  "; padding: 15px; border-radius: 8px; text-align: center;"),
                   h4(style = paste0("color: ", 
                                     ifelse(length(v_ing$falten_a_ambientals) > 0, "#721c24", "#155724"), 
                                     "; margin: 0;"), 
                      length(v_ing$falten_a_ambientals)),
                   p(style = paste0("color: ", 
                                    ifelse(length(v_ing$falten_a_ambientals) > 0, "#721c24", "#155724"), 
                                    "; margin: 5px 0 0 0;"), 
                     "Falten a Ambientals")
               )
        ),
        column(4,
               div(style = "background: #e2e3e5; padding: 15px; border-radius: 8px; text-align: center;",
                   h4(style = "color: #383d41; margin: 0;", length(v_ing$sobren_a_ambientals)),
                   p(style = "color: #383d41; margin: 5px 0 0 0;", "No utilitzats")
               )
        )
      ),
      
      br(),
      
      if(length(v_ing$falten_a_ambientals) > 0) {
        div(style = "background: #f8d7da; border: 1px solid #f5c6cb; padding: 15px; border-radius: 8px; margin-top: 10px;",
            h4(style = "color: #721c24; margin-top: 0;", 
               icon("exclamation-triangle"), " ATENCIÓ: Ingredients sense dades ambientals"),
            p(style = "color: #721c24;", 
              "Els següents ingredients apareixen a les dietes però NO tenen valors d'impacte:"),
            tags$ul(
              lapply(v_ing$falten_a_ambientals, function(x) tags$li(tags$code(x)))
            )
        )
      } else {
        div(style = "background: #d4edda; border: 1px solid #c3e6cb; padding: 15px; border-radius: 8px; margin-top: 10px;",
            icon("check-circle"), 
            strong(" Tots els ingredients de les dietes tenen dades ambientals assignades.")
        )
      },
      
      if(length(v_ing$sobren_a_ambientals) > 0) {
        div(style = "background: #e2e3e5; border: 1px solid #d6d8db; padding: 15px; border-radius: 8px; margin-top: 10px;",
            tags$details(
              tags$summary(style = "cursor: pointer; color: #383d41;", 
                           icon("info-circle"), 
                           paste(" ", length(v_ing$sobren_a_ambientals), "ingredients a ambientals no utilitzats (clic per veure)")
              ),
              br(),
              tags$code(style = "font-size: 11px;", paste(v_ing$sobren_a_ambientals, collapse = ", "))
            )
        )
      }
    )
  })
  
  ########################### 13.A COMPARATIVA A/B (STEPS SELECCIONATS) ################################
  
  # Gràfic comparatiu de les DUES dietes seleccionades (Step A vs Step B)
  output$plot_comparativa_AB <- renderPlotly({
    
    res <- rv_contribucio_totes()
    req(input$stepA, input$stepB)
    
    if(res$error) return(NULL)
    
    # Filtrem només els steps seleccionats
    df_plot <- res$resum_per_dieta %>%
      filter(step %in% c(input$stepA, input$stepB)) %>%
      mutate(
        step_label = case_when(
          step == input$stepA ~ paste("A (Step", input$stepA, ")"),
          step == input$stepB ~ paste("B (Step", input$stepB, ")"),
          TRUE ~ as.character(step)
        ),
        step_label = factor(step_label, levels = c(
          paste("A (Step", input$stepA, ")"),
          paste("B (Step", input$stepB, ")")
        )),
        diet = factor(diet, levels = c("ACABAT", "ENGREIX", "CREIXEMENT", "ENTRADA"))
      )
    
    # Colors per fase
    colors_fases <- c(
      "ENTRADA" = "#66c2a5",
      "CREIXEMENT" = "#fc8d62", 
      "ENGREIX" = "#8da0cb",
      "ACABAT" = "#e78ac3"
    )
    
    # Creem el gràfic directament amb plotly
    plot_ly(
      df_plot,
      x = ~petjada_total,
      y = ~step_label,
      color = ~diet,
      colors = colors_fases,
      type = "bar",
      orientation = "h",
      hovertemplate = paste(
        "<b>%{y}</b><br>",
        "Fase: %{data.name}<br>",
        "Valor: %{x:.6f} Pt<extra></extra>"
      )
    ) %>%
      layout(
        barmode = "stack",
        title = list(
          text = "<b>Comparació Petjada Ambiental: Solució A vs Solució B</b>",
          font = list(size = 16),
          y = 0.95
        ),
        xaxis = list(
          title = ""
        ),
        yaxis = list(
          title = "",
          categoryorder = "array",
          categoryarray = c(
            paste("B (Step", input$stepB, ")"),
            paste("A (Step", input$stepA, ")")
          )
        ),
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = -0.35,
          traceorder = "reversed"
        ),
        margin = list(l = 120, r = 50, t = 80, b = 120)
      )
  })
  
  output$tbl_comparativa_AB <- renderDT({
    
    res <- rv_contribucio_totes()
    req(input$stepA, input$stepB)
    
    if(res$error) return(NULL)
    
    taula <- res$resum_per_dieta %>%
      filter(step %in% c(input$stepA, input$stepB)) %>%
      mutate(
        solucio = case_when(
          step == input$stepA ~ "Solució A",
          step == input$stepB ~ "Solució B"
        )
      ) %>%
      select(solucio, diet, petjada_total) %>%
      pivot_wider(
        names_from = diet,
        values_from = petjada_total,
        values_fill = 0
      ) %>%
      rowwise() %>%
      mutate(TOTAL = sum(c_across(where(is.numeric)), na.rm = TRUE)) %>%
      ungroup()
    
    datatable(
      taula %>%
        mutate(across(where(is.numeric), ~ format_num_pt(.x, 6))),
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        ordering = FALSE
      ),
      rownames = FALSE,
      caption = paste("Comparativa Step", input$stepA, "vs Step", input$stepB)
    ) %>%
      formatStyle('TOTAL', backgroundColor = '#c8e6c9', fontWeight = 'bold') %>%
      formatStyle('solucio', fontWeight = 'bold')
  })
  
  output$info_diferencia_AB <- renderUI({
    
    res <- rv_contribucio_totes()
    req(input$stepA, input$stepB)
    
    if(res$error) return(NULL)
    
    total_A <- res$resum_per_step %>%
      filter(step == input$stepA) %>%
      pull(petjada_total_step)
    
    total_B <- res$resum_per_step %>%
      filter(step == input$stepB) %>%
      pull(petjada_total_step)
    
    if(length(total_A) == 0) total_A <- 0
    if(length(total_B) == 0) total_B <- 0
    
    if(total_A < total_B) {
      millor <- "A"
      color_millor <- "#4caf50"
      # Quant menys té A respecte a B
      percentatge <- ((total_B - total_A) / total_B) * 100
    } else if(total_B < total_A) {
      millor <- "B"
      color_millor <- "#4caf50"
      # Quant menys té B respecte a A
      percentatge <- ((total_A - total_B) / total_A) * 100
    } else {
      millor <- "Iguals"
      color_millor <- "#9e9e9e"
      percentatge <- 0
    }
    
    tagList(
      fluidRow(
        column(4,
               div(style = "background: #e3f2fd; padding: 15px; border-radius: 8px; text-align: center;",
                   h5(style = "margin: 0; color: #1565c0;", paste("Solució A (Step", input$stepA, ")")),
                   h3(style = "margin: 10px 0; color: #0d47a1;", format_num_pt(total_A, 6))
               )
        ),
        column(4,
               div(style = "background: #fff3e0; padding: 15px; border-radius: 8px; text-align: center;",
                   h5(style = "margin: 0; color: #e65100;", paste("Solució B (Step", input$stepB, ")")),
                   h3(style = "margin: 10px 0; color: #bf360c;", format_num_pt(total_B, 6))
               )
        ),
        column(4,
               div(style = paste0("background: ", color_millor, "22; padding: 15px; border-radius: 8px; text-align: center; border: 2px solid ", color_millor, ";"),
                   h5(style = paste0("margin: 0; color: ", color_millor, ";"), "Millor opció"),
                   h3(style = paste0("margin: 10px 0; color: ", color_millor, ";"), 
                      if(millor == "Iguals") "=" else paste("Solució", millor)),
                   p(style = "font-size: 12px; margin: 0;", 
                     if(millor != "Iguals") paste0(abs(round(percentatge, 1)), "% menys impacte") else "")
               )
        )
      )
    )
  })
  
  ############################ 13.B CÀLCUL GLOBAL DE TOTS ELS STEPS ####################################
  
  rv_contribucio_totes <- reactive({
    req(dades_dietes(), dades_env_totals(), environmental_df())
    
    fn_calcul_contribucio_totes_dietes(
      df_dietes = dades_dietes(),
      df_env_totals = dades_env_totals(),
      df_emissions = environmental_df()
    )
  })
  
  output$tbl_resum_steps <- renderDT({
    
    res <- rv_contribucio_totes()
    
    if(res$error) {
      return(datatable(data.frame(Error = res$missatge), options = list(dom = 't')))
    }
    
    datatable(
      res$resum_per_step %>%
        mutate(
          step = as.integer(step),
          petjada_total_step = format_num_pt(petjada_total_step, 6)
        ) %>%
        arrange(step),
      colnames = c("Step", "Petjada Total"),
      extensions = 'Buttons',
      rownames = FALSE,
      options = list(
        pageLength = 15,
        dom = 'Bfrtip',
        buttons = c('copy', 'excel')
      ),
      caption = "Petjada ambiental total per cada Step"
    ) %>%
      formatStyle('petjada_total_step', backgroundColor = '#c8e6c9', fontWeight = 'bold')
  })
  
  output$tbl_resum_dietes <- renderDT({
    
    res <- rv_contribucio_totes()
    
    if(res$error) {
      return(datatable(data.frame(Error = res$missatge), options = list(dom = 't')))
    }
    
    taula_pivot <- res$resum_per_dieta %>%
      pivot_wider(
        names_from = diet,
        values_from = petjada_total,
        values_fill = 0
      ) %>%
      arrange(step) %>%
      mutate(
        step = as.integer(step),
        across(-step, ~ format_num_pt(.x, 6)))
    
    datatable(
      taula_pivot,
      extensions = 'Buttons',
      rownames = FALSE,
      options = list(
        pageLength = 15,
        dom = 'Bfrtip',
        buttons = c('copy', 'excel'),
        scrollX = TRUE
      ),
      caption = "Petjada ambiental per Step i Fase de creixement"
    )
  })
  
  output$tbl_detall_impactes <- renderDT({
    
    res <- rv_contribucio_totes()
    
    if(res$error) {
      return(datatable(data.frame(Error = res$missatge), options = list(dom = 't')))
    }
    
    datatable(
      res$resum_per_dieta_impacte %>%
        mutate(
          step = as.integer(step),
          total_ponderada = format_num_pt(total_ponderada, 6)
        ) %>%
        arrange(step, diet, impacte),
      colnames = c("Step", "Fase", "Impacte", "Total Ponderat"),
      extensions = 'Buttons',
      rownames = FALSE,
      options = list(
        pageLength = 20,
        dom = 'Bfrtip',
        buttons = c('copy', 'excel'),
        scrollX = TRUE
      ),
      filter = "top"
    )
  })
  
  output$plot_comparativa_steps <- renderPlotly({
    
    res <- rv_contribucio_totes()
    
    if(res$error) return(NULL)
    
    df_plot <- res$resum_per_dieta %>%
      mutate(
        step = factor(step),
        diet = factor(diet, levels = c("ENTRADA", "CREIXEMENT", "ENGREIX", "ACABAT"))
      )
    
    colors_fases <- c(
      "ENTRADA" = "#66c2a5",
      "CREIXEMENT" = "#fc8d62", 
      "ENGREIX" = "#8da0cb",
      "ACABAT" = "#e78ac3"
    )
    
    plot_ly(
      df_plot,
      x = ~petjada_total,
      y = ~step,
      color = ~diet,
      colors = colors_fases,
      type = "bar",
      orientation = "h",
      hovertemplate = paste(
        "<b>Step %{y}</b><br>",
        "Fase: %{data.name}<br>",
        "Valor: %{x:.6f} Pt<extra></extra>"
      )
    ) %>%
      layout(
        barmode = "stack",
        title = list(
          text = "<b>Petjada Ambiental per Step</b><br><sub>Desglossament per fase de creixement</sub>",
          font = list(size = 16)
        ),
        xaxis = list(
          title = list(
            text = "Petjada Total Ponderada (Pt)",
            font = list(size = 12),
            standoff = 30
          )
        ),
        yaxis = list(
          title = "Step"
        ),
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = -0.15,
          traceorder = "reversed"
        ),
        margin = list(l = 80, r = 50, t = 80, b = 100)
      )
  })
  
  output$info_totes_dietes <- renderUI({
    
    res <- rv_contribucio_totes()
    
    if(res$error) {
      return(div(style = "color: red;", icon("exclamation-triangle"), res$missatge))
    }
    
    tagList(
      div(style = "background: #e3f2fd; padding: 20px; border-radius: 10px; margin-bottom: 20px;",
          fluidRow(
            column(4,
                   h5("Steps (Solucions)", style = "margin: 0; color: #1565c0;"),
                   h3(res$n_steps, style = "margin: 5px 0;")
            ),
            column(4,
                   h5("Fases de Creixement", style = "margin: 0; color: #1565c0;"),
                   h3(res$n_dietes, style = "margin: 5px 0;")
            ),
            column(4,
                   h5("Total Combinacions", style = "margin: 0; color: #1565c0;"),
                   h3(res$n_combinacions, style = "margin: 5px 0;")
            )
          )
      )
    )
  })

  ################################################################################
  # HANDLERS PARA BOTONES DE EXPLICACIÓN CON IA
  ################################################################################

  # Helper function para capturar gráficos del entorno
  get_plots_from_env <- function(plot_names) {
    plots <- list()
    for (name in plot_names) {
      if (exists(name, envir = .GlobalEnv)) {
        plots[[name]] <- get(name, envir = .GlobalEnv)
      }
    }
    return(plots)
  }

  # AI Explicar Composició
  observeEvent(input$ai_explain_comp, {
    tryCatch({
      showNotification("Consultant Gemini...", type = "message", duration = NULL, id = "ai_loading_comp")
      plots <- list(plot_comp_A(), plot_comp_B())
      respuesta <- preparar_grafics_per_gemini(plots,
        context_adicional = "Concentra't en les diferències de composició entre les dues solucions.",
        n_cols = 2)
      removeNotification("ai_loading_comp")
      output$ai_response_comp <- renderUI({
        HTML(markdown::markdownToHTML(text = respuesta, fragment.only = TRUE))
      })
    }, error = function(e) {
      removeNotification("ai_loading_comp")
      output$ai_response_comp <- renderUI({
        div(style = "color: red; padding: 10px; border: 1px solid red; border-radius: 5px;",
            HTML(paste("Error consultando IA:", e$message)))
      })
    })
  })

  # AI Explicar Impactes
  observeEvent(input$ai_explain_impactes, {
    tryCatch({
      showNotification("Consultant Gemini...", type = "message", duration = NULL, id = "ai_loading_impactes")
      plots <- llista_plots_comparatius()
      if (length(plots) > 0) {
        respuesta <- preparar_grafics_per_gemini(plots,
          context_adicional = "Analitza els impactes seleccionats i identifica els patrons més rellevants entre les dues solucions.",
          n_cols = 3)
      } else {
        respuesta <- "No hi ha gràfics disponibles. Assegura't que has seleccionat cap a menys una métrica d'impacte."
      }
      removeNotification("ai_loading_impactes")
      output$ai_response_impactes <- renderUI({
        HTML(markdown::markdownToHTML(text = respuesta, fragment.only = TRUE))
      })
    }, error = function(e) {
      removeNotification("ai_loading_impactes")
      output$ai_response_impactes <- renderUI({
        div(style = "color: red; padding: 10px; border: 1px solid red; border-radius: 5px;",
            HTML(paste("Error consultando IA:", e$message)))
      })
    })
  })

  # AI Explicar Origen
  observeEvent(input$ai_explain_origen, {
    tryCatch({
      showNotification("Consultant Gemini...", type = "message", duration = NULL, id = "ai_loading_origen")
      plots <- llista_plots_origen()
      if (length(plots) > 0) {
        respuesta <- preparar_grafics_per_gemini(plots,
          context_adicional = "Analitza la distribució geogràfica dels orígens. Identifica els countries que més contribueixen als impactes i el seu pes relatiu.",
          n_cols = 2)
      } else {
        respuesta <- "No hi ha gràfics disponibles per a origen."
      }
      removeNotification("ai_loading_origen")
      output$ai_response_origen <- renderUI({
        HTML(markdown::markdownToHTML(text = respuesta, fragment.only = TRUE))
      })
    }, error = function(e) {
      removeNotification("ai_loading_origen")
      output$ai_response_origen <- renderUI({
        div(style = "color: red; padding: 10px; border: 1px solid red; border-radius: 5px;",
            HTML(paste("Error consultando IA:", e$message)))
      })
    })
  })

  # AI Explicar Top Ingredients
  observeEvent(input$ai_explain_top, {
    tryCatch({
      showNotification("Consultant Gemini...", type = "message", duration = NULL, id = "ai_loading_top")
      plots <- llista_plots_top()
      if (length(plots) > 0) {
        respuesta <- preparar_grafics_per_gemini(plots,
          context_adicional = "Identifica els ingredients que més contribueixen als impactes seleccionats. Proposa alternatives més sostenibles.",
          n_cols = 2)
      } else {
        respuesta <- "No hi ha gràfics disponibles per a top ingredients. Selecciona cap a menys una métrica."
      }
      removeNotification("ai_loading_top")
      output$ai_response_top <- renderUI({
        HTML(markdown::markdownToHTML(text = respuesta, fragment.only = TRUE))
      })
    }, error = function(e) {
      removeNotification("ai_loading_top")
      output$ai_response_top <- renderUI({
        div(style = "color: red; padding: 10px; border: 1px solid red; border-radius: 5px;",
            HTML(paste("Error consultando IA:", e$message)))
      })
    })
  })

  # AI Explicar Mapes
  observeEvent(input$ai_explain_mapes, {
    tryCatch({
      showNotification("Consultant Gemini...", type = "message", duration = NULL, id = "ai_loading_mapes")
      plots <- llista_plots_mapes()
      if (length(plots) > 0) {
        respuesta <- preparar_grafics_per_gemini(plots,
          context_adicional = "Analitza els mapes geogràfics. Identifica les regions amb mayor impacte i proposa estratègies de sourcing més sostenible.",
          n_cols = 1)
      } else {
        respuesta <- "No hi ha mapes disponibles."
      }
      removeNotification("ai_loading_mapes")
      output$ai_response_mapes <- renderUI({
        HTML(markdown::markdownToHTML(text = respuesta, fragment.only = TRUE))
      })
    }, error = function(e) {
      removeNotification("ai_loading_mapes")
      output$ai_response_mapes <- renderUI({
        div(style = "color: red; padding: 10px; border: 1px solid red; border-radius: 5px;",
            HTML(paste("Error consultando IA:", e$message)))
      })
    })
  })

  # AI Explicar Distribució
  observeEvent(input$ai_explain_distribucio, {
    tryCatch({
      showNotification("Consultant Gemini...", type = "message", duration = NULL, id = "ai_loading_distribucio")
      plots <- llista_plots_distribucio()
      if (length(plots) > 0) {
        respuesta <- preparar_grafics_per_gemini(plots,
          context_adicional = "Analitza la distribució estadística dels impactes. Identifica outliers i dispels de variabilitat entre dietes.",
          n_cols = 3)
      } else {
        respuesta <- "No hi ha gràfics de distribució disponibles."
      }
      removeNotification("ai_loading_distribucio")
      output$ai_response_distribucio <- renderUI({
        HTML(markdown::markdownToHTML(text = respuesta, fragment.only = TRUE))
      })
    }, error = function(e) {
      removeNotification("ai_loading_distribucio")
      output$ai_response_distribucio <- renderUI({
        div(style = "color: red; padding: 10px; border: 1px solid red; border-radius: 5px;",
            HTML(paste("Error consultando IA:", e$message)))
      })
    })
  })

  # AI Explicar Diferència A-B
  observeEvent(input$ai_explain_diff, {
    tryCatch({
      showNotification("Consultant Gemini...", type = "message", duration = NULL, id = "ai_loading_diff")
      plots <- llista_plots_diff()
      if (length(plots) > 0) {
        respuesta <- preparar_grafics_per_gemini(plots,
          context_adicional = "Compara les dues solucions (Step A vs Step B). Identifica quins impactes milloren, quins empitjoren, i quines són les causes principals.",
          n_cols = 2)
      } else {
        respuesta <- "No hi ha diferències per mostrar. Assegura't que has seleccionat dues solucions diferents."
      }
      removeNotification("ai_loading_diff")
      output$ai_response_diff <- renderUI({
        HTML(markdown::markdownToHTML(text = respuesta, fragment.only = TRUE))
      })
    }, error = function(e) {
      removeNotification("ai_loading_diff")
      output$ai_response_diff <- renderUI({
        div(style = "color: red; padding: 10px; border: 1px solid red; border-radius: 5px;",
            HTML(paste("Error consultando IA:", e$message)))
      })
    })
  })

  # AI Explicar Desglossament
  observeEvent(input$ai_explain_desglossament, {
    tryCatch({
      showNotification("Consultant Gemini...", type = "message", duration = NULL, id = "ai_loading_desglossament")
      plots <- llista_plots_desglossament()
      if (length(plots) > 0) {
        respuesta <- preparar_grafics_per_gemini(plots,
          context_adicional = "Desglossa l'impacte en dues components: producció (ingredients) vs transport. Analitza quina part és més significativa i per quin motiu.",
          n_cols = 2)
      } else {
        respuesta <- "No hi ha gràfics de desglossament disponibles."
      }
      removeNotification("ai_loading_desglossament")
      output$ai_response_desglossament <- renderUI({
        HTML(markdown::markdownToHTML(text = respuesta, fragment.only = TRUE))
      })
    }, error = function(e) {
      removeNotification("ai_loading_desglossament")
      output$ai_response_desglossament <- renderUI({
        div(style = "color: red; padding: 10px; border: 1px solid red; border-radius: 5px;",
            HTML(paste("Error consultando IA:", e$message)))
      })
    })
  })

  # AI Explicar Contribució Total
  observeEvent(input$ai_explain_contrib_total, {
    tryCatch({
      showNotification("Consultant Gemini...", type = "message", duration = NULL, id = "ai_loading_contrib_total")
      plots <- llista_plots_contrib_total()
      if (length(plots) > 0) {
        respuesta <- preparar_grafics_per_gemini(plots,
          context_adicional = "Analitza la contribució total de cada animal al impacte ambiental. Identifica quins animals (per fase de creixement) són més o menys sostenibles.",
          n_cols = 2)
      } else {
        respuesta <- "No hi ha gràfics de contribució total disponibles."
      }
      removeNotification("ai_loading_contrib_total")
      output$ai_response_contrib_total <- renderUI({
        HTML(markdown::markdownToHTML(text = respuesta, fragment.only = TRUE))
      })
    }, error = function(e) {
      removeNotification("ai_loading_contrib_total")
      output$ai_response_contrib_total <- renderUI({
        div(style = "color: red; padding: 10px; border: 1px solid red; border-radius: 5px;",
            HTML(paste("Error consultando IA:", e$message)))
      })
    })
  })

}
