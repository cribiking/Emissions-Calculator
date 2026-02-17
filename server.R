
# ---------------------------
# Server
# ---------------------------

#Ordre del document:

#  - " #--- " : el conjunt de caràcters entre cometes, representa el final d'un bloc de codi


######################

server <- function(input, output, session) {
  
  ###################################### CÀRREGA DE FITXERS ##########################################33
  
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
  
  #---
  
  ################## SELECCIO D'STEPS I FILTRES #################################
  
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
 
  
  ########################## NAVBAR SELECCIO ORIGENS , INGREDIENTS , OVERRIDES, ORDRE DIETES, DOWNLOAD CSV ###############################

  # ordre de dietes (segons arxiu)
  
  ordre_dietes <- reactive({
    df <- dades_dietes()
    req(df)#Si df esta vuit o es NULL, el programa es detè aqui a la espera
    
    # ' %>% ' farà la esxtracció del dataframe, eliminem duplicats i seleccionem valors unics amb 'distinct'
    #'pull' transofrma la columna de la taula en un vector convencional de R
    df %>% distinct(diet) %>% pull(diet)
  })
  
  
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
    
    # Retorna vector d'orígens disponibles per un ingredient basat en dades_env
    oris <- origens_per_ingredient(env, input$sel_ingredient) 
    if(length(oris) == 0) {
      
      #Podem llegir les dades seleccionades per l'usuari amb 'input$sel_origen'
      selectInput("sel_origen", "Orígens disponibles", choices = c("(cap)"), selected = "(cap)")
    } else {
      selectInput("sel_origen", "Orígens disponibles", choices = oris, selected = oris[1])
    }
  })
  
  # solucions: recalc segons overrides i transport
  # mantenim una taula reactiva d'overrides: tibble(ingredient, origen_selected)
  
  #character() , crea un vector de caracters vuit
  #Esta definint una taula amb dues columnes: ingredient i origen_selected
  
  overrides <- reactiveVal(tibble(ingredient = character(0), origen_selected = character(0)))
  
  # Aplica override quan s'apreta el botó 'apply_override' , descrit a la UI
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
 
  ######################################### VISIO GENERAL #######################################################3
  
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
  
  ########################################### COMPOSICIO PER DIETA #########################################
  
  output$plot_comp_A <- renderPlotly({ plot_composicio(solA_joined_transport(), ordre_dietes = ordre_dietes()) })
  output$plot_comp_B <- renderPlotly({ plot_composicio(solB_joined_transport(), ordre_dietes = ordre_dietes()) })
  
  #---
  
  ############################################# IMPACTE PER DIETA ############################################
  
  # Plot impacte A vs B (resum format long)
  output$plot_impacte_AB <- renderUI({
    
    req(input$impactes_sel)
    
    impactes <- input$impactes_sel
    
    plots <- lapply(impactes, function(imp) {
      
      outputId <- paste0("plot_AB_", imp)
      
      output[[outputId]] <- renderPlotly({
        
        if(input$mostrar_per_animal) {
          A <- resumA_animal()
          B <- resumB_animal()
        } else {
          A <- resumA_kg()
          B <- resumB_kg()
        }
        
        both <- bind_rows(
          A %>% mutate(solucio = "A"),
          B %>% mutate(solucio = "B")
        ) %>% 
          filter(impacte == imp)
        
        p <- ggplot(both, aes(x = diet, y = valor, fill = solucio)) +
          geom_col(position = position_dodge(width = 0.9)) +
          coord_flip() +
          labs(
            title = paste("Comparació A vs B –", imp),
            y = "Valor",
            x = "Dieta"
          ) +
          theme_minimal()+
          theme(
            text = element_text(face = "bold"),
            axis.text = element_text(face = "bold"),
            axis.title = element_text(face = "bold"),
            plot.title = element_text(face = "bold"),
            legend.text = element_text(face = "bold"),
            legend.title = element_text(face = "bold")
          )
        
        ggplotly(p)
      })
      
      tagList(
        plotlyOutput(outputId, height = "400px"),
        hr()
      )
    })
    
    do.call(tagList, plots)
  })
  
  #---
  
  ########################################### CONTRIBUCIO PER ORIGEN  ########################################### 
  
  # --- SOLUCIÓ A ---
  output$plot_origen_A_dinamic <- renderUI({
    req(input$impactes_sel)
    impactes <- input$impactes_sel
    
    plots_A <- lapply(impactes, function(imp) {
      # ID únic per a la solució A
      plot_id <- paste0("plot_orig_A_", imp)
      
      output[[plot_id]] <- renderPlotly({
        df_plot <- contribucio_per_origen_from_joined(
          solA_joined_transport(), 
          per_animal = input$mostrar_per_animal,
          kg_table = dades_consum_animals() 
        ) %>%
          filter(impacte == imp)
        
        if (nrow(df_plot) == 0 || all(is.na(df_plot$valor))) {
          return(plotly_empty() %>% layout(title = paste("Sense dades A:", imp)))
        }
        
        p <- ggplot(df_plot, aes(x = diet, y = valor, fill = origen)) +
          geom_col(color = "white", size = 0.2) +
          coord_flip() +
          labs(title = paste("A -", imp), y = "Valor", x = "Dieta") +
          theme_minimal() +
          scale_y_continuous(labels = scales::label_scientific(digits = 2))
        
        ggplotly(p)
      })
      
      tagList(plotlyOutput(plot_id, height = "350px"), br())
    })
    do.call(tagList, plots_A)
  })
  
  # --- SOLUCIÓ B ---
  output$plot_origen_B_dinamic <- renderUI({
    req(input$impactes_sel)
    impactes <- input$impactes_sel
    
    plots_B <- lapply(impactes, function(imp) {
      # ID únic per a la solució B (important canviar la lletra)
      plot_id <- paste0("plot_orig_B_", imp)
      
      output[[plot_id]] <- renderPlotly({
        df_plot <- contribucio_per_origen_from_joined(
          solB_joined_transport(), # Fem servir solB
          per_animal = input$mostrar_per_animal,
          kg_table = dades_consum_animals() 
        ) %>%
          filter(impacte == imp)
        
        if (nrow(df_plot) == 0 || all(is.na(df_plot$valor))) {
          return(plotly_empty() %>% layout(title = paste("Sense dades B:", imp)))
        }
        
        p <- ggplot(df_plot, aes(x = diet, y = valor, fill = origen)) +
          geom_col(color = "white", size = 0.2) +
          coord_flip() +
          labs(title = paste("B -", imp), y = "Valor", x = "Dieta") +
          theme_minimal() +
          scale_y_continuous(labels = scales::label_scientific(digits = 2))
        
        ggplotly(p)
      })
      
      tagList(plotlyOutput(plot_id, height = "350px"), br())
    })
    do.call(tagList, plots_B)
  })
  
  #---
  
  ########################################### TOP INGREDIENTS  ########################################### 
  
  
  # --- TOP INGREDIENTS SOLUCIÓ A ---
  output$plots_topA <- renderUI({
    req(solA_joined_transport(), input$impacte_top)
    diets <- unique(solA_joined_transport()$diet)
    
    plots <- lapply(diets, function(d) {
      outputId <- paste0("topA_", d)
      
      output[[outputId]] <- renderPlotly({
        plot_topN_ingredients_per_dieta(
          joined_df = solA_joined_transport(),
          diet_sel = d,
          impacte_sel = input$impacte_top,
          n = 5,
          per_animal = input$mostrar_per_animal,
          kg_table = kg_table()
        )
      })
      
      # Afegim un títol petit per a cada dieta dins de la llista
      tagList(
        h5(paste("Dieta:", d), style = "color: #34495e; margin-top: 20px;"),
        plotlyOutput(outputId, height = "300px"),
        hr()
      )
    })
    do.call(tagList, plots)
  })
  
  # --- TOP INGREDIENTS SOLUCIÓ B ---
  output$plots_topB <- renderUI({
    req(solB_joined_transport(), input$impacte_top)
    diets <- unique(solB_joined_transport()$diet)
    
    plots <- lapply(diets, function(d) {
      outputId <- paste0("topB_", d)
      
      output[[outputId]] <- renderPlotly({
        plot_topN_ingredients_per_dieta(
          joined_df = solB_joined_transport(),
          diet_sel = d,
          impacte_sel = input$impacte_top,
          n = 5,
          per_animal = input$mostrar_per_animal,
          kg_table = kg_table()
        )
      })
      
      tagList(
        h5(paste("Dieta:", d), style = "color: #34495e; margin-top: 20px;"),
        plotlyOutput(outputId, height = "300px"),
        hr()
      )
    })
    do.call(tagList, plots)
  })
  
  
  #---
  
  ############################################ MAPA  ########################################### 
  
  # 1. Mapes amb validació
  output$map_solA <- renderHighchart({
    req(solA_joined_transport(), dades_env()) # Validació de seguretat
    plot_map_solucio_highcharter(solA_joined_transport(), dades_env())
  })
  
  output$map_solB <- renderHighchart({
    req(solB_joined_transport(), dades_env()) # Validació de seguretat
    plot_map_solucio_highcharter(solB_joined_transport(), dades_env())
  })
  
  # 2. Reactiu d'ingredients faltants (només càlcul)
  ingredients_no_trobats <- reactive({
    req(dades_dietes(), dades_env())
    comprovar_ingredients_faltants(dades_dietes(), dades_env())
  })
  
  # 3. Observador per a la notificació (només surt un cop)
  observeEvent(ingredients_no_trobats(), {
    faltants <- ingredients_no_trobats()
    if(length(faltants) > 0) {
      showNotification(
        paste("Atenció: Falten dades ambientals per a:", paste(faltants, collapse = ", ")),
        type = "warning",
        id = "aviso_faltants", # ID fix per evitar duplicats
        duration = 10 # Millor posar un temps llarg que no "infinit" per no molestar
      )
    }
  })
  
  # 4. Interfície d'avís (es manté igual, és correcta)
  output$aviso_faltantes_ui <- renderUI({
    faltantes <- ingredients_no_trobats()
    if (length(faltantes) == 0) return(NULL)
    
    wellPanel(
      style = "background-color: #f8d7da; border-color: #f5c6cb; color: #721c24; margin-top: 15px;",
      h4(icon("exclamation-triangle"), "Atenció: Ingredients no trobats"),
      p("Els següents ingredients estan a la teva dieta, però no tenen dades associades a l'arxiu ambiental:"),
      tableOutput("tabla_faltantes")
    )
  })
  
  output$tabla_faltantes <- renderTable({
    data.frame(Ingrediente = ingredients_no_trobats())
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  #---
  
  
  ############################################ DISTRIBUCIÓ ############################################ 
  
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
  
  #---
  
  
  ############################################ DIFERENCIA DIETA A-B ############################################ 
  
  # --- COMPARATIVA DE DIFERENCIAS (A - B) ---
  output$plot_diff <- renderUI({
    req(input$impactes_sel)
    
    lapply(input$impactes_sel, function(imp) {
      plot_id <- paste0("diff_", imp)
      output[[plot_id]] <- renderPlotly({
        A <- if(input$mostrar_per_animal) resumA_animal() else resumA_kg()
        B <- if(input$mostrar_per_animal) resumB_animal() else resumB_kg()
        plot_diferencies_AB(A, B, imp)
      })
      plotlyOutput(plot_id, height = "350px")
    }) %>% do.call(tagList, .)
  })
  
  
  #---
   
  ################### CRIDA A LA FUNCIO calcula_solucio_amb_transport ####################################
  
  # solucions calculades (joined) amb transport i overrides
  
  solA_joined_transport <- reactive({
    
    req(dades_env(), dades_dietes(), transport_df()  ,input$stepA)
    
    calculate <- calcula_solucio_amb_transport(dades_dietes(), dades_env(), input$stepA,
                                               overrides_df = overrides(), transport_df = transport_df(),
                                               ordre_dietes = ordre_dietes())
    
    validate(need(nrow(calculate) > 0, "Solució A (step) no té dades o hi ha un error"))
    
    return(calculate)
  })
  
  solB_joined_transport <- reactive({
    
    req(dades_env(), dades_dietes(), transport_df() ,input$stepB)
    
    calculate <- calcula_solucio_amb_transport(dades_dietes(), dades_env(), input$stepB,
                                               overrides_df = overrides(), transport_df = transport_df(),
                                               ordre_dietes = ordre_dietes())
    
    validate(need(nrow(calculate) > 0, "Solució B (step) no té dades o hi ha un error"))
    
    return(calculate)
  })
  
  
  # Ara el resum DEPÈN directament de solA_joined_transport()
  resumA_kg <- reactive({
    req(solA_joined_transport()) # Ens assegurem que el càlcul ja està fet
    
    # Passem el dataframe JA CALCULAT a la funció de resum
    resum_per_dieta_from_joined(solA_joined_transport(), per_animal = FALSE)
  })
  
  resumB_kg <- reactive({ 
    req(solB_joined_transport()) # Ens assegurem que el càlcul ja està fet
    
    resum_per_dieta_from_joined(solB_joined_transport(), per_animal = FALSE)
  })
  
  
  resumA_animal <- reactive({ resum_per_dieta_from_joined(solA_joined_transport(), per_animal = TRUE, kg_table = kg_table()) })
  resumB_animal <- reactive({ resum_per_dieta_from_joined(solB_joined_transport(), per_animal = TRUE, kg_table = kg_table()) })
  
  #Outputs VISIO GENERAL amb transport
  
  output$tbl_dietes_A <- renderDT({ solA_joined_transport() %>% distinct(diet) %>% datatable(options = list(pageLength = 10)) })
  output$tbl_dietes_B <- renderDT({ solB_joined_transport() %>% distinct(diet) %>% datatable(options = list(pageLength = 10)) })
  
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
 
  

  
  #---------------------------------------
 
  
  
  ## no utilitzat, revisar per a que serveix 'plot_impacte_A_vs_B'
  
  output$plot_impacte_A_vs_B <- renderPlotly({
    if(input$mostrar_per_animal) {
      plot_impacte_A_vs_B_generic(resumA_animal(), resumB_animal(), impactes_sel = input$impactes_sel, per_animal = TRUE)
    } else {
      plot_impacte_A_vs_B_generic(resumA_kg(), resumB_kg(), impactes_sel = input$impactes_sel, per_animal = FALSE)
    }
  })
  
  

  
  
}
