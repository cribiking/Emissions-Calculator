
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
  
  
  
  #VISIO GENERAL
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
  
  # ordre de dietes (segons arxiu)
  
  #reactive serveix per actualaitzar les dades automaticament, si 'dades_dietes()' canvia
  ordre_dietes <- reactive({
    df <- dades_dietes()
    req(df)#Si df esta vuit o es NULL, el programa es detè aqui a la espera
    
    # ' %>% ' farà la esxtracció del dataframe, eliminem duplicats i seleccionem valors unics amb 'distinct'
    #'pull' transofrma la columna de la taula en un vector convencional de R
    df %>% distinct(diet) %>% pull(diet)
  })
  
  # solucions: recalc segons overrides i transport
  # mantenim una taula reactiva d'overrides: tibble(ingredient, origen_selected)
  
  #character() , crea un vector de caracters vuit
  #Esta definint una taula amb dues columnes: ingredient i origen_selected
  overrides <- reactiveVal(tibble(ingredient = character(0), origen_selected = character(0)))
  
  #NAVBAR SELECCIO ORIGENS I INGREDIENTS

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
  
  #Outputs VISIO GENERAL
  
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
  
  #Plots Composicio per Dieta
  output$plot_comp_A <- renderPlotly({ plot_composicio(solA_joined(), ordre_dietes = ordre_dietes()) })
  output$plot_comp_B <- renderPlotly({ plot_composicio(solB_joined(), ordre_dietes = ordre_dietes()) })
  
  output$plot_impacte_A_vs_B <- renderPlotly({
    if(input$mostrar_per_animal) {
      plot_impacte_A_vs_B_generic(resumA_animal(), resumB_animal(), impactes_sel = input$impactes_sel, per_animal = TRUE)
    } else {
      plot_impacte_A_vs_B_generic(resumA_kg(), resumB_kg(), impactes_sel = input$impactes_sel, per_animal = FALSE)
    }
  })
  
  #COMPOSICIÓ PER DIETA
  
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
  
  # MAPA
  
  output$map_origens <- renderLeaflet({
    req(solA_joined(), solB_joined())
    plot_map_origens(solA_joined(), solB_joined(), transport_df = transport_df())
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
}
