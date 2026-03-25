

ui <- dashboardPage(
  # 1. HEADER AMB INPUTS (Maximitzem espai lateral)
  header = dashboardHeader(title = "Estudi d'Emissions", titleWidth = 300),
  
  # 2. SIDEBAR DESACTIVAT (Per guanyar el 100% de l'ample)
  sidebar = dashboardSidebar(disable = TRUE),
  
  # 3. BODY
  body = dashboardBody(
    tags$head(
      includeCSS("www/style.css"),
      tags$style(HTML("
        /* Ajustos per a que el contingut ocupi tot l'ample i es vegi net */
        .content-wrapper { background-color: #ecf0f5; }
        .box-header { background: #f4f4f4; border-bottom: 1px solid #ddd; }
        .shiny-input-container { margin-bottom: 10px !important; }
      "))
    ),
    
    div(style = "padding : 1px;",
        
        # --- NOU BLOC DE CONTROL SUPERIOR (SUBSTITUEIX EL HEADER) ---
        fluidRow(
          box(title = span(icon("sliders-h"), " Configuració General i Càrrega de Dades"), 
              width = 12, status = "primary", solidHeader = TRUE,
              collapsible = TRUE, collapsed = TRUE,
              
              fluidRow(
                # BLOC 1: Fitxers
                column(4,
                       div(style = "padding: 5px; border-right: 1px solid #eee; display:flex ; flex-direction:column; align-items:center;",
                         h3(strong("1. Importar Arxius")),
                         fileInput("file_env", NULL, buttonLabel = "Ambiental", accept = ".xlsx", width = "100%"),
                         fileInput("file_diets", NULL, buttonLabel = "Dietes", accept = ".xlsx", width = "100%"),
                         fileInput("file_transport", NULL, buttonLabel = "Transport", accept = ".xlsx", width = "100%")
                       ),  
                ),
                
                # BLOC 2: Etapa / Pas
              
                  column(4,
                         div(style = "display:flex ; flex-direction:column; align-items:center ; width: 100%;",
                             h3(strong("2. Selecció d'Etapa")),
                             uiOutput("steps_ui")
                        )
                ),
                
                # BLOC 3: Impactes
                column(4,
                       div(style = "padding: 10px;border-left: 1px solid #eee;  border-right: 1px solid #eee;display:flex; flex-direction:column; align-items:center;",
                         h3(strong("3. Impactes Ambientals")),
                         checkboxGroupInput("impactes_sel", NULL, 
                                            choices = c("Climate Change" = "climate_change", "Land Use" = "land_use", 
                                                        "Water Use" = "water_use", "Eutrophication" = "eutrophication_marine", 
                                                        "Acidification" = "acidification", "Particulate Matter" = "particulate_matter"),
                                            selected = c("climate_change", "land_use"),
                                            inline = FALSE)
                       ),
                )
                
              )
          )
        ),
        
        # --- BLOC DE CONFIGURACIÓ AVANÇADA (Col·lapsable) ---
        fluidRow(
          box(title = "Reasignar Origens i Descàrrega de dades (Configuració Avançada)", width = 12, 
              collapsible = TRUE, collapsed = TRUE, status = "warning",
              fluidRow(
                column(6, 
                       h4("Reassignar orígens"),
                       div(style = "background-color: #f8f9fa; border-left: 4px solid #3498db; padding: 10px; margin-bottom: 15px; border-radius: 4px;",
                           span(icon("info-circle"), style = "color: #3498db; margin-right: 5px;"),
                           tags$small(style = "color: #5a5a5a;", 
                                      "S'exclouen els ingredients que només tenen un origen disponible.")
                       ),
                       uiOutput("sel_ingredient_ui"), 
                       uiOutput("sel_origen_ui"),
                       hr(),
                       actionButton("apply_override", "Aplica canvi", class = "btn-primary btn-block"),
                       br(),
                       div(align = "center",
                           actionButton("reset_overrides", "Restablir tot", class = "btn-danger btn-sm")
                       )
                ),
                column(6, align = "center",
                       h3("Descarregar Excel"),
                       downloadButton("download_summary", "Exportar a Excel", class = "btn-success btn-block")
                
              )
          )     
          )
        ),
        
        # --- TABSET PRINCIPAL ---
        tabBox(
          width = 12,
          id = "tabset1",
          
          # --- Pestanya Visió general ---
          tabPanel("Visió general",
                   br(),
                   div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; border-left: 5px solid #2c3e50;",
                       h4("Resum Executiu de les Dades", style = "font-weight: bold; margin-left: 10px;")),
                   br(),
                   helpText(icon("info-circle"), "Configuració actual: Les dades d'impacte base es mostren per kg de producte."),
                   br(),
                   fluidRow(
                     valueBoxOutput("box_ingredients", width = 3),
                     valueBoxOutput("box_origins", width = 3),
                     valueBoxOutput("box_diets", width = 3),
                     valueBoxOutput("box_steps", width = 3)
                   ),
                   br(), hr(), br(),
                   fluidRow(
                     column(6, 
                            div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; border-bottom: 3px solid #34495e; margin-bottom:15px;",
                                h5("Llistat Dietes: Solució A", style = "text-align: center; font-weight: bold;")),
                            DTOutput("tbl_dietes_A")
                     ),
                     column(6, 
                            div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; border-bottom: 3px solid #34495e; margin-bottom:15px;",
                                h5("Llistat Dietes: Solució B", style = "text-align: center; font-weight: bold;")),
                            DTOutput("tbl_dietes_B")
                     )
                   ),
                   fluidRow(
                     column(12,
                       div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; border-bottom: 3px solid #34495e; margin-bottom:15px;",
                         br(),
                         div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; border-left: 5px solid #f1c40f;",
                             h4("Integritat de la Base de Dades", style = "font-weight: bold; margin-left: 10px;")),
                         br(),
                         uiOutput("aviso_faltantes_ui")
                       )
                     )
                   )
          ),
          
          # --- Pestanya Composició per dieta ---
          tabPanel("Composició per dieta",
                   br(),
                 downloadButton("download_comp", "Descarregar gràfics (PNG)",
                        style = "background-color: #2c3e50; color: white; border: none; padding: 10px 20px; font-weight: bold;"),
                 actionButton("ai_explain_comp", "Explicar amb IA", icon = icon("robot"),
                              style = "background-color: #3498db; color: white; border: none; padding: 10px 20px; font-weight: bold; margin-left: 10px;"),
                 br(),
                 htmlOutput("ai_response_comp"),
                 br(),
                   div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; border-left: 5px solid #9b59b6;",
                       h4("Anàlisi de Formulació i Proporcions", style = "font-weight: bold; margin-left: 10px;"),
                       helpText(icon("lightbulb"), "Truc: Fes doble clic en la llegenda per aïllar un ingredient.")
                   ),
                   br(),
                   fluidRow(
                     column(12,
                            div(style = "padding: 10px; border: 1px solid #eee; border-radius: 10px;",
                                h5("Estructura Solució A", style = "font-weight: bold; color: #2c3e50;"),
                                plotlyOutput("plot_comp_A", height = "600px"))
                     )
                   ),
                   br(), hr(), br(),
                   fluidRow(
                     column(12,
                            div(style = "padding: 10px; border: 1px solid #eee; border-radius: 10px;",
                                h5("Estructura Solució B", style = "font-weight: bold; color: #2c3e50;"),
                                plotlyOutput("plot_comp_B", height = "600px"))
                     )
                   )
          ),
          
          # --- Pestanya Impactes per dieta ---
          tabPanel("Impactes per dieta",
                   br(),
                   # Botó de descàrrega amb un estil cridaner
                   downloadButton("download_plots_AB", "Descarregar Comparativa (PNG)",
                                  style = "background-color: #2c3e50; color: white; border: none; padding: 10px 20px; font-weight: bold;"),
                   actionButton("ai_explain_impactes", "Explicar amb IA", icon = icon("robot"),
                                style = "background-color: #3498db; color: white; border: none; padding: 10px 20px; font-weight: bold; margin-left: 10px;"),
                   br(),
                   htmlOutput("ai_response_impactes"),
                   br(),
                   div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; border-left: 5px solid #27ae60;",
                       h4("Perfil Ambiental Comparatiu", style = "font-weight: bold; margin-left: 10px;"),
                       p("Visualització agregada dels indicadors seleccionats.", style = "margin-left: 10px; color: #7f8c8d;")
                   ),
                   br(),
                   uiOutput("plot_impacte_AB")
          ),
          
          # --- Pestanya Contribució per origen (AMB COLUMNES AL COSTAT) ---
          tabPanel("Contribució per origen",
                   br(),
                   downloadButton("download_origen", "Descarregar gràfics (PNG)",
                                  style = "background-color: #2c3e50; color: white; border: none; padding: 10px 20px; font-weight: bold;"),
                   actionButton("ai_explain_origen", "Explicar amb IA", icon = icon("robot"),
                                style = "background-color: #3498db; color: white; border: none; padding: 10px 20px; font-weight: bold; margin-left: 10px;"),
                   br(),
                   htmlOutput("ai_response_origen"),
                   br(),
                   fluidRow(
                     column(6, 
                            style = "border-right: 1px solid #ddd; padding-right: 25px;",
                            div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; border-left: 5px solid #2c3e50; margin-bottom: 20px;",
                                h4("Anàlisi Geogràfic: Solució A", style = "font-weight: bold; margin-left: 10px;")),
                            uiOutput("plot_origen_A_dinamic")
                     ),
                     column(6, 
                            style = "padding-left: 25px;",
                            div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; border-left: 5px solid #2c3e50; margin-bottom: 20px;",
                                h4("Anàlisi Geogràfic: Solució B", style = "font-weight: bold; margin-left: 10px;")),
                            uiOutput("plot_origen_B_dinamic")
                     )
                   )
          ),
          
          # --- Pestanya Top ingredients ---
          tabPanel("Top ingredients",
                   br(),
                   downloadButton("download_top", "Descarregar gràfics (PNG)",
                                  style = "background-color: #2c3e50; color: white; border: none; padding: 10px 20px; font-weight: bold;"),
                   actionButton("ai_explain_top", "Explicar amb IA", icon = icon("robot"),
                                style = "background-color: #3498db; color: white; border: none; padding: 10px 20px; font-weight: bold; margin-left: 10px;"),
                   br(),
                   htmlOutput("ai_response_top"),
                   br(),
                   # Selector d'impacte centrat
                   fluidRow(
                     column(12, align = "center",
                            selectInput("impacte_top", "Selecciona l'impacte per al Top 5:",
                                        choices = c("climate_change", "land_use", "water_use", 
                                                    "eutrophication_marine", "acidification", "particulate_matter"),
                                        width = "50%"),
                            
                            # Nota informativa sobre els percentatges petits
                            div(style = "width: 50%; background-color: #fff9e6; border-left: 5px solid #ffcc00; padding: 10px; margin-top: 10px; text-align: left; border-radius: 4px;",
                                span(icon("exclamation-triangle"), style = "color: #e67e22; margin-right: 8px;"),
                                tags$small(style = "color: #7f8c8d;",
                                           "Nota: Si algun ingredient no mostra una barra visible, és perquè el seu impacte és proporcionalment molt baix. ",
                                           strong("Podeu fer zoom al gràfic"))
                            )
                     )
                   ),
                   br(),
                   
                   fluidRow(
                     # Columna Solució A
                     column(6, 
                            style = "border-right: 1px solid #ddd; padding-right: 25px;",
                            div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; border-left: 5px solid #2c3e50; margin-bottom: 15px;",
                                h4("Top 5 Ingredients - Solució A", style = "font-weight: bold; margin-left: 10px;")),
                            uiOutput("plots_topA")
                     ),
                     
                     # Columna Solució B
                     column(6, 
                            style = "padding-left: 25px;",
                            div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; border-left: 5px solid #2c3e50; margin-bottom: 15px;",
                                h4("Top 5 Ingredients - Solució B", style = "font-weight: bold; margin-left: 10px;")),
                            uiOutput("plots_topB")
                     )
                   )
          ),
          
          # --- Pestanya Mapa d'orígens ---
          tabPanel("Mapa d'orígens",
                   br(),
                   downloadButton("download_mapes", "Descarregar mapes (PNG)",
                                  style = "background-color: #2c3e50; color: white; border: none; padding: 10px 20px; font-weight: bold;"),
                   actionButton("ai_explain_mapes", "Explicar amb IA", icon = icon("robot"),
                                style = "background-color: #3498db; color: white; border: none; padding: 10px 20px; font-weight: bold; margin-left: 10px;"),
                   br(),
                   htmlOutput("ai_response_mapes"),
                   br(),
                   # Nota informativa millorada en català
                   div(style = "background-color: #e8f4fd; padding: 15px; border-radius: 8px; border-left: 6px solid #3498db; margin-bottom: 25px; box-shadow: 0 2px 4px rgba(0,0,0,0.05);",
                       span(icon("info-circle"), style = "color: #3498db; margin-right: 10px; font-size: 18px;"),
                       strong("Notes sobre la representació:"), 
                       tags$ul(style = "margin-top: 10px;",
                               tags$li(style = "margin-bottom: 5px;", 
                                       "Els ingredients amb origen genèric europeu (", strong("RER"), 
                                       ") s'han assignat a ", strong("Espanya"), " per a la seva visualització."),
                               tags$li("A causa de la codificació internacional (ISO), les dades de ", strong("França"), 
                                       " es repliquen automàticament en els seus territoris d'ultramar i illes (com la Guaiana Francesa o Reunió).")
                       )
                   ),
                   
                   fluidRow(
                     column(6, 
                            div(style = "background-color: #f8f9fa; padding: 12px; border-radius: 8px 8px 0 0; border-bottom: 2px solid #2c3e50;",
                                h4(icon("map-marker-alt"), " Origen ingredients – Solució A", style = "font-weight: bold; margin:0;")),
                            highchartOutput("map_solA", height = "550px")
                     ),
                     column(6, 
                            div(style = "background-color: #f8f9fa; padding: 12px; border-radius: 8px 8px 0 0; border-bottom: 2px solid #2c3e50;",
                                h4(icon("map-marker-alt"), " Origen ingredients – Solució B", style = "font-weight: bold; margin:0;")),
                            highchartOutput("map_solB", height = "550px")
                     )
                   )
          ),
          
          tabPanel("Distribucio",
                   br(),
                 downloadButton("download_distribucio", "Descarregar gràfics (PNG)",
                        style = "background-color: #2c3e50; color: white; border: none; padding: 10px 20px; font-weight: bold;"),
                 actionButton("ai_explain_distribucio", "Explicar amb IA", icon = icon("robot"),
                              style = "background-color: #3498db; color: white; border: none; padding: 10px 20px; font-weight: bold; margin-left: 10px;"),
                 br(),
                 htmlOutput("ai_response_distribucio"),
                 br(),
                   div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; border-left: 5px solid #e74c3c;",
                       h4("Distribució d'emissions", style = "font-weight: bold; margin-left: 10px;")),
                   uiOutput("plot_box_ui")
            ),
          
          # --- Pestanya Diferència A - B ---
          tabPanel("Diferència A - B",
                   br(),
                 downloadButton("download_diff", "Descarregar gràfics (PNG)",
                        style = "background-color: #2c3e50; color: white; border: none; padding: 10px 20px; font-weight: bold;"),
                 actionButton("ai_explain_diff", "Explicar amb IA", icon = icon("robot"),
                              style = "background-color: #3498db; color: white; border: none; padding: 10px 20px; font-weight: bold; margin-left: 10px;"),
                 br(),
                 htmlOutput("ai_response_diff"),
                 br(),
                   div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; border-left: 5px solid #e74c3c;",
                       h4("Comparativa Directa: Diferència d'Impacte (A - B)", style = "font-weight: bold; margin-left: 10px;")),
                   p("Valors positius = A pitjor. Valors negatius = B pitjor.", style = "margin-left: 15px; color: #7f8c8d; font-style: italic;"),
                   uiOutput("plot_diff")
          ),
          
        
          
          # --- Pestanya Percentatge Contribucio Emissio---
          # --- Pestanya Percentatge Contribucio Emissio---
          tabPanel("Desglossament Impacte",
                   br(),
                 downloadButton("download_desglossament", "Descarregar gràfics (PNG)",
                        style = "background-color: #2c3e50; color: white; border: none; padding: 10px 20px; font-weight: bold;"),
                 actionButton("ai_explain_desglossament", "Explicar amb IA", icon = icon("robot"),
                              style = "background-color: #3498db; color: white; border: none; padding: 10px 20px; font-weight: bold; margin-left: 10px;"),
                 br(),
                 htmlOutput("ai_response_desglossament"),
                 br(),
                   # --- BLOC INFORMATIU ---
                   div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; border-left: 5px solid #e67e22; margin-bottom: 20px; box-shadow: 0 2px 4px rgba(0,0,0,0.05);",
                       h4("Anàlisi de la Provinença de les Emissions", style = "font-weight: bold; color: #2c3e50;"),
                       p("Aquesta visualització permet identificar quina part de l'impacte ambiental total correspon a la", 
                         strong("producció dels ingredients"), "(blau fosc) i quina part és conseqüència del", 
                         strong("transport"), "(taronja) des del seu origen fins al lloc de consum (ES).")
                   ),
                   
                   # --- Secció de visualització de gràfics comparatius ---
                   fluidRow(
                     column(12,
                            # Espai on es generaran dinàmicament els gràfics (un per cada impacte seleccionat)
                            uiOutput("plots_totals_emissio")
                     )
                   )
          ),
          
          # --- Contribució Total---

          tabPanel("Contribució Total",
                   br(),
                 downloadButton("download_contrib_total", "Descarregar gràfics (PNG)",
                        style = "background-color: #2c3e50; color: white; border: none; padding: 10px 20px; font-weight: bold;"),
                 actionButton("ai_explain_contrib_total", "Explicar amb IA", icon = icon("robot"),
                              style = "background-color: #3498db; color: white; border: none; padding: 10px 20px; font-weight: bold; margin-left: 10px;"),
                 br(),
                 htmlOutput("ai_response_contrib_total"),
                 br(),
                   # Banner de títol amb estil coherent
                   div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; border-left: 5px solid #3498db;",
                       h4("Anàlisi de Contribució Acumulada: Solució A vs Solució B", 
                          style = "font-weight: bold; margin-left: 10px;")),
                   
                   p("Edita els kg consumits per ajustar l'impacte total segons l'etapa productiva.", 
                     style = "margin-left: 15px; color: #7f8c8d; font-style: italic;"),
                   
                   br(),
                   
                   fluidRow(
                     # Taula d'edició (centrada en una columna de 8)
                     column(width = 8, offset = 2,
                            div(style = "background-color: white; padding: 15px; border-radius: 10px; box-shadow: 0px 2px 4px rgba(0,0,0,0.05);
                                display: flex; flex-direction:column; align-items:center",
                                h5("Consum per Etapa (kg):", style = "font-weight: bold;"),
                                DTOutput("tbl_kg_edit")
                            )
                     )
                   ),
                   
                   br(),
                   hr(), # Línia separadora
                   
                   fluidRow(
                     # Gràfics de contribució
                     column(width = 12,
                            uiOutput("plot_contribucio_total_AB")
                     )
                   )
                   
                  
          ),
          
          tabPanel(
            "Petjada Ambiental",
            br(),
            
            # ===== SECCIÓ 0: COMPARATIVA A vs B (DESTACADA) =====
            div(style = "background-color: #e8f5e9; padding: 15px; border-radius: 5px; border-left: 5px solid #4caf50; margin-bottom: 20px;",
                h3(icon("balance-scale"), " Comparació Directa: Solució A vs Solució B", 
                   style = "font-weight: bold; margin: 0;"),
                p(style = "margin: 5px 0 0 0; color: #6c757d;",
                  "Petjada ambiental total ponderada de les dues solucions seleccionades")
            ),
            
            
            # Info diferència
            uiOutput("info_diferencia_AB"),
            
            br(),
            
            # Gràfic comparatiu A vs B
            div(style = "background: white; padding: 20px; border-radius: 10px; box-shadow: 0 2px 10px rgba(0,0,0,0.1);",
                plotlyOutput("plot_comparativa_AB", height = "300px")
            ),
            
            br(),
            
            # Taula comparativa ràpida
            div(style = "background: white; padding: 15px; border-radius: 10px; box-shadow: 0 2px 10px rgba(0,0,0,0.1);",
                h5(icon("table"), " Desglossament per Fase", style = "margin-bottom: 15px;"),
                DTOutput("tbl_comparativa_AB")
            ),
            
            hr(style = "margin: 30px 0; border-top: 3px solid #eee;"),
            
            # ===== SECCIÓ 1: Visió general de TOTES =====
            div(style = "background-color: #e8f5e9; padding: 15px; border-radius: 5px; border-left: 5px solid #4caf50; margin-bottom: 20px;",
                h4(icon("globe"), " Petjada Ambiental - Totes les Dietes", style = "font-weight: bold; margin: 0;"),
                p(style = "margin: 5px 0 0 0; color: #6c757d;", 
                  "Càlcul complet per a tots els Steps i Fases de creixement")
            ),
            
            uiOutput("info_totes_dietes"),
            
            br(),
            
            # ===== SECCIÓ 2: Gràfic comparatiu de TOTS =====
            h4(icon("chart-bar"), " Comparativa de Tots els Steps"),
            plotlyOutput("plot_comparativa_steps", height = "500px"),
            
            hr(),
            
            # ===== SECCIÓ 3: Taula resum per Step =====
            h4(icon("table"), " Resum per Step (Total)"),
            p("Suma de les 4 fases: ENTRADA + CREIXEMENT + ENGREIX + ACABAT"),
            DTOutput("tbl_resum_steps"),
            
            hr(),
            
            # ===== SECCIÓ 4: Taula Step × Fase =====
            h4(icon("th"), " Desglossament per Step i Fase"),
            DTOutput("tbl_resum_dietes"),
            
            hr(),
            
            # ===== SECCIÓ 5: Detall per impacte (col·lapsable) =====
            tags$details(
              tags$summary(style = "cursor: pointer; font-weight: bold; color: #666; padding: 10px;",
                           icon("list"), " Veure detall per categoria d'impacte"
              ),
              br(),
              DTOutput("tbl_detall_impactes")
            ),
            
            hr(),
            
            # ===== SECCIÓ 6: Verificació de dades =====
            tags$details(
              tags$summary(style = "cursor: pointer; font-weight: bold; color: #666; padding: 10px;",
                           icon("clipboard-check"), " Verificació d'integritat de dades"
              ),
              br(),
              uiOutput("verify_footprint_panel")
            )
          )
        ) # Tanca tabBox
    ) # Tanca div padding
  ) # Tanca body
)