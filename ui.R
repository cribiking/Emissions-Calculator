ui <- dashboardPage(
  # 1. HEADER AMB INPUTS (Maximitzem espai lateral)
  header = dashboardHeader(
    title = "", titleWidth = 0,
    
    tags$li(class = "dropdown",
            style = "width: 100vw; height: 220px; min-height:220px; display: flex; align-items: center; justify-content: space-around; padding: 0 20px;",
            
            # BLOC 1: Fitxers (Columna)
            div(style = "display: flex; flex-direction: column; gap: 5px; justify-content: center; padding: 20px;",
                fileInput("file_env", NULL, buttonLabel = "Ambiental", accept = ".xlsx", width = "200px"),
                fileInput("file_diets", NULL, buttonLabel = "Dietes", accept = ".xlsx", width = "200px"),
                fileInput("file_transport", NULL, buttonLabel = "Transport", accept = ".xlsx", width = "200px")
            ),
            
            # BLOC NOU: Selecció de Pas (Step) - Integrat al Header
            div(style = "display: flex; flex-direction: column; align-items: center; gap: 10px; min-width: 180px; background: rgba(255,255,255,0.05); padding: 15px; border-radius: 8px;",
                h4("Etapa / Pas", style = "margin: 0; color: white; font-weight: bold; font-size: 14px;"),
                uiOutput("steps_ui") 
            ),
            
            
            # BLOC 2: Impactes (Columna Centrada)
            div(style = "display: flex; flex-direction: column; justify-content: center; background: rgba(255,255,255,0.05); padding: 10px; border-radius: 8px;",
                checkboxGroupInput("impactes_sel", NULL, 
                                   choices = c("Climate Change" = "climate_change", "Land Use" = "land_use", 
                                               "Water Use" = "water_use", "Eutrophication" = "eutrophication", 
                                               "Acidification" = "acidification", "Particulate Matter" = "particulate_matter"),
                                   selected = c("climate_change", "land_use"),
                                   inline = FALSE)
            ),
            
            # BLOC 3: Opcions i Botó (Centrat verticalment)
            div(style = "display: flex; flex-direction: column; align-items: center; gap: 15px; justify-content: center;",
                div(style = "margin-top: -10px;", # Correcció d'alineació per al checkbox
                    checkboxInput("mostrar_per_animal", "Per animal", value = FALSE)
                ),
                downloadButton("download_summary", "Excel", class = "btn-success")
            )
    )
  ),
  # 2. SIDEBAR DESACTIVAT (Per guanyar el 100% de l'ample)
  sidebar = dashboardSidebar(disable = TRUE),
  
  # 3. BODY
  body = dashboardBody(
    tags$head(
      includeCSS("www/style.css"),
      tags$style(HTML("
        .main-header .navbar-custom-menu { float: left !important; }
        .main-header .logo { width: 200px !important; }
        .navbar-static-top { margin-left: 200px !important; }
        .shiny-input-container { margin-bottom: 0 !important; }
      "))
    ),
    
    div(style = "padding : 5px;",
        
        # --- BLOC DE CONFIGURACIÓ AVANÇADA (Col·lapsable) ---
        fluidRow(
          box(title = "Configuració Avançada, Steps i Overrides", width = 12, 
              collapsible = TRUE, collapsed = TRUE, status = "warning", icon = icon("cog"),
              fluidRow(
                column(3, 
                       h4("Reassignar orígens"),
                       selectInput("sel_ingredient", "Ingredient", choices = NULL),
                       uiOutput("sel_origen_ui"),
                       actionButton("apply_override", "Aplica", class = "btn-primary btn-block"),
                       br(),
                       actionButton("reset_overrides", "Reset All", class = "btn-danger btn-sm")
                ),
                column(6,
                       conditionalPanel(
                         condition = "input.mostrar_per_animal == true",
                         h4("Edita els kg consumits:"),
                         DTOutput("tbl_kg_edit")
                       )
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
                   )
          ),
          
          # --- Pestanya Composició per dieta ---
          tabPanel("Composició per dieta",
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
                   div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; border-left: 5px solid #27ae60;",
                       h4("Perfil Ambiental Comparatiu", style = "font-weight: bold; margin-left: 10px;"),
                       p("Visualització agregada dels indicadors seleccionats.", style = "margin-left: 10px; color: #7f8c8d;")
                   ),
                   br(),
                   uiOutput("plot_impacte_AB", height = "900px")
          ),
          
          # --- Pestanya Contribució per origen (AMB COLUMNES AL COSTAT) ---
          tabPanel("Contribució per origen",
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
                   # Selector d'impacte centrat
                   fluidRow(
                     column(12, align = "center",
                            selectInput("impacte_top", "Selecciona l'impacte per al Top 5:",
                                        choices = c("climate_change", "land_use", "water_use", 
                                                    "eutrophication", "acidification", "particulate_matter"),
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
          
          # --- Pestanya Diferència A - B ---
          tabPanel("Diferència A - B",
                   br(),
                   div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; border-left: 5px solid #e74c3c;",
                       h4("Comparativa Directa: Diferència d'Impacte (A - B)", style = "font-weight: bold; margin-left: 10px;")),
                   p("Valors positius = A pitjor. Valors negatius = B pitjor.", style = "margin-left: 15px; color: #7f8c8d; font-style: italic;"),
                   uiOutput("plot_diff")
          ),
          
          # --- Pestanya Verificació ---
          tabPanel("Verificació d'Ingredients",
                   br(),
                   div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; border-left: 5px solid #f1c40f;",
                       h4("Integritat de la Base de Dades", style = "font-weight: bold; margin-left: 10px;")),
                   br(),
                   uiOutput("aviso_faltantes_ui")
          )
        ) # Tanca tabBox
    ) # Tanca div padding
  ) # Tanca body
)