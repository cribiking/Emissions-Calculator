ui <- dashboardPage(
  header = dashboardHeader(title = "Impactes Ambientals"),
  
  sidebar = dashboardSidebar(
    sidebarMenu(
      # 1. Inputs de Archivos
      fileInput("file_env", "Dades ambientals", accept = c(".xlsx")),
      fileInput("file_diets", "Dades dietes", accept = c(".xlsx")),
      fileInput("file_transport", "Fitxer transport", accept = c(".xlsx")),
      hr(),
      
      # 2. Selección de Steps y Filtros
      uiOutput("steps_ui"),
      hr(),
      checkboxGroupInput("impactes_sel", "Impactes a mostrar",
                         choices = c("climate_change", "land_use", "water_use",
                                     "eutrophication", "acidification", "particulate_matter"),
                         selected = c("climate_change", "land_use", "particulate_matter")),
      
      div(class = "mi-checkbox-mostrar-per-animal",
          checkboxInput("mostrar_per_animal", "Mostrar per animal", value = FALSE)
      ),
      
      # 3. Panel Condicional
      conditionalPanel(
        condition = "input.mostrar_per_animal == true",
        helpText("Edita els kg consumits:"),
        DTOutput("tbl_kg_edit")
      ),
      hr(),
      
      # 4. Reasignar Orígenes
      h4("Reassignar orígens"),
      selectInput("sel_ingredient", "Ingredient", choices = NULL),
      
      uiOutput("sel_origen_ui"),
      actionButton("apply_override", "Aplica override"),
      br(), br(),
      
      actionButton("reset_overrides", "Reset all"),
      hr(),
      downloadButton("download_summary", "Descarrega resum")
    )
  ), # <-- AQUÍ se cierra el dashboardSidebar
  
  body = dashboardBody(
    
    # El CSS se carga mejor aquí, al principio del cuerpo
    tags$head(
      includeCSS("www/style.css")
    ),
    
    div( style = "padding : 20px;",
      tabBox(
      width = 12,
      id = "tabset1",
      
      tabPanel("Visió general",
               h4("Resum de dades carregades"),
               
               # Detecció de ValueBoxes
               fluidRow(
                 valueBoxOutput("box_ingredients", width = 3),
                 valueBoxOutput("box_origins", width = 3),
                 valueBoxOutput("box_diets", width = 3),
                 valueBoxOutput("box_steps", width = 3)
               ),
               
               hr(),
               
               h4("Llistat de dietes"),
               fluidRow(
                 column(6, h5("Solució A"), DTOutput("tbl_dietes_A")),
                 column(6, h5("Solució B"), DTOutput("tbl_dietes_B"))
               )
      ),
      
      tabPanel("Composició per dieta",
               
               h4("Solució A"), 
               plotlyOutput("plot_comp_A", height = "600px"),
               
               hr(),
               
               h4("Solució B"),
               plotlyOutput("plot_comp_B", height = "600px")
      ),
      
      tabPanel("Impactes per dieta",
               uiOutput("plot_impacte_AB", height = "900px")
      ),
      
      tabPanel("Contribució per origen",
               
               h4("Solució A"), 
               plotlyOutput("plot_origen_A", height = "600px"),
               
               hr(),
               
               h4("Solució B"),
               plotlyOutput("plot_origen_B", height = "600px")
      ),
      
      tabPanel("Top ingredients",
               selectInput("impacte_top", "Tria impacte:",
                           choices = c("climate_change", "land_use", "water_use")),
               
               h4("Solució A – Top ingredients"),
               uiOutput("plots_topA", height = "600px"),
               
               hr(),
               
               h4("Solució B – Top ingredients"),
               uiOutput("plots_topB", height = "600px")
      ),
      
      tabPanel("Mapa d'orígens",
               highchartOutput("map_solA", height = "450px"),
               highchartOutput("map_solB", height = "450px")
      ),
      
      tabPanel("Distribució",
               plotlyOutput("plot_box", height = "700px")
      ),
      
      tabPanel("Diferència A - B",
               uiOutput("plot_diff")
               #plotlyOutput("plot_diff", height = "700px")
      )
    )
    )
  ) # <-- AQUÍ se cierra el dashboardBody
) # <-- AQUÍ se cierra el dashboardPage