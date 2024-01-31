  library(leaflet)
  library(shiny)
  library(shinydashboard)
  
  # Choices for drop-downs
  vvars <- c(
    "Tamaño cuenca original" = "km2Nosn",
    "Tamaño cuenca ajustado" = "km2Hort",
    "Validado" = "validated",
    "Revisado" = "revised",
    "Diferencia en Km2 entre cuencas" = "differencekm2",
    "Diferencia en % entre cuencas" = "differenceperc",
    "Fuente" = "sou"
    #"Caudal promedio anual" = 'annualav',
    #"Caudal minimo anual" = 'annualmn',
    #"Caudal maximo anual" = 'annualmx'
  )
  
  
  navbarPage(
    "Cuencas", id="nav",
    
    tabPanel("Mapa",
             div(
               class="outer",
               
               tags$head(
                 # Include our custom CSS
                 includeCSS("styles.css"),
                 includeScript("gomap.js")
               ),
               
               # If not using custom CSS, set height of leafletOutput to a number instead of percent
               leafletOutput("map", width="100%", height="100%"),
               
               # Shiny versions prior to 0.11 should use class = "modal" instead.
               absolutePanel(
                 id = "controls", class = "panel panel-default", fixed = TRUE,
                 draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                 width = 330, height = "auto",
                 
                 shiny::fixedRow(
                   column(6, h3("Visualización")), 
                   column(4, align="left", br(), actionButton("inXreset", HTML("Reset view")))
                 ),
                 
                 h4(''),
                 
                 selectInput(inputId = "inxcolor", 
                             label = "Color de puntos", 
                             choices = vvars),
                 #selectInput("size", "Size", vars, selected = "adultpop"),
                 conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
                                  # Only prompt for threshold when coloring or sizing by superzip
                                  numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
                 ),
                 # shinydashboard::box( width = 10, solidHeader = FALSE, collapsible = TRUE, 
                 #                      title = "Advanced parameters", status = "primary", 
                 #                      collapsed = F, 
                 #                      column(12, 
                 #                             sliderInput("orders", "Orders", min = 1, max = 2000, value = 650)
                 #                      )
                 # ),
                 shiny::fixedRow(
                   column(6, align="center", actionButton("inXprev", HTML("<< Prev"))), 
                   column(6, align="center", actionButton("inXnext", HTML("Next >>")))
                 ),
                 
                 h3("Validación"),
                 
                 textInput("inxuser", "Usuario:", ''),
                 textInput("inxqlid", "ID de estación:", ''),
                 #actionButton("govalidate", HTML("Validar cuenca")),
                 h4(''),
                 
                 textInput("inxcomment", "Comentario:", ''),
                 h5('Nuevas coordenadas:'),

                 verbatimTextOutput("inxxcord"),
                 verbatimTextOutput("inxycord"),
                 shiny::fixedRow(
                 #actionButton("gocoord", HTML("Enviar coordenada")),
                   column(3, 
                          align="center", 
                          checkboxInput('isvalid', 'Correcta?', value = FALSE, width = NULL)),
                   column(9, align="center", 
                          actionButton("gocoment", HTML("Enviar comentario"))
                          ) 
                 ),
                 
                 verbatimTextOutput("outxlog", placeholder = T)
                 
                 # plotOutput("histCentile", height = 200),
                 # plotOutput("scatterCollegeIncome", height = 250)
               ),
               
               tags$div(id="cite",
                        'Data compiled for ', tags$em('Coming Apart: The State of White America, 1960–2010'), ' by Charles Murray (Crown Forum, 2012).'
               )
             )
    ),
    
    tabPanel("Datos",
             fluidRow(
               column(3,
                      selectInput("states", "States", c("All states"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE)
               ),
               column(3,
                      conditionalPanel("input.states",
                                       selectInput("cities", "Cities", c("All cities"=""), multiple=TRUE)
                      )
               ),
               column(3,
                      conditionalPanel("input.states",
                                       selectInput("zipcodes", "Zipcodes", c("All zipcodes"=""), multiple=TRUE)
                      )
               )
             ),
             fluidRow(
               column(1,
                      numericInput("minScore", "Min score", min=0, max=100, value=0)
               ),
               column(1,
                      numericInput("maxScore", "Max score", min=0, max=100, value=100)
               )
             ),
             hr(),
             DT::dataTableOutput("ziptable")
    ),
    
    conditionalPanel("false", icon("crosshair"))
  )