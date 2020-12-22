# TAB DA UI RELACIONADA AO MODELO ARMA
#source('auxiliar.R')
#source('avaliacao/modules.R')

TabARMA = tabPanel("Modelo ARMA",
         sidebarLayout(
           sidebarPanel(
             titlePanel(h4("Modelo ARMA (p,q)",align="center")),
             br(),
             selectizeInput("estacoes_ARMA",label = "Escolha a Estação",choices=""),
             hr(),
             radioButtons ("type", label = "Estimação de parâmetros",
                           choices = list ("Metodo de Powell" = 1,
                                           "Algoritmo Genetico" = 2),
                           selected = 1,inline=TRUE),
             tags$hr ( ),
             titlePanel(h5(strong("Escolha os lags:"))),
             fluidRow(
               column(6,numericInput ("p_ARMA", label = "p", value = 1, min = 0, max = 12, width = "70px")),
               column(6,numericInput ("q_ARMA", label = "q", value = 0, min = 0, max = 12, width = "70px"))
             ),
             hr(),
             sliderInput ("nsint_ARMA", label = "Tamanho da série sintética", min = 0, max = 50000, value = 10000),
             hr(),
             shinyjs::hidden(
               div(id = "parametros_ag_arma",
                   h5(strong("Parametros do Algoritmo Genetico"),align="center"),
                   sliderInput ("nPopArma", label = "Tamanho da população", min = 10, max = 100, value = 50, width = "100%"),
                   sliderInput ("cicloMaxArma", label = "Ciclo Maximo", min = 0, max = 50000, value = 10000, width = "100%"),
                   fluidRow (
                     column (width = 6,numericInput ("pCA", label = "Probabilidade de cruzamento", value = 80, min = 0, max = 100)),
                     column(width = 6, numericInput ("pMA", label = "Probabilidade de mutacao", value = 5, min = 0, max = 100))
                   ),
                   fluidRow(
                     column (width = 6,numericInput ("MAPEdiferencaMAXArma", label = "MAPEdiferencaMAX", value = 5, min = 0, max = 100)),
                     column(width = 6,numericInput ("MAPEavaliacaoArma", label = "MAPEavaliacao", value = 20, min = 0, max = 100))
                   ),
                   fluidRow(
                     column (width = 6,numericInput ("lagAnualArma", label = "lag Anual", value = 1, min = 1, max = 12)),
                   ),
                   checkboxInput ("lagSignificativoArma", "Lag Significativo", TRUE)
               )),
             fluidRow( 
               column(6,actionButton("goButton_ARMA", "Iniciar", class = "btn-primary")),
               column(6,actionButton("limparButton_ARMA", "Limpar", class = "btn-primary",style="background-color:#ff0000;border-color: #ff0000"))
             ),
             hr(),
             titlePanel(h5(strong("Armazenar Serie:"))),
             actionButton("armazenarButton_ARMA", "Armazenar", class = "btn-primary"),
             shinyjs::hidden(
               span(id = "armazenando_msg_ARMA", "Armazenando..."),
               div(id = "error_armazenar_ARMA",
                   div(br(), tags$b("Error: "), span(id = "error_msg_armazenar_ARMA"))
               )
             )
             
           ),
           mainPanel(
             shinyjs::hidden(
               div(id="resultados_ARMA",
                   h3 (strong ("Resultados"),align = "center"),
                   shinyjs::hidden(
                     div(id="plotly_avaliacoes_arma",
                         hr(),
                         h4 (strong ("Grafico: MAPEfacAnual x MAPEdesvio x MAPEmédia"),align = "center"),
                         plotlyOutput(outputId = "grafico_avaliacoes_arma"))
                   ),
                   selectInput ("nSerieArma", "Serie a ser analisada:", choices = 1:50, selected = 50),
                   tabsetPanel ( id = "tabs",
                     tabPanel("Avaliação séries",
                                br(),
                                dataTableOutput ("tabelaAvaliacaoArma")
                     ),
                     tabPanel("Graficos FAC anuais",
                              br(),
                              # Module facAnual
                              facAnualOutput("ARMA")
                     ),
                     tabPanel("Avaliacao",
                              br ( ),
                              # Module avaliacaoAnual
                              avaliacaoAnualOutput("ARMA")
                     ),
                     tabPanel("Medidas",
                              br ( ),
                              volumeOutput("ARMA"),
                              hr ( ),
                              h4 (strong ("Coeficiente de Hurst")),
                              coeficienteHurstOutput("ARMA"),
                              hr(),
                              h4 (strong ("Soma Residual")),
                              verbatimTextOutput ("somaRes_ARMA")
                     )
                   ),
                   hr(),
                   downloadButton ("downloadSerie_ARMA", "Download Serie", icon ("save"))
               )
             )
           )
         )
)