#Aplicacao

# Carregando os arquivos com as funcoes

library(data.table)
library(e1071)
library(shinyalert)
library(plotly)
library(RMySQL)
library(shinyjs)
library(DT)
library(GA)
library(forecast)


source('auxiliar.R')

source('avaliacao/coeficienteHurst.R')
source('avaliacao/correlograma.R')
source('avaliacao/graficoFAC_Anual.R')
source('avaliacao/graficoFAC_Mensal.R')
source('avaliacao/graficoSerieHistorica.R')
source('avaliacao/graficoSeries.R')
source('avaliacao/modules.R')
source('avaliacao/volumeUtil.R')

source('ui/tab-arma.R')
source('ui/tab-pmix.R')
source('ui/tab-desagregacao.R')
source('ui/tab-estacoes.R')
source('ui/tab-series-geradas.R')

source('mysql/mysql-arma.R')
source('mysql/mysql-pmix.R')
source('mysql/mysql-desagregacao.R')
source('mysql/mysql-functions.R')


source('algoritmos/arma/algoritmo-arma.R')
source('algoritmos/desagregacao/algoritmo-desagregacao.R')
source('algoritmos/pmix/algoritmo-pmix.R')
source('algoritmos/pmix/cenarioSintetico.R')
source('algoritmos/pmix/powell.R')
source('algoritmos/pmix/entrada.R')
source('algoritmos/pmix/sumQuadRes.R')
source('algoritmos/pmix/otimizacao/avaliacao.R')
source('algoritmos/pmix/otimizacao/inicializaPop.R')
source('algoritmos/pmix/otimizacao/mecanismos.R')
source('algoritmos/pmix/otimizacao/tempo.R')

# Shiny Server
# Fonte: https://shiny.rstudio.com/articles/scoping.html

## O Server foi dividido em 5 arquivos para facilitar a leitura do codigo.Esse arquivos se encontram na pasta 'server'.
## Os arquivos: 'server-pmix.R','server-estacoes.R','server-serie-geradas.R','server-arma.R','server-desagregacao.R'
## Arquivo 'server-pmix.R': Possui o server relacionado a tabpanel 'PMIX'. UI realionada a esse server: 'tab-PMIX.R'
## Arquivo 'server-estacoes.R': Possui o server relacionado a tabpanel 'Estacoes'. UI realionada a esse server: 'tab-estacoes.R'
## Arquivo 'server-series-geradas.R': Possui o server relacionado a tabpanel 'Series Geradas'. UI realionada a esse server: 'tab-series-geradas.R'
## Arquivo 'server-arma.R': Possui o server relacionado a tabpanel 'ARMA'. UI realionada a esse server: 'tab-ARMA.R'
## Arquivo 'serve-desagregacao.R': Possui o server relacionado a tabpanel 'Desagregacao'. UI realionada a esse server: 'tab-desagregacao.R'


server <- function (input, output, session) {
  
  source('server/server-pmix.R',local = TRUE)
  
  source('server/server-estacoes.R',local = TRUE)
  
  source('server/server-series-geradas.R',local = TRUE)
  
  source('server/server-arma.R',local = TRUE)
  
  source('server/server-desagregacao.R',local = TRUE)
  
}
ui <- navbarPage (title = div(img(src="logo.jpg", height='40px',width='40px', style="margin-top: -10px"), "MAEVAZ"),
  TabEstacoes,      
  TabPMIX,
  TabARMA,
  TabDesagregacao,
  TabSerieGeradas
            
)

shinyApp (ui = ui, server = server)