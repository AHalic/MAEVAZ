#Aplicacao

# [1]  826.5939  794.4376  776.3641  904.2595  678.4912  935.2074  921.4238  957.5839  917.4750  669.8844
# [11]  907.0708  805.4649  748.6720  933.8707  762.0006  619.7249 1133.6330 1060.0012  669.8621  673.5460
# [21]  969.6225 1137.1218 1302.0300  707.6737  747.5660  747.7401 1103.1497  596.0219  734.9721  856.2790
# [31]  913.6773 1571.7230 1188.6872  818.4207  777.3808  738.8170  506.0929  496.3265  764.8382  738.3722
# [41] 1123.2559  662.9795  894.0591  695.7170  472.0482  536.4443  888.9775  995.1589 1143.8845 1005.7166
# [51] 1037.0884 1168.9584 1002.9729  811.8749 1476.5715  698.1906  933.4728  562.3622  646.2120  483.9016
# [61]  618.6469  932.3840  819.6662

# [1] 6.717313
# [[1]] 857.3338
# [[2]] 954.8898
# [[3]] 907.5199
# [[4]]795.5509
# [[5]] 1035.809
# [[6]]903.5211
# [[7]] 943.6472
# [[8]]1620.201
# [[9]]1208.145
# [[10]]1016.591
# [[11]]1072.674
# [[12]]1088.78
# [[13]]970.7866
# [[14]]844.8623
# [[15]]913.6128
# [[16]]899.8847
# [[17]]817.036
# [[18]]807.5687
# [[19]]843.9211
# [[20]]898.3034
# [[21]]913.0743
# [[22]]1099.48
# [[23]]822.3545
# [[24]]998.8981
# [[25]]955.8235
# [[26]]913.3109
# [[27]]911.7606
# [[28]]918.4644
# [[29]]1052.817
# [[30]]850.967
# [[31]]1023.434
# [[32]]1031.898
# [[33]]943.4193
# [[34]]875.5226
# [[35]]1015.934
# [[36]]946.5722
# [[37]]1061.923
# [[38]]909.5506
# [[39]]1048.599
# [[40]]920.028
# [[41]]983.3851
# [[42]]1020.891
# [[43]]813.2448
# [[44]]1022.376
# [[45]]918.5906
# [[46]]1023.988
# [[47]]809.5463
# [[48]]1477.347
# [[49]]912.6666
# [[50]]857.4859
# [1] 851.0518

# Carregando os arquivos com as funcoes

library(data.table)
library(e1071)
library(shinyalert)
library(plotly)
library(RMySQL)
library(shinyjs)
library(DT)
library(GA)

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

ui <- navbarPage (title = div(img(src="logo.jpg", height='40px',width='40px'), "MAEVAZ"),
  TabEstacoes,      
  TabPMIX,
  TabARMA,
  TabDesagregacao,
  TabSerieGeradas
            
)

shinyApp (ui = ui, server = server)