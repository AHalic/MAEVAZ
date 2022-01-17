## SERVER: TABPANEL MODELO ARMA
#source('mysql/mysql-functions.R')
#source('mysql/mysql-arma.R')
#source('algoritmos/arma/algoritmo-arma.R')


updateSelectInput(session, "estacoes_ARMA",
                  choices = estacao$nome,
                  selected = NULL)

# Input do modelo ARMA
serieHist_ARMA = reactive({
  serieHist_ARMA = valorSH('',input$estacoes_ARMA)
})

serieHistAnual_ARMA = reactive({
  apply (serieHist_ARMA(), 1, sum)  
})


observeEvent(input$type,{
  if(input$type == 2){
    shinyjs::show("parametros_ag_arma")
  }else{
    shinyjs::hide("parametros_ag_arma")
    shinyjs::enable("goButton_ARMA")
  }
})


# Funcao algoritmo do modelo ARMA
resultados_ARMA = reactive({
  
  progress <- shiny::Progress$new()
  on.exit(progress$close())
  progress$set(message = "Calculando o ARMA", value = 0)
  
  if (input$goButton_ARMA)
    isolate (cenarioSinteticoAnual(serieHist_ARMA(),c(input$p_ARMA,input$q_ARMA),input$nsint_ARMA, 
                                   input$type, c(input$nPopArma, input$cicloMaxArma, input$pCA, input$pMA,
                                                 input$MAPEdiferencaMAXArma, input$MAPEavaliacaoArma,
                                                 input$lagAnualArma, input$lagSignificativoArma)))
})

# Serie gerada pelo MODELO ARMA
serieSint_ARMA = reactive ({
  serieSint = resultados_ARMA()$serieSintetica
  if (input$type == 2) {
    serieSint = as.numeric(serieSint[[as.numeric(input$nSerieArma)]])
  }
  
  return (serieSint)
})


# Avaliacao da serie sintetica gerada pelo modelo ARMA
avaliacaoAnualARMA <- callModule(avaliacaoAnual,"ARMA",serieHistAnual_ARMA,serieSint_ARMA)
acfAnualARMA <- callModule(facAnual,"ARMA",serieHistAnual_ARMA,serieSint_ARMA)
hurstAnualARMA <- callModule(coeficienteHurst,"ARMA","Anual",serieHistAnual_ARMA,serieSint_ARMA)
volumeARMA <- callModule(volume,"ARMA",FALSE,serieHistAnual_ARMA,serieSint_ARMA)

output$resultadoGeralARMA = renderPrint ({
  # if (input$iniciar == 0)
  #   return ("Aguardando inicio...")
  if (input$type == 1) {
    print("Método dos Momentos")
  } 
  if (input$type == 2) {
    duracao = resultados_ARMA( )$duracao
    print("Algoritmo Genetico")
    
    print (paste ("Duracao:", duracao, "seg"))
    ciclos = resultados_ARMA( )$ciclos
    print (paste ("ciclos: ", ciclos))
  }
})

somaRes_ARMA = reactive({ 
  residuos = resultados_ARMA()$residuos
  
  if(input$type == 2){
    residuos = residuos[[as.numeric(input$nSerieArma)]]
  }
  
  somRes = sum(residuos^2)
})

observeEvent(input$goButton_ARMA,{
  hideTab(inputId = "tabs", target = "Avaliação séries")
  shinyjs::enable("limparButton_ARMA")
  shinyjs::disable("goButton_ARMA")
  shinyjs::disable("nPopArma")
  shinyjs::show("resultados_ARMA")
  shinyjs::disable("type")
  
  output$somaRes_ARMA = renderPrint ({
    print (somaRes_ARMA())
  })
  
  # In case of selecting GA
  if(input$type == 2){
    showTab(inputId = "tabs", target = "Avaliação séries")
    shinyjs::show("plotly_avaliacoes_arma")
    
    # Ploting the 3D graphic of ga
    output$grafico_avaliacoes_arma = renderPlotly({
      dadosArma = data.frame (resultados_ARMA( )$avaliacao[,-4])
      dadosArma$X = 1:nrow(dadosArma)
      ddArma = replicate(2, dadosArma, simplify = F)

      plot_ly(color = I("steelblue"), showlegend = F, text = ~X,
              hovertemplate = paste(
                "<b>Serie: %{text}</b><br>",
                "MAPEfacAnual: %{x}<br>",
                "MAPEmedia: %{y}<br>",
                "MAPEDesvio: %{z}",
                "<extra></extra>"
              )) %>%
        add_markers(data = dadosArma, x = ~MAPEfacAnual, y = ~MAPEmedia, z = ~MAPEdp)
    })

    # Show tabel containing MAPE mean, MAPE sd and acf 
    output$tabelaAvaliacaoArma = renderDataTable({
      avaliacoesarma = data.frame (resultados_ARMA( )$avaliacao)
      colnames (avaliacoesarma) = c ("MAPE media", "MAPE desvio", "Fac Anual", "SomRes")
      rownames(avaliacoesarma) = paste("Serie", 1:input$nPopArma)
      return (datatable (avaliacoesarma))
    })
  }
  
  output$downloadSerie_ARMA = downloadHandler (
    filename = function ( ) {
      paste("serieARMA.csv",sep="")
    },
    content = function (file) {
      write.table(data.frame (serieSint_ARMA()), file,
                  col.names = "Serie Anual",
                  row.names = F,
                  sep = ";",
                  dec = ",")
    })
})

# if cleaning button was pressed
observeEvent(input$limparButton_ARMA,{
  shinyjs::enable("goButton_ARMA")
  shinyjs::enable("type")
  shinyjs::disable("limparButton_ARMA")
  shinyjs::enable("nPopArma")
  shinyjs::hide("resultados_ARMA")
  shinyjs::enable("armazenarButton_ARMA")
  shinyjs::reset("resultados_ARMA")
  shinyjs::hide("plotly_avaliacoes_arma")
})

observeEvent(input$armazenarButton_ARMA,{
  
  tryCatch({ 
    
    shinyjs::disable("armazenarButton_ARMA")
    shinyjs::show("armazenando_msg_ARMA")
    shinyjs::hide("error_armazenar_ARMA")
    
    p_ARMA = input$p_ARMA
    q_ARMA = input$q_ARMA
    lags_ARMA = c(p_ARMA,q_ARMA)
    nAnos_ARMA = input$nsint_ARMA
    estacao_ARMA = input$estacoes_ARMA
    
    MediaArmazenar = avaliacaoAnualARMA$media() 
    DesvioArmazenar = avaliacaoAnualARMA$desvioPadrao() 
    KurtArmazenar = avaliacaoAnualARMA$kurt() 
    AssimetriaArmazenar = avaliacaoAnualARMA$assimetria() 
    CoefVarArmazenar =  avaliacaoAnualARMA$coefVar() 
    HurstArmazenar = hurstAnualARMA()  
    VolumeArmazenar = volumeARMA()
    somResArmazenar = somaRes_ARMA()
    acfAnual = data.frame (as.vector (acfAnualARMA()[-1]))  
    
    idEstacao_ARMA <- findID(estacao,input$estacoes_ARMA)
    idSERIE_SINTETICA <- registrarSSARMA(p_ARMA,q_ARMA,nAnos_ARMA,idEstacao_ARMA, input$type)
    inserirSS_ARMA(idSERIE_SINTETICA, serieSint_ARMA())
    inserirAvaliacaoSS_ARMA(idSERIE_SINTETICA,MediaArmazenar,DesvioArmazenar,AssimetriaArmazenar,KurtArmazenar,CoefVarArmazenar)
    inserirACF_ANUALSS(idSERIE_SINTETICA,acfAnual)
    inserirSomHurst_ARMA(idSERIE_SINTETICA,somResArmazenar,HurstArmazenar)
    
    if(!is.infinite(VolumeArmazenar) && is.numeric(VolumeArmazenar)){
      inserirVol_ARMA(idSERIE_SINTETICA,VolumeArmazenar)
    }
    
    shinyalert("Armazenado!","A serie foi armazenada com sucesso", type = "success")
  },
  error = function(err) {
    shinyjs::hide("armazenando_msg_ARMA")
    shinyjs::html("error_msg_armazenar_ARMA", err$message)
    shinyjs::show(id = "error_armazenar_ARMA", anim = TRUE, animType = "fade")
  },
  finally = {
    shinyjs::hide("armazenando_msg_ARMA")
    SSTable <- SeriesDesagregacao <- SeriesSinteticas()
    output$SeriesSinteticas<- DT::renderDataTable(SSTable,server = TRUE, selection = 'single')
    output$SeriesDesagregacao <- DT::renderDataTable(SeriesDesagregacao,server = TRUE, selection = 'single')
    
  })
  
})

observe ({
  if (input$type == 1) {
    updateSelectInput(session, "nSerieArma",
                      choices = 1,
                      selected = 1)
  }else {
    updateSelectInput (session, "nSerieArma",
                       choices = 1:input$nPopArma,
                       selected = input$nPopArma)
  }
})




