cenarioSinteticoAnual = function (serieH, lags, n, tipo, ag) {
  # serieH = serie inserida por .txt
  # lags = valores de p e q 
  # n = tamanho da serie sintetica
  # tipo = 1 powel, tipo = 2 ga 
  
  serieAnualH = apply (serieH, 1, sum)
  
  serieHL = log (serieAnualH)
  mediaHL = mean (serieHL)

  dpHL = sd (serieHL)
  
  dpH = sd(serieAnualH)
  mediaH = mean(serieAnualH)
  # print("DPH e dps media")
  # print(dpH)
  # print(mediaH)
  # print("--------------------------------")

  #  TODO verificar lag
  # lagmax = lagAnualSignificativo(serieAnualH)
  
  serieHN = (serieHL - mediaHL) / dpHL
  lagmax = lagAnualSignificativo(serieAnualH)
  facAnualH = autocorrelacaoAnual(serieAnualH, lagmax)
  # print(mediaH)
  # print(mediaHL)
  # print(mean(serieHN))
  entrada = list(serieH = serieH, dpH = dpH, mediaH = mediaH, serie = serieHN, facAnualH = facAnualH,
                  dpHL = dpHL, mediaHL = mediaHL, lagmax = lagmax, mapeMax = ag[5], serieHL = serieHL)
  
  modelo = ARMA (serieHN, lags)
  
  if(tipo == 1){
    parametros = modelo$parametros
    cat("parametros arima0", parametros, "\n")
    residuos = modelo$residuos
    dpRes =  modelo$dpRes
    c = modelo$constante
  
    
    serieS = serieSinteticaAnual (parametros, dpRes, c, lags, n)
    serieS = (serieS * dpHL) + mediaHL
    serieS = exp (serieS)
    
    # autocorrelacaoAnual (serieS, 12)
    
    MAPEMedia = sum (abs ((mediaH - mean(serieS)) / mediaH)) 
    MAPEDesvio = sum (abs ((dpH - sd(serieS))) / dpH)

    MAPEFacAnual = sum (abs ((facAnualH - autocorrelacaoAnual (serieS, lagmax)) / facAnualH))
    
    cat("MAPEmedia", MAPEMedia, "\n")
    cat("MAPEdesvio", MAPEDesvio, "\n")
    cat("MAPEfac", MAPEFacAnual, "\n")
    
    final = list (serieSintetica = serieS, parametros = parametros, residuos = residuos)
    return (final)
  }
  if(tipo == 2){
    inicio = Sys.time ( )
    arquivos = NSGA_ARMA (serieAnualH, lags,
                     ag[1], ((ag[3]) / 100), ((ag[4]) / 100),
                     ag[2], ((ag[5])/100), n, entrada)
    fim = Sys.time ( )
    duracao = difftime (fim, inicio, units = c ("secs"))
    # print(duracao)
    
    
    parametrosIniciais = arquivos$arquivoParametrosIniciais
    parametros = arquivos$arquivoParametros
    # print(parametros)
    series = lapply(arquivos$arquivosSeries, as.matrix)
    avaliacoes = arquivos$arquivoAvaliacoes


    residuos = lapply(1:ag[1], function(x)
                                sqrt(avaliacoes[x, 4]))

    final = list (serieSintetica = series, parametros = parametros, residuos = residuos, avaliacao = avaliacoes, duracao = duracao, ciclos = arquivos$ciclos)
    return(final)
  }
  
}

ARMA = function (serieAnual, lags) {

  # TODO o intercept talvez seja na verdade a media, portanto
  #  c = media*(1 - sum(parametros ar))
  modelo = arima0 (ts (serieAnual), order = c (lags[1], 0, lags[2]), seasonal = list (order = c (0, 0, 0), period = NA),
                  xreg = NULL, include.mean = TRUE, delta = 0.01, transform.pars = TRUE, fixed = NULL, init = NULL, method = "ML")
  
  parametros = as.vector (modelo$coef)

  constante = parametros[length(parametros)]
  # print(constante)
  
  parametros = parametros[-length(parametros)]
  # print(constante/(1 - sum(parametros)))
  
  # auto = 1
  # if (lags[1] > 0) {
  #   auto = auto - sum(parametros)
  # }
  # c = mean(serieAnual)*auto
  # print(c)
  
  dpRes = sqrt (modelo$sigma2)
  # print(dpRes)
  residual = modelo$residuals
  
  final = list (parametros = parametros, dpRes = dpRes, constante = constante, residuos = residual, somRes = sum(residual*residual))
  return (final)
}

serieSinteticaAnual = function (parametros, dpRes, c, lags, n) {

  p = lags[1]
  q = lags[2]
  
  limInf = 0
  limSup = 0
  if (p > 0) {
    limInf = 1
    limSup = p
    phi = parametros[limInf : limSup]
  }
  if (q > 0) {
    limInf = limSup + 1
    limSup = limInf + q - 1
    tht = parametros[limInf : limSup]
  }
  
  
  residuoS = rnorm (n)
  residuoS = (residuoS - mean (residuoS)) / sd (residuoS)
  residuoS = residuoS * dpRes
  # print(residuoS)
  
  serieS = rep (0, n)
  
  for (v in ((max (p, q) + 1):n)) {
    auto = 0
    mm = 0
    if (p > 0) {
      for (i in (1:p))
        auto = auto + phi[i]*serieS[v-i]
    }
    if (q > 0) {
      for (j in (1:q))
        mm = mm + tht[j]*residuoS[v-j]
    }
    
    serieS[v] = c + auto + mm + residuoS[v]
  }
  
  # print(mean(serieS))
  # print(serieS)
  return (serieS)
}

residuos_ARMA = function(serie, parametros, lags, constante) {
  # cat("parametros: ")
  # print(parametros)
  # cat("constante: ", constante, "\n")
  # cat("lags: ")
  # print(lags)
  p = lags[1]
  q = lags[2]
  
  limInf = 0
  limSup = 0
  if (p > 0) {
    limInf = 1
    limSup = p
  
    phi = parametros[limInf : limSup]
  }
  if (q > 0) {
    limInf = limSup + 1
    limSup = limInf + q - 1
    
    tht = parametros[limInf : limSup]
  }
  
  residuo = rep(0, length(serie))
  serieAux = as.vector (t (serie))
  # print(residuo)
  # print(serieAux)
  
  # auto = 1
  # if (p > 0) {
  #   auto = auto - sum(phi)
  # }
  # c = mean(serie)*auto
  # print("-------------------")
  # print(mean(serie))
  # print(sum(phi))
  # print(auto)
  # print(c)
  # print("-------------------")
  
  for (v in ((max (p, q) + 1):(length (serie)))) {
    auto = 0
    mm = 0
    if (p > 0) {
      for (i in (1:p))
        auto = auto + phi[i]*serieAux[v-i]
    }
    if (q > 0) {
      for (j in (1:q))
        mm = mm + tht[j]*residuo[v-j]
    }
    # print("cade residuoosssss")
    # print(auto)
    # print(mm)
    # print(constante)
    residuo[v] = serieAux[v] - auto - mm - constante
    # print("erro residuoosssss")
  }
  
  dpRes = sd(residuo)
  somQuadRes = sum (residuo * residuo)
  # cat("dpres: ", dpRes, "e a somRes", somQuadRes, "\n")
  
  fim = list(somRes = somQuadRes, dpRes = dpRes, residuos = residuo)
  # print("aq n chega")
  return(fim)
}
