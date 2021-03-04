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
  

  #  TODO verificar lag
  # lagmax = lagAnualSignificativo(serieAnualH)
  
  facAnualH = acf (serieAnualH, lag.max = ag[7], type = c ("correlation"), plot = F, na.action = na.pass)
  facAnualH = as.numeric (facAnualH$acf)
  facAnualH = facAnualH[2]
  entrada = list(dpH = dpH, mediaH = mediaH, serie = serieAnualH, facAnualH = facAnualH,
                  dpHL = dpHL, mediaHL = mediaHL, lagmax = ag[7], mapeMax = ag[5])
  
  serieHN = (serieHL - mediaHL) / dpHL

  
  modelo = ARMA (serieHN, lags)
  if(tipo == 1){
    parametros = modelo$parametros

    residuos = modelo$residuos
    # print(residuos)
    # print(sum(residuos * residuos))
    dpRes =  modelo$dpRes
    c = modelo$constante
    
    serieS = serieSinteticaAnual (parametros, dpRes, c, lags, n)
    serieS = (serieS * dpHL) + mediaHL
    serieS = exp (serieS)
    
    final = list (serieSintetica = serieS, parametros = parametros, residuos = residuos)
    return (final)
  }
  if(tipo == 2){
    modeloGA = AG_ARMA(serieHN, lags, ag, n, entrada, modelo$parametros, modelo$constante)
    parametros = modeloGA$population
    
    tampop = 1:ag[1]
    serieS = lapply(tampop, function(x) 
                      residuos_ARMA(serieHN, parametros[x, ], lags, n, 1, entrada, modelo$constante))
    
    residuo = lapply(tampop, function(x) 
                                serieS[[x]]$residuo)
    serie = list()
    serie = lapply(tampop, function(x)
                            (serieS[[x]]$serieS))

    mediaS = sapply(serie, mean)
    dpS = sapply(serie, sd)
 
  
    avaliacaoga = sapply(tampop, function(x) avaliacoes(entrada, serieS[[x]]$serieS, mediaS[[x]], dpS[[x]]))

    avaliacaoga = t(avaliacaoga)
    
    avaliacaoga = data.frame (avaliacaoga)
    
    final = list (serieSintetica = serie, parametros = parametros, residuos = residuo, avaliacao = avaliacaoga)
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
  parametros = parametros[-length(parametros)]

  
  dpRes = sqrt (modelo$sigma2)

  residual = modelo$residuals
  
  final = list (parametros = parametros, dpRes = dpRes, constante = constante, residuos = residual)
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
  
  # TODO: a constante so é igual a media quando ar != 0
  if(p != 0)
    c = c*(1- sum(phi))
  
  residuoS = rnorm (n)
  residuoS = (residuoS - mean (residuoS)) / sd (residuoS)
  residuoS = residuoS * dpRes
  
  
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
    # TODO: mm n deveria estar subtraindo?
    serieS[v] = c + auto + mm + residuoS[v]
  }
  
  return (serieS)
}


residuos_ARMA = function(serie, parametros, lags, n, t, entrada, constante){
  
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
  
  # TODO: a constante so é igual a media qnd ar != 0
  if(p != 0)
    c = constante*(1- sum(phi))
  else
    c = constante

  residuo = numeric (length (serie))

  serieAux = as.vector (t (serie))
  

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
    residuo[v] = serieAux[v] + auto + mm + c
  }



  dpRes = sd(residuo)
  somQuadRes = sum (residuo * residuo)

  serieS = serieSinteticaAnual(parametros, dpRes, constante, lags, n)

  serieS = serieS* entrada$dpHL + entrada$mediaHL

  serieS = exp(serieS)


  if(t == 1){
    final = list(dpRes = dpRes, somQuadRes = somQuadRes, residuo = residuo, serieS = serieS)
    return(final)
  }

  if(t == 0){
    mediaH = entrada$mediaH
  
    media = mean(serieS)
  
    MAPEMedia = mean(abs ((entrada$mediaH - media) / entrada$mediaH))
  
    return(MAPEMedia)
  }
  
  facAnual = acf (serieS, lag.max = entrada$lagmax, type = c ("correlation"), plot = F, na.action = na.pass)
  facAnual = as.numeric (facAnual$acf)
  facAnual = facAnual[2]
  
  return(somQuadRes)
  return(facAnual)
}


AG_ARMA = function(serieHN, lags, ag, n, entrada, inicio, constante){
  tampop = ag[1]
  ciclomax = ag[2]
  probC = ag[3]
  probM = ag[4]
  

  
  # Criando população inicial a partir de arima
  low = c()
  up = c()
  for(i in 1:length(inicio)){
    if(inicio[i] < 0){
      low = append(low, inicio[i])
      up = append(up, -inicio[i]) 
    }
    else{
      low = append(low, -inicio[i])
      up = append(up, inicio[i])   
    }
  }
  
  sugestao = matrix(inicio, ncol = length(inicio),  byrow = TRUE, nrow = tampop-1)
 
  
  modelo = ga(type = "real-valued", fitness = function(x) -residuos_ARMA(serieHN, x, lags, n, 0, entrada, constante), lower = low, upper = up, popSize = tampop,
                  maxiter = ciclomax, pcrossover = probC/100, pmutation = probM/100,
                  suggestions = sugestao, run = 100, keepBest = TRUE, selection =  gareal_tourSelection, monitor = NULL)

  

  # modelo = ga(type = "real-valued", fitness = function(x) -residuos_ARMA(serieHN, x, lags, n, 0, entrada, constante), lower = low, upper = up, popSize = tampop, 
  #             maxiter = ciclomax, pcrossover = probC/100, pmutation = probM/100,
  #             suggestions = modelo@population, run = 70, keepBest = TRUE, selection =  gareal_tourSelection, monitor = NULL)
  
  
  final = list(population = modelo@population, ciclos = modelo@iter)
  return(final)
}

avaliacoes = function(entrada, sintetico, media, dp){
  mediaH = entrada$mediaH
  dpH = entrada$dpH
  facAnualH = entrada$facAnualH
  serie = entrada$serie
    
  facAnual = acf (sintetico, lag.max = entrada$lagmax, type = c ("correlation"), plot = F, na.action = na.pass)
  facAnual = as.numeric (facAnual$acf)
  facAnual = facAnual[2]
  
  
  MAPEMedia = abs ((mediaH - media) / mediaH)
  MAPEDesvio = abs ((dpH - dp)) / dpH
  
  
  # TODO: seria o MAPEFacAnual ou apenas o facAnual?
  MAPEFacAnual = sum(abs((facAnualH - facAnual) / facAnualH))/entrada$lagmax
  
  
  final = list(MAPEMedia = MAPEMedia, MAPEDesvio = MAPEDesvio, facAnual = facAnual)
  return(final)
}