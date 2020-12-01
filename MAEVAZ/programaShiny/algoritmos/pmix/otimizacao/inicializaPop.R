#source('avaliacao.R')

geraIndividuo = function (entrada, lags, nS) {
  nINDIVIDUO = (sum (lags))*12
  individuoMIN = -1
  individuoMAX = 1
  individuo = runif (nINDIVIDUO, individuoMIN, individuoMAX)
  momentos = momentos (entrada, individuo, lags, nS)
  
  while (estouro (momentos)) {
    individuo = runif (nINDIVIDUO, individuoMIN, individuoMAX)
    momentos = momentos (entrada, individuo, lags, nS)
  }
  avaliacao = avaliacao (entrada, momentos)
  
  final = list (individuo = individuo, serie = momentos$serie, avaliacao = avaliacao)
  return (final)
}

avaliaIndividuo = function (entrada, lags, individuo, nS) {
  momentos = momentos (entrada, individuo, lags, nS)
  if (estouro (momentos))
    return (NULL)
  # print(momentos)
  
  avaliacao = avaliacao (entrada, momentos)
  # print(avaliacao)
  
  final = list (individuo = individuo, serie = momentos$serie, avaliacao = avaliacao)
  return (final)
}

geraPopulacao = function (entrada, lags, parametrosIniciais, nP, nS) {
  populacao = list ()
  if (is.null (parametrosIniciais)) {
    p = 1:nP
    populacao = lapply (p, function (x)
                           geraIndividuo (entrada, lags, nS))
  }
  
  else {
    p = 1:((length(parametrosIniciais)) / (12*(sum (lags))))
    # print(parametrosIniciais)
    
    populacao = lapply (p, function (x)
                           avaliaIndividuo (entrada, lags, parametrosIniciais[x, ], nS))
    # print(populacao[[10]])
    populacao = populacao[lengths(populacao) != 0]
    # print(populacao[[10]])
  
    # 0.7887616 0.6427017 0.7245622 0.55332 0.8537521 0.8952665 0.9555897 0.97924 0.952858 0.9612083 0.8932456 0.8832840
    if ((length (populacao)) < nP) {
      n = nP - (length (populacao))
      populacao = completaPopulacao (entrada, lags, populacao, n, nS)
    }
  }
  
  return (populacao)
}

completaPopulacao = function (entrada, lags, populacao, n, nS) {
  p = 1:n
  
  populacaoRestante = list ()
  populacaoRestante = lapply (p, function (x)
                                 cruzamentoBLX (entrada, lags, populacao, 1, -1, nS))
  
  populacaoFinal = c (populacao, populacaoRestante)
  return (populacaoFinal)
}

geraCruzamento = function (entrada, lags, populacao, Pc, Pm, nP, nS) {
  p = 1:nP
  
  novaPopulacao = list ()
  novaPopulacao = lapply (p, function (x)
                             cruzamentoBLX (entrada, lags, populacao, Pc, Pm, nS))
  
  return (novaPopulacao)
}

cruzamentoBLX = function (entrada, lags, populacao, Pc, Pm, nS) {
  nINDIVIDUO = sum (lags) * 12
  
  if ((runif (1, 0, 1)) < Pm) {
    individuo = geraIndividuo (entrada, lags, nS)
    return (individuo)
  }
    
  ALFA = 0.5
  beta = runif (nINDIVIDUO, -ALFA, 1 + ALFA)
  homog = runif (nINDIVIDUO, 0, 1) <= Pc
  
  pais = torneio (3, length (populacao))
  pai1 = populacao[[pais[1]]]$individuo
  pai2 = populacao[[pais[2]]]$individuo
  
  filho = pai1 + homog*beta*(pai2 - pai1)
  momentos = momentos (entrada, filho, lags, nS)
  
  while (estouro (momentos)) {
    pais = torneio (3, length (populacao))
    pai1 = populacao[[pais[1]]]$individuo
    pai2 = populacao[[pais[2]]]$individuo
    
    filho = pai1 + homog*beta*(pai2 - pai1)
    momentos = momentos (entrada, filho, lags, nS)
  }
  avaliacao = avaliacao (entrada, momentos)
  
  final = list (individuo = filho, serie = momentos$serie, avaliacao = avaliacao)
  return (final)
}

torneio = function (nPossibilidades, n) {
  nPossibilidades = min (nPossibilidades, n)
  possiveis = sample (n, nPossibilidades, replace = F)
  possiveis = sort (possiveis)
  possiveis = possiveis[1:2]
  
  return (possiveis)
}