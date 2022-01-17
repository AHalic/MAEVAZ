#source('avaliacao.R')

variaIndividuo_ARMA = function(parametros, constante, serie, lags) {
  n = 1:length(parametros)
  parametros = sapply(parametros, function(x) runif(1, x-0.025, x+0.025))
  constante = runif(1, constante-0.025, constante+0.025)
  # print(constante)
  residuos = residuos_ARMA(serie, parametros, lags, constante)
  
  final = list(parametros = parametros, dpRes = residuos$dpRes, constante = constante, residuos = residuos$residuos, somRes = residuos$somRes)
  # print(final)
  return(final)
}

geraPinicial_ARMA = function(lags, serie, n) {
  p = 1:n
  cont = length(n)*0.1
  saidas = lapply (p, function (x) {
                        individuo = ARMA (serie, lags)
                        if (cont > 0) {
                          cont = cont - 1
                          individuo = variaIndividuo_ARMA(individuo$parametros, individuo$constante, serie, lags)
                        }
                        return(individuo)
                      })
  
  # print(saidas)

  # final = list (parametros = parametros, dpRes = dpRes, constante = constante, residuos = residual, somRes = sum(residual*residual))
  return(saidas)
}

geraIndividuo_ARMA = function (entrada, lags, nS) {
  # print("eu entrei aq")
  nINDIVIDUO = (sum (lags)) + 1
  individuoMIN = -1
  individuoMAX = 1
  individuo = runif (nINDIVIDUO, individuoMIN, individuoMAX)
  
  constante = individuo[length(individuo)]
  
  auto = 1
  if (lags[1] > 0) {
    auto = auto - sum(individuo[1:lags[1]])
  }
  constante = constante*auto
  # cat("constante ", constante, "\n")
  
  individuo = individuo[-length(individuo)]
  
  momentos = momentos_ARMA (entrada, individuo, lags, nS, constante)
  
  while (estouro_ARMA (momentos)) {
    individuo = runif (nINDIVIDUO, individuoMIN, individuoMAX)
    momentos = momentos_ARMA (entrada, individuo, lags, nS, constante)
  }
  avaliacao = avaliacao_ARMA (entrada, momentos)
  
  final = list (individuo = individuo, serie = momentos$serie, avaliacao = avaliacao, constante = constante)
  return (final)
}

avaliaIndividuo_ARMA = function (entrada, lags, individuo, nS, constante) {
  # print(individuo)
  # print("const:")
  # print(constante)
  momentos = momentos_ARMA (entrada, individuo, lags, nS, constante)
  if (estouro_ARMA (momentos))
    return (NULL)
  # print(momentos)
  
  avaliacao = avaliacao_ARMA (entrada, momentos)
  # print(avaliacao)
  
  final = list (serie = momentos$serie, individuo = individuo, avaliacao = avaliacao, constante = constante)
  return (final)
}

geraPopulacao_ARMA = function (entrada, lags, parametrosIniciais, nP, nS) {
  populacao = list ()
  if (is.null (parametrosIniciais)) {
    p = 1:nP
    populacao = lapply (p, function (x) geraIndividuo_ARMA (entrada, lags, nS))
  }
  
  else {
    p = 1:((nrow(parametrosIniciais)) / ((sum (lags))))
    # print("-/-/-/-/-/-/-//--/---/-/")
    # print(p)
    # print("-/-/-/-/-/-/-//--/---/-/")
    # print(parametrosIniciais)
    # print((nrow(parametrosIniciais)) / ((sum (lags))))
    
    populacao = lapply (p, function (x){
                              # print(x)
                              # print(parametrosIniciais[x, -ncol(parametrosIniciais)])
                              # print(parametrosIniciais[x, ncol(parametrosIniciais)])
                              avaliaIndividuo_ARMA (entrada, lags, parametrosIniciais[x, -ncol(parametrosIniciais)], nS, parametrosIniciais[x, ncol(parametrosIniciais)])
                            })

    populacao = populacao[lengths(populacao) != 0]

    if ((length (populacao)) < nP) {
      n = nP - (length (populacao))
      populacao = completaPopulacao_ARMA (entrada, lags, populacao, n, nS)
    }
  }
  
  return (populacao)
}

completaPopulacao_ARMA = function (entrada, lags, populacao, n, nS) {
  p = 1:n
  
  populacaoRestante = list ()
  populacaoRestante = lapply (p, function (x)
                                  cruzamentoBLX_ARMA (entrada, lags, populacao, 1, -1, nS))
  
  populacaoFinal = c (populacao, populacaoRestante)
  return (populacaoFinal)
}

geraCruzamento_ARMA = function (entrada, lags, populacao, Pc, Pm, nP, nS) {
  p = 1:nP
  # print("##################")
  # print(constante)
  # print("##################")
  novaPopulacao = list ()
  novaPopulacao = lapply (p, function (x)
                                cruzamentoBLX_ARMA (entrada, lags, populacao, Pc, Pm, nS))
  
  return (novaPopulacao)
}

cruzamentoBLX_ARMA = function (entrada, lags, populacao, Pc, Pm, nS) {
  nINDIVIDUO = sum (lags) + 1

  if (runif (1, 0, 1) < Pm) {
    individuo = geraIndividuo_ARMA (entrada, lags, nS)
    return (individuo)
  }
    
  ALFA = 0.5
  beta = runif (nINDIVIDUO, -ALFA, 1 + ALFA)
  homog = runif (nINDIVIDUO, 0, 1) <= Pc
  
  betaC = beta[length(beta)]
  beta = beta[-length(beta)]
  homogC = homog[length(homog)]
  homog = homog[-length(homog)]
  
  
  pais = torneio_ARMA (3, length (populacao))
  pai1 = populacao[[pais[1]]]$individuo
  pai2 = populacao[[pais[2]]]$individuo

  pai1C = populacao[[pais[1]]]$constante
  pai2C = populacao[[pais[2]]]$constante
  
  filho = pai1 + homog*beta*(pai2 - pai1)
  filhoC = pai1C + homogC*betaC*(pai2C - pai1C)
  
  momentos = momentos_ARMA (entrada, filho, lags, nS, filhoC)
  
  while (estouro_ARMA (momentos)) {
    pais = torneio_ARMA (3, length (populacao))
    pai1 = populacao[[pais[1]]]$individuo
    pai2 = populacao[[pais[2]]]$individuo
    pai1C = populacao[[pais[1]]]$constante
    pai2C = populacao[[pais[2]]]$constante
    
    filho = pai1 + homog*beta*(pai2 - pai1)
    filhoC = pai1C + homogC*betaC*(pai2C - pai1C)
    
    momentos = momentos_ARMA (entrada, filho, lags, nS, filhoC)
  }
  avaliacao = avaliacao_ARMA (entrada, momentos)
  
  # final = list (individuo = filho, serie = momentos$serie, avaliacao = avaliacao)
  final = list (individuo = filho, serie = momentos$serie, avaliacao = avaliacao, constante = filhoC)
  return (final)
}

torneio_ARMA = function (nPossibilidades, n) {
  nPossibilidades = min (nPossibilidades, n)
  possiveis = sample (n, nPossibilidades, replace = F)
  possiveis = sort (possiveis)
  possiveis = possiveis[1:2]
  
  return (possiveis)
}