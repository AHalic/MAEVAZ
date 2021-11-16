#source('mecanismos.R')

NSGA_ARMA = function (serieH, lags, nP, Pc, Pm, cicloMAX, MAXDiferenca, nS, entrada) {
  modelo = geraPinicial_ARMA (lags, entrada$serie, nP)
  
  parametrosIniciais = sapply (modelo, function (x) c(x$parametros, x$constante))
  parametrosIniciais = t (parametrosIniciais)
  
  populacao = geraPopulacao_ARMA (entrada, lags, parametrosIniciais, nP, nS)

  populacao = CCO_ARMA (populacao)

  
  ciclo = 0
  
  while (TRUE) {
  # while ((ciclo < cicloMAX) && (MAPEdiferenca_ARMA (populacao) > MAXDiferenca)){
    ciclo = ciclo + 1

    novaPopulalacao = geraCruzamento_ARMA (entrada, lags, populacao, Pc, Pm, nP, nS)
    populacaoTotal = c (populacao, novaPopulalacao)
    populacaoTotal = CCO_ARMA (populacaoTotal)
    populacao = populacaoTotal[1:nP]

    print(MAPEdiferenca_ARMA(populacao))
    if((ciclo >= cicloMAX) || (MAPEdiferenca_ARMA (populacao) < MAXDiferenca))
        break
  }
  # print(populacao)
  
  arquivoParametrosIniciais = arquivoParametrosIniciais_ARMA (modelo, lags)
  arquivosSeries = arquivosSeries_ARMA (populacao)
  arquivoParametros = arquivoParametros_ARMA (populacao, lags)
  arquivoAvaliacoes = arquivoAvaliacoes_ARMA (populacao)
  
  final = list (ciclos = ciclo,
                arquivoParametrosIniciais = arquivoParametrosIniciais,
                arquivosSeries = arquivosSeries,
                arquivoParametros = arquivoParametros,
                arquivoAvaliacoes = arquivoAvaliacoes)
  return (final)
}

MAPEdiferenca_ARMA = function (populacao) {
  nP = length (populacao)
  a = round (runif (1, 1, nP))
  individuo = populacao[[a]]$individuo
  # print(a)
  # print(individuo)
  # print(populacao[[a]]$parametros)
  
  
  MAPE = lapply (populacao, function (x)
                                   abs ((individuo - x$individuo) / individuo))
  # print(MAPE)
  MAPEdif = sum (unlist (MAPE)) / (nP * (length (individuo)))
  # print(MAPEdif)
  
  if (is.finite (MAPEdif))
    return (MAPEdif)
  else
    return (1)
}

arquivoParametrosIniciais_ARMA = function (modelo, lags) {
  p = 1:length(modelo)
  parametros = t (sapply (p, function (x) c (modelo[[x]]$somRes, modelo[[x]]$parametros)))
  parametros = data.frame (parametros)
  
  colnames(parametros) = c ("SomRes", nomesLags_ARMA (lags))
  return (parametros)
}

arquivosSeries_ARMA = function (populacao) {
  p = 1:length (populacao)
  series = lapply (p, function (x) {
                      serie = data.frame (populacao[[x]]$serie)
                      return (serie)
  })
  return (series)
}

arquivoParametros_ARMA = function (populacao, lags) {
  parametros = t (sapply (populacao, function (x)
                                     x$individuo))
  if(sum(lags[1], lags[2]))
    dim(parametros) = c(length(populacao), sum(lags[1], lags[2]))
  
  parametros = data.frame (parametros)
  # print(parametros)
  rownames (parametros) = c (1:length (populacao))
  colnames (parametros) = nomesLags_ARMA (lags)
  return (parametros)
}

arquivoAvaliacoes_ARMA = function (populacao) {
  avaliacoes = sapply (populacao, function (x)
                                  x$avaliacao)
  avaliacoes = t (matrix (as.numeric (avaliacoes), ncol = length (populacao)))
  avaliacoes = data.frame (avaliacoes)
  rownames (avaliacoes) = c (1:length (populacao))
  colnames (avaliacoes) = c("MAPEmedia", "MAPEdp", "MAPEfacAnual", "SomRes")
  return (avaliacoes)
}

nomesLags_ARMA = function (lags) {
  phis = NULL
  thts = NULL
  
  if (lags[1] > 0)
    phis = paste0 ("phi", 1:lags[1])
  if (lags[2] > 0)
    thts = paste0 ("tht", 1:lags[2])
  
  nomes = c (phis, thts)
  return (nomes)
}
