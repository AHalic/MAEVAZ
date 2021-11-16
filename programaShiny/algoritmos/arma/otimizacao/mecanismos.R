FNS_ARMA = function (populacao) {
    
    npIndividual = lapply (populacao, function (x)
                                unlist (dominanciaCompleta_ARMA (x, populacao)))
    
    np = unlist (lapply (npIndividual, sum))
    np = np-1
    
    return (np)
}

dominanciaCompleta_ARMA = function (termo, populacao) {
  lapply (populacao, function (x){
                     if (((termo$avaliacao$media >= x$avaliacao$media) && (termo$avaliacao$dp >= x$avaliacao$dp) &&
                          (termo$avaliacao$facAnual >= x$avaliacao$facAnual)) ||
                         ((termo$avaliacao$media >= x$avaliacao$media) && (termo$avaliacao$dp >= x$avaliacao$dp) &&
                          (termo$avaliacao$facAnual >= x$avaliacao$facAnual) && (termo$avaliacao$somRes >= x$avaliacao$somRes)) ||
                         ((termo$avaliacao$media >= x$avaliacao$media) && (termo$avaliacao$dp >= x$avaliacao$dp) && 
                          (termo$avaliacao$somRes >= x$avaliacao$somRes)) ||
                         ((termo$avaliacao$media >= x$avaliacao$media) && (termo$avaliacao$facAnual >= x$avaliacao$facAnual) &&
			                    (termo$avaliacao$somRes >= x$avaliacao$somRes)) ||
                         ((termo$avaliacao$dp >= x$avaliacao$dp) && (termo$avaliacao$facAnual >= x$avaliacao$facAnual) &&
			                    (termo$avaliacao$somRes >= x$avaliacao$somRes)))
                            return (1)
    
                     else
                            return (0)
                    })
}

CDA_ARMA = function (populacao) {
  nINDIVIDUO = length (populacao[[1]]$individuo)
  n = length (populacao)
  
  if (n > 2) {
    avaliacoes = sapply (populacao, function (x) x$avaliacao)
    avaliacoes = matrix (as.numeric (avaliacoes), ncol = n)
    p = 1:((length (avaliacoes)) / n)
    distancias = t (sapply (p, function (x)
                               distancia_ARMA (avaliacoes[x, ], n)))
    
    diversidade = apply (distancias, 2, sum)
    diversidade = order (diversidade, decreasing = T)
  }
  else
    diversidade = 1:n
  
  return (diversidade)
}

distancia_ARMA = function (avaliacoes, n) {
  
  dist = numeric (n)
  ord = order (avaliacoes)
  avMax = max (avaliacoes)
  avMin = min (avaliacoes)
  
  for (i in (2:(n-1))) {
    dist[ord[i]] = dist[ord[i-1]] + ((avaliacoes[ord[i+1]] - avaliacoes[ord[i-1]]) / (avMax - avMin))
  }
  
  dist[ord[1]] = dist[ord[n]] = Inf
  return (dist)
}

CCO_ARMA = function (populacao) {
  np = FNS_ARMA (populacao)
  p = sort (unique (np))
  fronteiras = lapply (p, function (x)
                          which (np %in% x))
  populacaoFronteiras = lapply (fronteiras, function (x)
                                            populacao[x])
  diversidadeFronteiras = lapply (populacaoFronteiras, CDA_ARMA)
  p = 1:length (p)
  diversidade = sapply (p, function (x)
                           fronteiras[[x]][diversidadeFronteiras[[x]]])
  diversidade = unlist (diversidade)
  # cat('diversidade: ', diversidade, "\n")
  populacao = populacao[diversidade]
  return (populacao)
}
