momentos_ARMA = function (entrada, parametros, lags, nS, constante) {
  # print("to dentro de momentos")
  # print(parametros)
  # # print("em cima foram os param")
  # print(constante)
  # print(lags)
  residuos = residuos_ARMA (entrada$serie, parametros, lags, constante)
  dpRes = residuos$dpRes
  # print(dpRes)
  serieSint = serieSinteticaAnual (parametros, dpRes, constante, lags, nS)
  serieSint = t ((t (serieSint) * entrada$dpHL) + entrada$mediaHL)
  serieSint = exp (serieSint)

  facAnual = autocorrelacaoAnual (serieSint, entrada$lagmax)[-1]
  # facAnual = autocorrelacaoAnual (serieSint, 12)
  # print("----------------------------------------")
  # print(facAnual)
  # print("----------------------------------------")
  media = mean(serieSint)
  dp = sd(serieSint)

  somRes = residuos$somRes
  # cat("som res: ", somRes, "\n")
  # print("to saindo de momentos")
  final = list (serie = serieSint, media = media, dp = dp, facAnual = facAnual, somRes = somRes)
  return (final)
}

avaliacao_ARMA = function (entrada, momentosS) {
  # print("to dentro da avaliacao")  
  mediaH = entrada$mediaH
  dpH = entrada$dpH
  facAnualH = entrada$facAnualH[-1]
  # print("cade-------------------------")
   
  
  MAPEMedia = sum (abs ((mediaH - momentosS$media) / mediaH)) 
  MAPEDesvio = sum (abs ((dpH - momentosS$dp)) / dpH)
  # print(facAnualH)
  # print(momentosS$facAnual)
  MAPEFacAnual = sum (abs ((facAnualH - momentosS$facAnual) / facAnualH))
  # print(MAPEMedia)
  # print(MAPEDesvio)
  
  # print("cade-------------------------")
  somRes = momentosS$somRes
  # print("to saindo da avaliacao")  
  
  final = list (media = MAPEMedia, dp = MAPEDesvio, facAnual = MAPEFacAnual, somRes = somRes)
  return (final)
}

estouro_ARMA = function (momentos) {
  if ((min (is.finite (momentos$media))) &&
      (min (is.finite (momentos$dp))) &&
      (min (is.finite (momentos$facAnual))) &&
      (min (is.finite (momentos$somRes)))) {
    return (FALSE)
  }
  
  return (TRUE)
}
