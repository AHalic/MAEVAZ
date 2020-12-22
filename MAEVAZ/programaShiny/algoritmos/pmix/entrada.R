entrada = function (serieH) {
  serieAnualH = apply (serieH(), 1, sum)
  mediaH = apply (serieH(), 2, mean)
  dpH = apply (serieH(), 2, sd)
  nH = length (serieH) / 12
  
  serieHL = log (serieH())
  mediaHL = apply (serieHL, 2, mean)
  dpHL = apply (serieHL, 2, sd)
  
  serieHN = t ((t (serieHL) - mediaHL) / dpHL)
  
  lagANUAL = lagAnualSignificativo (serieAnualH)
  # print(lagANUAL)
  lagMENSAL = lagMensalSignificativo (serieH())
  # print(lagMENSAL)
  facAnualH = autocorrelacaoAnual (serieAnualH, 12)
  # print("entrada")
  # print(facAnualH)
  facMensalH = autocorrelacaoMensal (serieH(), 12)
  
  final = list (serieH = serieH, mediaH = mediaH, dpH = dpH, lagAnual = lagANUAL, lagMensal = lagMENSAL, facAnualH = facAnualH, facMensalH = facMensalH,
                serieHN = serieHN, mediaHL = mediaHL, dpHL = dpHL)
  return (final)
}