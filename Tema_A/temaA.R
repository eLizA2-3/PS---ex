###################################################
#A1 (a)
graph_a=function(lam, p, n, k)
{
  poisson=dpois(k:n, lam)
  geometric=dgeom(k:n, p)
  binomial=dbinom(k:n, n, p)
  
  cases = rbind(poisson, geometric, binomial)
  
  barplot(cases,
          beside=TRUE, 
          col=c("gold", "orange", "pink"),
          names.arg=k:n,las=1,
          legend=c("P", "G", "B"),
          args.legend=list(x="top", cex=0.75))
}
graph_a(3.5, 0.25, 10, 4)


###################################################
#A1 (b)
geometric_prob=function(n, k)
{
  sum=0
  suma_f=0
  suma_20=0
  
  a=k:n
  for(i in a)
  {
    if(i %% 2 == 1)
      sum = sum+dgeom(i, 0.5)
  }
  
  for(i in a)
  {
    if(i >=4)
      suma_f = suma_f+dgeom(i, 0.1)
  }
  
  for(i in a)
  {
    if(i <=20)
      suma_20 = suma_20+dgeom(i, 0.1)
  }
  print(sum)
  print(suma_f)
  print(suma_20)
}
geometric_prob(10, 1)


###################################################
#A1 (c)
poisson_prob = function(lam) {
  k = 0
  p = 1
  while (p >= 1e-7) {
    p = ppois(k, lam, lower.tail = FALSE)
    k = k + 1
  }
  return(k)
}

cat("Cea mai mica valoare a lui k0 a.i P(Y >= k0) < 1e-7 este ", poisson_prob(2.8), "\n")

####################################################
 #A2 (a)    
  functie_a = function(file_name) 
  {
    # Citesc datele din fisier
    esantion = scan(file_name)
  
    # Calculam statisticile pentru primul esantion
    esantion1 = esantion[1:193]
    media1 = mean(esantion1)
    mediana1 = median(esantion1)
    cvartil1 = quantile(esantion1, c(0.25, 0.75))
    ds1 = sd(esantion1)
  
    # Calculam statisticile pentru al doilea esantion
    esantion2 = esantion[194:386]
    media2 = mean(esantion2)
    mediana2 = median(esantion2)
    cvartil2 = quantile(esantion2, c(0.25, 0.75))
    ds2 = sd(esantion2)

    cat("\nStatistici pentru primul esantion:\n")
    cat("Media: ", round(media1, 3), "\n") 
    cat("Mediana: ", round(mediana1, 3), "\n")
    cat("Deviatia standard: ", round(ds1, 3), "\n")
    cat("Cvartila 1: ", round(cvartil1[1], 3), "\n")
    cat("Cvartila 2: ", round(cvartil1[2], 3), "\n\n")
  
    cat("Statistici pentru al doilea esantion:\n")
    cat("Media: ", round(media2, 3), "\n") 
    cat("Mediana: ", round(mediana2, 3), "\n")
    cat("Deviatia standard: ", round(ds2, 3), "\n")
    cat("Cvartila 1: ", round(cvartil2[1], 3), "\n")
    cat("Cvartila 2: ", round(cvartil2[2], 3), "\n\n\n")
  
 }
 functie_a("date.txt")
 
 
 ###################################################
 #A2 (b)
 functie_valori_aberante = function(file_name, care_esantion)
 {
    esantion = scan(file_name) 
    P = esantion[1:193]
    S = esantion[194:386]
    if (care_esantion == "P")
    {
      # Calculam cvartilele 1 si 3 si amp1litudinea intercvartila
      Q1 = quantile(P, 0.25)
      Q3 = quantile(P, 0.75)
      IQR = Q3 - Q1
      # Determinam valorile aberante
      val_aberante = which(P < Q1 - 1.5*IQR | P > Q3 + 1.5*IQR)
      # Eliminam valorile aberante din esantion
      P = P[-val_aberante]
      return(P)
    }
    else
    {
      # Calculam cvartilele 1 si 3 si amp1litudinea intercvartila
      Q1 = quantile(S, 0.25)
      Q3 = quantile(S, 0.75)
      IQR = Q3 - Q1
      # Determinam valorile aberante
      val_aberante = which(S < Q1 - 1.5*IQR | S > Q3 + 1.5*IQR)
      # Eliminam valorile aberante din esantion
      S = S[-val_aberante]
      return(S)
    }
    
 }

 cat("Esantionul P: a fost filtrat\n")
 esantion_filtrat_P = functie_valori_aberante("date.txt", "P")
 cat("Esantionul S: a fost filtrat\n")
 esantion_filtrat_S = functie_valori_aberante("date.txt", "S")
 

 
 ###################################################
 #A2 (c)

 write(esantion_filtrat_P, file = "P.txt", append = FALSE, sep = "\n")
 write(esantion_filtrat_S, file = "S.txt", append = FALSE, sep = "\n")
 
 grafic = function(file_name)
 {
   # Citim esantionul din fisier
   esantion = scan(file_name)
   
   # Reprezentam grafic distributia frecventelor
   hist(esantion, breaks = 59, right = FALSE, freq = TRUE,
        col = "gold", main = "Distributia frecventelor", xlab = "Valoarea", ylab = "Frecventa")
 }

 grafic("P.txt")
 Sys.sleep(10) #pune pauza 10s pt a vedea schimbarea graficelor celor 2 esantioane
 grafic("S.txt")
  