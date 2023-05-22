###################################################
#A1 (a)

#lam - valoarea medie a distribuției Poisson
#p - prob de succes într-o singură încercare în distribuția geom și binom
#n - numărul de încercări
#k - vector de valori de la care se vor calcula probab pt fiecare distribuție
graph_a=function(lam, p, n, k)
{
  #fiecare returneaza un vector de prob
  poisson=dpois(k:n, lam)
  geometric=dgeom(k:n, p)
  binomial=dbinom(k:n, n, p)
  
  #sunt puse impreuna si formeaza o matrice
  cases = rbind(poisson, geometric, binomial)
  
  barplot(cases,
          beside=TRUE, 
          col=c("gold", "orange", "pink"),
          names.arg=k:n,las=1,
          legend=c("P", "G", "B"),
          args.legend=list(x="top", cex=0.75))
}
graph_a(3.5, 0.25, 10, 4)
Sys.sleep(10)


###################################################
#A1 (b)
geometric_prob=function(n, k)
{
  sum=0     #pt prob ca e numar impar
  suma_f=0  #pt prob ca sunt nr >= 4
  suma_20=0 #pt prob ca sunt nr <= 20
  
  a=k:n
  for(i in a)
  {
    if(i %% 2 == 1)
      #se adaugă probab geometrică nr gasit cu o prob de succes de 0.5
      sum = sum+dgeom(i, 0.5) 
  }
  
  for(i in a)
  {
    if(i >=4)
      #se adaugă probab geometrică nr gasit cu o prob de succes de 0.1
      suma_f = suma_f+dgeom(i, 0.1)
  }
  
  for(i in a)
  {
    if(i <=20)
      #se adaugă probab geometrică nr gasit cu o prob de succes de 0.1
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
  esantion =read.csv(file_name, header=TRUE, sep=',')
  
  # Calculam statisticile pentru primul esantion
  esantion1 = esantion$P   #extrag coloana P
  media1 = mean(esantion1) #media
  mediana1 = median(esantion1) #mediana
  cvartil1 = quantile(esantion1, c(0.25, 0.75)) #cvartilele
  ds1 = sd(esantion1) #deviatia standard
  
  # Calculam statisticile pentru al doilea esantion
  esantion2 = esantion$S   #extrag coloana S
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
functie_a("note.csv")


###################################################
#A2 (b)
functie_valori_aberante = function(file_name, care_esantion)
{
  #citesc datele din fisier
  esantion =read.csv(file_name, header=TRUE, sep=',') 
  P = esantion[[1]] #coloana P
  S = esantion[[2]] #coloana S
  
  if (care_esantion == "P")
  {
    Q1 = quantile(P, 0.25) #cvartila Q1
    Q3 = quantile(P, 0.75) #cvartila Q3
    IQR = Q3 - Q1 #amplitudinea intercvatila
    
    # Determinam valorile aberante
    val_aberante = which(P < Q1 - 1.5*IQR | P > Q3 + 1.5*IQR)
    # Eliminam valorile aberante din esantion
    P <- subset(P, P >= Q1 - 1.5*IQR & P <= Q3 + 1.5*IQR)
    
    return(P)
  }
  else
  {
    Q1 = quantile(S, 0.25) #cvartila Q3
    Q3 = quantile(S, 0.75) #cvartila Q1
    IQR = Q3 - Q1 #amp1litudinea intercvartila
    
    # Determinam valorile aberante
    val_aberante = which(S < Q1 - 1.5*IQR | S > Q3 + 1.5*IQR)
    # Eliminam valorile aberante din esantion
    S = subset(S, S >= Q1 - 1.5*IQR & S <= Q3 + 1.5*IQR)
    
    return(S)
  }
  
}

cat("Esantionul P: a fost filtrat\n")
esantion_filtrat_P = functie_valori_aberante("note.csv", "P")
cat("Esantionul S: a fost filtrat\n")
esantion_filtrat_S = functie_valori_aberante("note.csv", "S")



###################################################
#A2 (c)

#scriu conținutul vectorilor "esantion_filtrat_P" și "esantion_filtrat_S" în fișiere separate, "P.txt" și "S.txt"
# append = FALSE : conținutul fișierelor va fi înlocuit cu noul conținut
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
