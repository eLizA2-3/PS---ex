#1

int_incredere  = function(alfa, n, media, ds)
{
  critical_z = qnorm(1 - alfa/2, 0, 1) #cuantila
  
  inf_lim = media - critical_z*ds/sqrt(n)
  sup_lim = media + critical_z*ds/sqrt(n)
  
  int_incredere = c(inf_lim, sup_lim)
  cat("1) Intervalul de incredere: " ,int_incredere, '\n')
}

alfa = c(0.1, 0.05, 0.01) #90% 95% 99% - nivelul de semnificatie
n =  20 # nr masuratori
media = 138 #media masuratorilor
ds = 11 #deviatia standard

for(i in 1:3)
  int_incredere(alfa[i], n, media, ds)

#2
alfa = 0.05 #95% nivelul de semnificatie
n = 256
dispersia = 1.44
media = 18

s = sqrt(dispersia) #sigma
critical_z = qnorm(1 - alfa/2, 0, 1)
inf_lim = media - critical_z*s/sqrt(n)
sup_lim = media + critical_z*s/sqrt(n)

int_incredere = c(inf_lim, sup_lim)
cat("2) Intervalul de incredere: " ,int_incredere, '\n')




#3
clienti = function(alfa)
{
  n = 153 #nr total de clienti
  succese = 17 #nr de clienti nemultumiti dupa schimbare
  p_prim = succese/n  # Proportia de clienți nemulțumiți după schimbare
  p0 = 0.12  # Proportia de nemulțumire înainte de schimbare
  
  # Calcularea scorului testului
  z_score = abs(p_prim - p0) / sqrt(p0 * (1 - p0) / n)
  
  # Calcularea valorii critice z la nivelul de semnificație ales
  critical_z = qnorm(1 - alfa, 0, 1)
  
  # Afișarea rezultatelor
  print(z_score)
  print(critical_z)
  
  if (z_score < critical_z) cat("Schimbarea nu a avut un impact semnificativ\n")
  else cat("Schimbarea nu a avut un impact semnificativ\n")
  
}

cat("Nivel de semnificatie 1% \n", clienti(0.01))
cat("Nivel de semnificatie 5% \n", clienti(0.05))
