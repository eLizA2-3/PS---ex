#1

LNM_func = function(n, p)
{
  #se generează n variabile aleatoare cu distribuție geometrică și probab de succes p
  x = rgeom(n,p);
  media = mean(x) #media
  media_t = 1/p   #media teoretica
  
  cat("Media generata de variabilele aleatoare este ", media, "\n")
  cat("Media teoretica este ", media_t, "\n")
}


n_val = c(5000, 10000, 100000, 500000)
r_val = c(0.2, 0.6, 0.6, 0.8)

for (r in r_val) 
  for (n in n_val) 
    LNM_func(n, r)


#2


Student_TLC = function(n, r, N, z)
{
  suma=0

  #calculam variabilele aleatoare
  vector = rt(n, r)
  #calculez media variabilelor aleatoare
  vector_mean = mean(vector)
  
  #calculez valorile teoretice 
  media_teoretica = 0
  st_deviatia = sqrt(r / (r - 2))
  
  #deviatia standard
  dev_s = sd(vector)
  
  #calculam limitele
  inf_lim  =  media_teoretica + z * st_deviatia / sqrt(n)
  sup_lim  =  media_teoretica - z * st_deviatia / sqrt(n)
  
  for(i in 1:N) {
    vec = mean( vector);
    if(vec <= sup_lim && vec >= inf_lim) {
      suma = suma + 1;
    }
  }
  
  
  #calculez erorile 
  cat("TLC pentru distributia Student: ", suma/N, "\n")
  
  er_abs = abs(vector_mean - media_teoretica)
  er_abs_st = abs(dev_s - st_deviatia)
  
  cat("Eroarea absoluta standard este ", er_abs_st, "\n")
  cat("Eroarea absoluta medie este ", er_abs, "\n")

}
N = c(5000,10000, 20000)
z = c(-1.5, 0, -1.5)

for(i in 1:3)
  for(j in 1:3)
    Student_TLC(2, 50, N[i], z[j])

 #3
#programul aproximează prob de a obține un număr între h și k 
#într-un eșantion de n încercări, iar fiecare încercare are o prob de succes p

func_aprox = function(n, p, h, k) {
  m = n * p #media didstributiei binoamiale
  ds = sqrt(n * p * (1 - p)) #deviatia standard
  inf_lim = (h - m - 0.5) / ds #limita inferioara
  sup_lim = (k - m - 0.5) / ds #limita superioara
  
  # folosi funcția de distribuție normală standard pnorm pentru a aproxima probabilitatea 
  pr_aprox = pnorm(sup_lim) - pnorm(inf_lim) 
  
  return(pr_aprox)
}

print(func_aprox(80, 0.6, 10, 40))
#probabilitatea de a obține un număr între 10 și 39 într-un eșantion de 80 de încercări 
#cu o probabilitate de succes de 0.6 pentru fiecare încercare
