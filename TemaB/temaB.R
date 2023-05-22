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
r_val = c(0.2, 0.4, 0.6, 0.8)

for (r in r_val) 
  for (n in n_val) 
    LNM_func(n, r)

#########################################2

TLC_student = function(n, r, N, z) {
  sum=0
  medie_vec = c()
  #Se generează n variabile aleatoare cu distribuție Student
  vec_medie = mean(vec)  #media variabilelor aleatoare
  
  #valorile teoretice 
  media_t = 0
  var_t = (r / (r - 2)) / n
  # media si dispersia 
  vec_medie = mean(vec)
  vec_var = var(vec)
  
  #calculez limitele  utilizând formulele pentru intervalul de confidență bazat 
  #pe Teorema Limitei Centrale
  inf_lim = media_t + z * vec_var / sqrt(n) #limta inferioara
  sup_lim = media_t - z * vec_var / sqrt(n) #limita superioara
  
  for(i in 1:N) {
    x_n = mean(vec); #media
    #daca se afla in interiorul intervalului, se incrementeaza sum
    if(x_n <= sup_lim&& x_n >= inf_lim) { 
      sum = sum + 1;
    }
  }
  
  eroare_std = sqrt((r / (r - 2)) / n) #eroarea standard
  eroare_teor_std = sqrt(var_t)        #eroarea standard teoretica
  eroare_abs = abs(vec_medie - media_t) #eroarea absoluta
  eroare_abs_std = abs(eroare_std - eroare_teor_std) #eroarea absoluta standard
  cat("Eroarea standard = ", eroare_std, "\n")
  cat("Eroarea absoluta standard = ", eroare_abs_std, "\n")
  cat("Eroarea medie = ", eroare_abs, "\n")
  cat("Eroarea teoretica standard = " , eroare_teor_std, "\n")
}

N=c(5000,10000, 20000)
z=c(-1.5, 0, -1.5)

for(i in 1:3)
  for(j in 1:3)
    TLC_student(2, 50, N[i], z[j])



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
