#1

LNM_func = function(n, p)
{
  #se genereaza variabilele aleatoare 
  x = rgeom(n,p);
  media = mean(x)
  media_t = 1/p
  
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
  #variabilele aleatoare
  vec = rt(n, r)
  #media variabilelor aleatoare
  vec_medie = mean(vec)
  
  #valorile teoretice 
  media_t = 0
  var_t = (r / (r - 2)) / n
  # media si dispersia 
  vec_medie = mean(vec)
  vec_var = var(vec)
  
  #limitele
  inf_lim = media_t + z * vec_var / sqrt(n)
  sup_lim = media_t - z * vec_var / sqrt(n)
  for(i in 1:N) {
    x_n = mean(vec);
    if(x_n <= sup_lim&& x_n >= inf_lim) {
      sum = sum + 1;
    }
  }
  
  #erorile
  eroare_std = sqrt((r / (r - 2)) / n)
  eroare_teor_std = sqrt(var_t)
  eroare_abs = abs(vec_medie - media_t)
  eroare_abs_std = abs(eroare_std - eroare_teor_std)
  cat("Eroarea std = ", eroare_std, "\n")
  cat("Eroarea absoluta std = ", eroare_abs_std, "\n")
  cat("Eroarea medie = ", eroare_abs, "\n")
  cat("Eroarea teoretica std = " , eroare_teor_std, "\n")
}
N=c(5000,10000, 20000)
z=c(-1.5, 0, -1.5)
for(i in 1:3)
  for(j in 1:3)
    TLC_student(2, 50, N[i], z[j])



 #3
func_aprox = function(n, p, h, k) {
  m = n * p #media didstributiei binoamiale
  ds = sqrt(n * p * (1 - p)) #deviatia dstandard
  inf_lim = (h - m - 0.5) / ds #limita inferioara
  sup_lim = (k - m - 0.5) / ds #limita superioara
  
  # folosi funcția de distribuție normală standard pnorm pentru a aproxima probabilitatea 
  pr_aprox = pnorm(sup_lim) - pnorm(inf_lim) 
  return(pr_aprox)
}

print(func_aprox(80, 0.6, 10, 40))
#probabilitatea de a obține un număr între 10 și 39 într-un eșantion de 80 de încercări 
#cu o probabilitate de succes de 0.6 pentru fiecare încercare
