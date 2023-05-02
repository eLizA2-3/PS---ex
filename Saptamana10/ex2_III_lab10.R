
TLC_Gamma = function(l, a, n, N, z) #l-lambda
{
  suma = 0;
  medie = a/l
  dispersia = a/l*l
  lower = medie + z * dispersia / sqrt(n)
  upper = medie - z * dispersia / sqrt(n)
  
  for(i in 1:N) {
    x = mean(rgamma(n, a));
    if(x <= upper && x >= lower) {
      suma = suma + 1;
    }
  }
  
  return(suma/n)
}

n_val = c(5000,10000, 20000)
z = c(-1.5, 0, -1.5)

for(i in 1:3)
  for(j in 1:3)
    cat("TLC_Gamma cand n_val:", n_val[i], 
        "si z:", z[j], "este = ", TLC_Gamma(1.5, 2, 50, n_val[i], z[j]), "\n")