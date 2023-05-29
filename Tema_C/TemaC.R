#1

volum = function(a, n)
{
  suma = 0;
  for(i in 1:n)
  {
    x1 = runif(1, -sqrt(a), sqrt(a))
    x2 = runif(1, -sqrt(a), sqrt(a))
    x3 = runif(1, 0, a)
    
    if(x3 >= (x1*x1+x2*x2))
      suma = suma + 1;
  }
  return(suma/n)
}
a_vec =  c(2,4,10)
n_vec  =  c(10000, 20000, 40000)

for(i in 1:3)
  for(j in 1:3)
  {
    cat("Volumul paraboloidului de revolutie cand n =  ", n_vec[i], "si a egal cu ", vector_a[j], "este = ",volum(vector_a[i], n_vec[j]), "\n")
    cat("Eroarea absoluta este: " , abs(volum(vector_a[i], n_vec[j]) - (pi*vector_a[i])/2), "\n")
    cat("Eroarea relativa este: ", abs(volum(vector_a[i], n_vec[j]) - ((pi*vector_a[i])/2))/abs((pi*vector_a[i])/2), "\n")
  }



#2
arie_patrulater = function()
{
  # numărul de puncte generate
  n = 20000
  
  # sunt generate n puncte aleatoare
  x = runif(n, 0, 10)
  y = runif(n, 0, 5)
  
  # Verificăm pentru fiecare punct dacă se află în interiorul patrulaterului T
  k = 0
  for (i in 1:n) {
    if (x[i] >= 0 && y[i] >= 0 && 3*y[i] <= x[i]+6 && y[i] <= 12-3*x[i])
      k = k + 1
  }
  
  # Estimez aria patrulaterului
  arie = 50 * k / n
  
  return(arie)
}
cat("Aria patrulaterului T este ", arie_patrulater(), "\n")


#3
#punctul (a)
integrare_function_a = function(n) {
  suma = 0;
  for(i in 1:n) {
    x = runif(1,-1, 1);
    suma = suma + (x+1)/sqrt(4-x*x);
  }
  return(suma/n);
}

#punctul (b)
integrare_function_b = function(n) {
  suma = 0;
  for(i in 1:n) {
    x = runif(1, -1000, 0);
    suma = suma + 1/(x*x+4);
  }
  return( suma/n);
}

#punctul (c)
integrare_function_c = function(n) {
  suma = 0;
  
  for(i in 1:n) {
    x = runif(1, -1000, 0);
    suma = suma + x*exp(x);
  }
  return(abs( suma/n));
}

cat("(a) Aria estimata este " , integrare_function_a(100), "\n");
cat("Eroarea absoluta este " , abs(integrare_function_a(100) - pi/3), "\n")
cat("Eroarea relativa este ", abs(integrare_function_a(100) - pi/3)/abs(pi/3), "\n")
cat("(b) Aria estimata este " ,integrare_function_b(100), "\n");
cat("Eroarea absoluta este " , abs(integrare_function_b(100) - pi/4), "\n")
cat("Eroarea relativa este ", abs(integrare_function_b(100) - pi/4)/abs(pi/4), "\n")
cat("(c) Aria estimata este " ,integrare_function_c(100), "\n");
cat("Eroarea absoluta este " , abs(integrare_function_c(100) - (-1)), "\n")
cat("Eroarea relativa este ", abs(integrare_function_c(100) - (-1))/abs(-1), "\n")





#4

#punctul (a)
fara_conturi_false = function() 
{
  nr_simulari = 100000  #nr de simulari
  nr_conturi_false = 10 #nr initial al conturilor false
  
  # Parametrii distribuției binomiale
  n = 10
  p = 0.2
  
  # Prob. de dezactivare a unui cont fals
  q = 0.9
  
  nr_zile = 0
  
  while (nr_conturi_false > 0) {
    #sunt generate nr de conturi false ce se adauga in ziua curenta
    nr_conturi_f_noi = rbinom(1, n, p)
    
    #conturile cu prob.= q sunt dezactivate si le scoatem din nr celor false
    nr_dezactivate = rbinom(1, nr_conturi_false, q)
    nr_conturi_false = nr_conturi_false - nr_dezactivate
    
    nr_conturi_false = nr_conturi_false + nr_conturi_f_noi #adaug conturile false noi
    
    nr_zile = nr_zile + 1 #increm. nr de zile
  }
  
  return(nr_zile)
}
# Se simuleaza evoluția numărului de conturi false și se estimeaza nr mediu de zile necesare
fara_conturi_false = replicate(nr_simulari, fara_conturi_false())
nr_mediu = mean(fara_conturi_false)

cat("Numărul mediu de zile necesare până când nu mai există conturi false si pana cand Iglon Mask devine noul proprietar al SocialNetworkOne este ", nr_mediu, "\n")



#punctul (b)

prob_dupa40 = function(nr_zile)
{
  n = 500 # numarul de conturi false initial
  p = 0.5 # Probabilitatea de adaugare a unui cont fals in fiecare zi
  q = 0.1 # Probabilitatea de dezactivare a unui cont fals
  
  conturi_false = rep(n, nr_zile) # numărul de conturi false inițial
  for (i in 1:nr_zile) {
    conturi_false[i] = conturi_false[i] + rbinom(1, size = n, prob = p) # Adăugare de conturi false
    conturi_false[i] = conturi_false[i] - rbinom(1, size = conturi_false[i], prob = q) # Dezactivare de conturi false
  }
  
  return(conturi_false[nr_zile])
}

nr_simulari = 10000
nr_zile = 40
nr_conturi_limita = replicate(nr_simulari, prob_dupa40(nr_zile))
prob = sum(nr_conturi_limita <= 50000) / nr_simulari
cat("Probabilitatea ca dupa", nr_zile, "zile sa existe cel mult 50000 de conturi false este ", prob, "\n")



#punctul (c)

prob_dupa40_eroare = function(nr_zile)
{
  n = 100 # numarul de conturi false initial
  p = 0.2 # Probabilitatea de adaugare a unui cont fals in fiecare zi
  q = 0.9 # Probabilitatea de dezactivare a unui cont fals
  
  conturi_false = n + rbinom(nr_zile, size = n, prob = p) # Adăugare de conturi false
  for (i in 1:nr_zile)
    conturi_false[i] = conturi_false[i] * rbinom(1, size = 1, prob = q) # Dezactivare de conturi false
  
  return(conturi_false[nr_zile])
  
}

nr_simulari = 100000 # numarul de simulări
nr_zile = 40
nr_conturi_limita = replicate(nr_simulari, prob_dupa40_eroare(nr_zile))
prob = sum(nr_conturi_limita <= 50000) / nr_simulari

cat("Probabilitatea ca dupa", nr_zile, "zile sa existe cel mult 50000 de conturi false este", prob, "\n")
