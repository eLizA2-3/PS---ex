
LNM_student = function(n, r)
  {
  a = rt(n, r)
  medie_esantion = mean(a) #media esantion
  ds = sd(a) #deviatie standard esantion
  medie_as = 0 #medie asteptata
  ds_as = sqrt(r/(r-2)) #deviatie standard asteptata
  
  if(abs(medie_esantion - medie_as < ds_as/sqrt(n)) && abs(ds - ds_as) < ds_as/sqrt(n))
    cat("LNM este verificată pentru distribuția Student(",r,") cu n = ", n, "\n\n")
  else 
    cat("LNM nu este verificată pentru distribuția Student(",r,") cu n = ", n, "\n\n")
}

n_val = c(1000, 10000, 100000, 1000000)
r_val = c(2, 3, 4, 5)

for (r in r_val) 
  for (n in n_val) 
    LNM_student(n, r)

