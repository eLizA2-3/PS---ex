function_integrala = function(n) {
  suma = 0;
  for(i in 1:n) {
    u = rexp(1, 3);
    suma = suma + exp(-2*u*u);
  }
  return(suma/n);
}

MC_function= function(k, n) {
  estimates = 0;
  for(i in 1:k)
    estimates[i] = function_integrala(n);
  print(mean(estimates));
  print(sd(estimates));
}

MC_function(30, 50000)
cat("Eroarea absoluta este ", abs(function_integrala(50000) - sqrt(pi/8)))
cat("\nEroarea relativa este ", (abs(function_integrala(50000) - sqrt(3.14/8)) / sqrt(pi/8)) * 100)