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


#b

MC_integr_b = function(n) {
  suma = 0;
  for(i in 1:n) {
    u = runif(1, 1, 4);
    suma = suma + exp(u);
  }
  return(10*suma/n);
}
cat("\n(b)\n")
cat("Aria estimata = " , MC_integr_b(100), "\n");
cat("Eroarea absoluta = " , abs(MC_integr_b(100) - 51.87987), "\n")
cat("Eroarea relativa = ", abs(MC_integr_b(100) - 51.87987)/abs(51.87987), "\n")


#d
MC_integr_d = function(n) {
  suma = 0;
  for(i in 1:n) {
    x = runif(1, 1, 100);
    suma = suma + 1/(4*x*x -1);
  }
  return( suma/n);
}
cat("(d)\n")
cat("Aria estimata = " ,MC_integr_d(100), "\n");
cat("Eroarea absoluta = " , abs(MC_integr_d(100) - log(3/4)), "\n")
cat("Eroarea relativa = ", abs(MC_integr_d(100) - log(3/4))/abs(log(3/4)), "\n")
