variabila_aleatoare = function(x_var, prob) 
{
  # verific daca nr variabilelor coincide cu nr probabilitatilor
  if (abs(sum(prob) - 1) > 1e-6) {
    cat("Nr variabilelor e diferit de nr probabilitatilor! Ele vor fi normalizate.\n")
    prob = prob / sum(prob)
  }
  
  cum_prob = cumsum(prob)
  r = runif(1)
  
  for (i in seq_along(cum_prob)) {
    if (r < cum_prob[i]) 
      return(x_var[i])
  }
  
  cat("Nu se poate genera o valoare!")
  return
}

x_var = c(10, 22, 3, 5)
prob = c(0.1, 0.4, 0.2, 0.3)

print(variabila_aleatoare(x_var, prob))

