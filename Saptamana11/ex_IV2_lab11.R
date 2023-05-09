# Setam numarul de zile si numarul de computere
nr_zile = 100
nr_comp = 40

#vector pt starea fiecarui calculator
calculatoare = rep(0, nr_comp)

# am ales ca un calculator sa fie infectat initial
comprob_infat = sample(1:nr_comp, 1)
calculatoare[comprob_infat] = 1

# Probabilitatea infectariii unuia curat
prob_inf = 0.2

#vector pt numarului de calculatoare infectate in fiecare zi
nr_infectate = rep(0, nr_zile)

# Cream un vector pt nr de zile in care cel putin 15 calculatoare sunt infectate
nr_infectate_15 = rep(0, nr_zile)

k = c(4, 6, 8, 10)

# Simulam evolutia virusului pentru fiecare zi
for (day in 1:nr_zile) {
  
  # Calculez probabilitatea de a infecta un calculator curat
  prob_inf = sum(calculatoare == 1) / nr_comp * 0.2
  
  # Infectarea calculatoarelor curate cu o probabilitate=prob_inf
  for (i in 1:nr_comp)
    if (calculatoare[i] == 0 && runif(1) < prob_inf) {
      calculatoare[i] = 1
    }
  
  # Alegem un k aleatoriu pentru a indeparta virusul
  k = sample(k, 1)
  
  # Daca sunt mai putin de k calculatoare infectate, le elimin
  if (sum(calculatoare == 1) < k) {
    calculatoare[calculatoare == 1] = 0
  } else {
    # Daca sunt cel putin k calculatoare infectate, elimin k dintre ele
    comp_inf_prob = which(calculatoare == 1)
    sterge_i = sample(comp_inf_prob, k)
    calculatoare[sterge_i] = 0
  }
  
  # Salvam numarul de calculatoare infectate in aceasta zi
  nr_infectate[day] = sum(calculatoare == 1)
  
  # Salvam numarul de zile in care cel putin 15 calculatoare sunt infectate
  nr_infectate_15[day] = sum(nr_infectate >= 15)
  
  # Daca toate calculatoarele sunt infectate, parasesc simularea
  if (sum(calculatoare == 0) == 0) 
    break
  
}

# Estimez probabilitatea ca, intr-o anumita zi, toate computerele sa fie infectate       #punctul a
toate_inf_prob = sum(nr_infectate == nr_comp) / length(nr_infectate)
cat("Probabilitatea ca toate calculatoarele sa fie infectate intr-o anumita zi = ", toate_inf_prob, "\n")




prob_inf15 = sum(nr_infectate >= 15) / length(nr_infectate)  ##punctul b
cat("Probabilitatea ca cel putin 15 calculatoare sa fie infectate intr-o anumita zi = ", prob_inf15, "\n")


set.seed(123)  #punctul c
ne = 1000
sample = matrix(sample(nr_infectate, replace = TRUE, size = length(nr_infectate) * ne), nrow = ne)
prob_15inf_c = apply(sample, 1, function(x) sum(x >= 15) / length(x)) 
inf_quantil = quantile(prob_15inf_c, 0.025)
sup_quantil = quantile(prob_15inf_c, 0.975)
cat("Probabilitatea ca cel putin 15 calculatoare sa fie infectate intr-o anumita zi cu o eroare de 0.01 cu probab de 0.95 = ", (prob_inf15 - inf_quantil), "\n")
