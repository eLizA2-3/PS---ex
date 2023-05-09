# Simulam evolutia infectiei in timp
set.seed(123)
n <- 40 # numarul total de computere
p <- 0.2 # probabilitatea de infectare intre doua computere
infected <- rep(FALSE, n) # vector cu starea initiala a fiecarui computer
infected[1] <- TRUE # primul computer este infectat
max_days <- 1000
infected_count <- numeric(max_days)
# numarul maxim de zile pe care il simulam
for (day in 2:max_days) {
  # infectam fiecare computer cu probabilitate p in functie de starea celorlalte computere
  infected <- pmax(infected, rbinom(n, 1, pmax(p, rowSums(t(as.matrix(infected))) / n)))
  # alegem k computere infectate si le vindecam
  k <- sample(c(4, 6, 8, 10), size = 1)
  infected[which(infected == TRUE)][sample(which(infected==TRUE), min(k, sum(infected)))] <- FALSE
  
  # daca toate computerele sunt infectate, iesim din simulare
  if (all(infected)) {
    break
  }
}

# Probabilitatea ca toate computerele sa fie infectate este 1 daca au fost infectate toate inainte de a iesi din simulare, altfel este 0
if (all(infected)) {
  prob_all_infected <- 1
} else {
  prob_all_infected <- 0
}
print(prob_all_infected)
all_infected_prob <- sum(infected_count == n) / max_days
cat("Probabilitatea ca intr-o anumita zi toate computerele sa fie infectate:", all_infected_prob, "\n")


#b

# Simulam evolutia infectiei in timp si inregistram numarul de computere infectate in fiecare zi
set.seed(123)
n <- 40 # numarul total de computere
p <- 0.2 # probabilitatea de infectare intre doua computere
max_days <- 1000 # numarul maxim de zile pe care il simulam
infected_count <- numeric(max_days)
for (day in 1:max_days) {
  infected <- ifelse(day == 1, rep(FALSE, n), infected_count[day - 1] >= 1)
  # infectam fiecare computer cu probabilitate p in functie de starea celorlalte computere
  infected <- pmax(infected, rbinom(n, 1, pmax(p, rowSums(t(as.matrix(infected))) / n)))
  # alegem k computere infectate si le vindecam
  k <- sample(c(4, 6, 8,10), size = 1)
  infected[which(infected)][sample(which(infected), min(k, sum(infected)))] <- FALSE
  infected_count[day] <- sum(infected)
}           

at_least_15_infected_prob <- sum(infected_count >= 15) / max_days
cat("Probabilitatea ca intr-o anumita zi cel putin 15 computere sa fie infectate:", at_least_15_infected_prob, "\n")

#C
k <- 1.96 # pentru intervalul de incredere de 95%
n_simulations <- 10000 # numarul de simulari pentru a estima distributia
simulated_probs <- numeric(n_simulations)
for (sim in 1:n_simulations) {
  infected_count <- numeric(max_days)
  for (day in 1:max_days) {
    infected <- ifelse(day == 1, rep(FALSE, n), infected_count[day - 1] >= 1)
    infected <- pmax(infected, rbinom(n, 1, pmax(p, rowSums(t(as.matrix(infected))) / n)))
    k <- sample(c(4, 6, 8, 10), size = 1)
    infected[which(infected)][sample(which(infected), min(k, sum(infected)))] <- FALSE
    infected_count[day] <- sum(infected)
  }
  simulated_probs[sim] <- sum(infected_count >= 15) / max_days
}
estimated_prob <- mean(simulated_probs)
standard_error <- sd(simulated_probs) / sqrt(n_simulations)
lower_bound <- estimated_prob - k * standard_error
upper_bound <- estimated_prob + k * standard_error
cat("Probabilitatea ca intr-o anumita zi cel putin 15 computere sa fie infectate, cu o eroare de ±0.01 cu probabilitatea 0.95:",
    estimated_prob, "+/-", k * standard_error, "\n")
cat("Intervalul de incredere de 95% pentru aceasta probabilitate:", lower_bound, "-", upper_bound, "\n")