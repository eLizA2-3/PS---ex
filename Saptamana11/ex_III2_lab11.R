#functiile de distributie exponențială pentru fiecare mecanic
mecanic1 = function(n) rexp(n, rate = 4)
mecanic2 = function(n) rexp(n, rate = 12)

# 10000 de eșantioane din distribuția exponențială pentru fiecare mecanic
set.seed(123) # seed-ul pentru a putea reproduce rezultatele
n = 10000
timp_servire1 = mecanic1(n)
timp_servire2 = mecanic2(3*n)

# media timpului de servire pentru fiecare mecanic
media1 = mean(timp_servire1)
media2 = mean(timp_servire2)

prob1 = rep(3/4, n)
prob2 = rep(1/4, 3*n)

medie_total = weighted.mean(c(timp_servire1, timp_servire2), c(prob1, prob2))

cat("Media timpului de servire pentru primul mecanic este ", media1, "\n")
cat("Media timpului de servire pentru al doilea mecanic este ", media2, "\n")
cat("Media estimata a timpului de servire este ", medie_total, "\n")
