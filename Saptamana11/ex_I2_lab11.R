# Definim functia parabola
parabola = function(x) {-2*x^2 + 5*x - 2}

# Generam 10000 de puncte aleatoare uniform distribuite in [0,2] x [0,2]
set.seed(123)
n = 10000
x = runif(n, min = 0, max = 2)
y = runif(n, min = 0, max = 2)

puncte = sum(y < parabola(x)) #puncte sub parabola

# Calculam aria sub parabola
aria_estimata = puncte / n * 4

# Calculam aria exacta prin integrare
integrala = function(x) {-2*x^2 + 5*x - 2}
aria_exacta = integrate(integrala, lower = 0, upper = 2)$value

# Calculam eroarea relativa
er_relativa = abs(aria_estimata - aria_exacta) / aria_exacta

print(aria_estimata)
print(aria_exacta)
print(er_relativa)