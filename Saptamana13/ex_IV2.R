# Nivelul de semnificație (alfa)
alfa = 0.05

# Nr total de componente
n = 150

# Nr de componente defecte
defecte = 20

# Procentul așteptat de componente defecte sub ipoteza nulă
p0 = 0.10

# Calculul procentului de componente defecte
p_prim = defecte / n

z_score = (p_prim - p0) / sqrt(p0 * (1 - p0) / n)


# Calculul scorului critic Z
critical_z = qnorm(1 - alfa, 0, 1)

# Afișarea scorului Z și scorului critic Z
print(z_score)
print(critical_z)

#z_score <= critical_z   nu putem respinge ipoteza nula si nici trage 
#concluzii că procentul componentelor defecte este mai mare decât 10% la un nivel de semnificație de 5%.
