 grafic = function(p, n) {
  y = dgeom(1:n, p)
  barplot(y, space = 0.5, names.arg = 1:n, xlab = "Nr. încercări",
          ylab = "Probabilitatea", main = "Repartitia geometrică")
}

  