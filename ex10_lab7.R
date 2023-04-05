poisson = function(l, n) 
{
  prob = dpois(0:n, l)
  barplot(prob, names.arg=0:n, space = 0.5, xlab="k", ylab="P", main="Repartitia Poisson")
}