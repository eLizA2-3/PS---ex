
densitate_gauss = function(m, sigma, a, n)
{
  b = seq(0, a, n)
  #distributia normala standard
  y = dnorm(b, m, sigma) 
  plot(b,y,type="l", col="blue")
  print(y)
  
}

densitate_gauss(1,1.25, 12, 1)


