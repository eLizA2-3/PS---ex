

interval_incredere = function(alfa)
{
  esantion = scan("history.txt")
  
  media_vec = mean(esantion)
  len = length(esantion)
  
  s = sqrt(100) #radical din dispersie   #sigma
  z_critic = qnorm(1 - alfa/2, 0, 1)
  
  a = media_vec - z_critic*s / sqrt(len)
  b = media_vec + z_critic*s / sqrt(len)
  
  interval = c(a, b)
  print(interval)
}

alfa = 0.05
interval_incredere(alfa)
