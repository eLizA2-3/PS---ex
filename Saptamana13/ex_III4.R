
t_conf_interval = function(alfa)
{
  esantion = scan ("data.txt")
  
  medie = mean(esantion)
  len = length(esantion)
  ds  =  sd(esantion) #deviatia standard
  std_err = ds/sqrt(len) #eroarea standard
  
  t_crit_val = qt(1 - alfa/2, len - 1) #valoarea critica a distributiei
  #limitele intervalului de incredere
  inf_lim = medie - t_crit_val*std_err
  sup_lim = medie + t_crit_val*std_err
  
  interval = c(inf_lim, sup_lim)
  print(interval)
}

alfa = 0.05
t_conf_interval(alfa)