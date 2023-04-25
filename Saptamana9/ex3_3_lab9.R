  num_vector = scan("sample2.txt")
  
  #summary
  Summary = function(num_vector){
  print("summary")
  print(summary(num_vector))
  }
  
  Outliers_mean = function(num_vector){
  print("outliers_mean")
  m = mean(num_vector)
  s = sd(num_vector)
  outliers = vector()
  j = 0
  for (i in 1:length(num_vector)) {
    if (num_vector[i] < m - 2*s || num_vector[i] > m + 2*s) {
      j = j + 1
      outliers[j] = num_vector[i]
    }
  }
  print(outliers)
  }
  
  Outliers_iqr = function(num_vector) {
  print("outliers_iqr")
  q1 = quantile(num_vector, 0.25)
  q3 = quantile(num_vector, 0.75)
  iqr = q3 - q1
  min = q1 - 1.5 * iqr
  max = q3 + 1.5 * iqr
  outliers_iqr = num_vector[num_vector < min | num_vector > max]
  print(outliers_iqr)
  }
  
  Summary(num_vector)
  Outliers_mean(num_vector)
  Outliers_iqr(num_vector)
  





