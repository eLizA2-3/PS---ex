#1
# punctul (a)
#functia numara de cate ori apare un element in vector
Count_elem = function(vec, element)
{
  count =0
  #calculze nr de aparitii ale lui elemnt in vec
  for(i in vec)
    if(i == element) count=count+1
  
  n=length(vec)
  
  if(count >= n/2 +1)
    return(TRUE)
  else
    return(FALSE)
}

#verifica daca elementul M se afla în vectorul vec de cel puțin n/2 + 1 ori
CautaElement = function(vec, k)
{
  n = length(vec)
  
  for(i in 1:k)
  {
    x = sample(vec, 1) #un element aleator
    
    if(Count_elem(vec, x))
      cat("Elementul M este: ", x, '\n')
    else
      print("Vectorul nu are elementul M")
  }
}

vector = c(4, 4, 2, 5, 5, 4, 4, 4, 2, 4, 4) 
k = 15  # Nr de repetari

CautaElement(vector, k)

#punctul (b)
#valoare pentru k astfel ca eroarea sa fie mai mica decat 10^−7
k >= log2(1 / 10^(-7))
k = ceiling(log2(1 / 10^(-7)))  #ceiling returneaza cel mai mare nr intreg >= ca numarul transmis
print(k)



#2
element_ith = function(i, A) 
{
  n = length(A)
  
  if (n == 1) #daca lungimea e 1 returneaza singurul element din A
    return(A[1])
  
  z = sample(A, 1) #un element aleator din A
  A_mai_mic = A[A < z] #un vector pt elementele din A < z
  A_mai_mare = A[A > z] #un vector pt elementele din A > z
  
  if (length(A_mai_mic) > i) #daca da - elementul de pe pozitia i se gaseste in A_mai_mic
    return(element_ith(i, A_mai_mic)) #apelez recursiv pt a cauta elem
  else if (n > i + length(A_mai_mare)) #daca da-elementul e in vectorul initial A
    return(z)
  else #elem de pe pozitia i se afla in A_mai_mare
    return(element_ith(i - n + length(A_mai_mare), A_mai_mare))

}
A = c(1, 2, 3, 4, 5, 6, 7, 8)
i = 5

print(element_ith(i, A))




#3
#punctul (a)
#estimeaza mediana lui S
mediana_monte_carlo = function(S, a) 
{
  n = length(S)
  m = floor(a * log(n))  # floor returneaza cel mai mare nr intreg <= ca numarul transmis
  # generez m elemente uniform aleator din S
  indici = sample(1:n, m, replace = FALSE)
  S_p = S[indici] #se obtine o submultime cu m elemente selectate aleator din S

  
  # Sortăm submulțimea S_p
  S_prim_sortat = sort(S_p)
  
  # calculez indicele medianei
  index = floor(m / 2)
  return(S_prim_sortat[index])  #mediana
}

S = runif(100, 1, 500) #vector cu 100 nr aleatore intre 1-500
a = 0.6

mediana = mediana_monte_carlo(S, a)
cat("Mediana este ", mediana)

#punctul (b)
# 1 - 2/n^2 >= 1 - 10^(-7)
# 2/n^2 <= 10^(-7)
# n^2 >= 2 / 10^(-7)
# n^2 >= 2 * 10^7
# n >= sqrt(2 * 10^7)
# n >= 4472.136
# Rezulta ca dimensiunea a lui S S pentru care 
#algoritmul de mai sus returneaza adevarata mediana a lui S
#cu probabilitate de cel putin 1 − 10^(-7) este 4472.136
