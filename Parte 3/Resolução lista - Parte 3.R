# Resolução lista - Parte 3

# Exercício 1

animal <- "gato"
som <- switch(animal,
              "cachorro" = {
                latir()
                },
              "vaca" = {
                mugir()
                },
              {
                silencio()
                })
som

# Exercício 5

x <- 1:4 
ifelse(x > 2, x^2, 0)

# Exercício 14

bhaskara <- function(a, b = 1, c = 0) {
  delta <- bˆ2 - 4 * a * c
  x <- (-b + c(-1, 1) * sqrt(delta))/(2 * a)
  return(x)
}
args(bhaskara)
bhaskara(a = 2)

# Exercício 15

x <- 3 
switch(x, "um", "dois", "três")

# Exercício 21

s <- 0 
for(i in 1:5) {
  if(i %% 2 == 0) next 
  s <- s + i 
  } 
s

# Exercício 24

if (a == 0) 
  stop("O valor de A não pode ser zero!")

# Exercício 25

formals(bhaskara)

# 26)

for(i in 1:3) 
{ if(i == 2) 
    break 
  print(i) }

# 27)

x <- 0 
while(TRUE) { 
  x <- x + 1 
  if(x == 4) 
    break 
  } 
x
