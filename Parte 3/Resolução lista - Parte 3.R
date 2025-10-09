# Resolução lista - Parte 3

# 1)

bhaskara <- function(a, b = 1, c = 0) {
  delta <- b^2 - 4 * a * c
  x <- (-b + c(-1, 1) * sqrt(delta))/(2 * a)
  return(x)
}
bhaskara(2, c = 5)

# 7)

i <- 1 
repeat { 
  if(i > 3) 
    break 
  i <- i + 1 
  } 
i

# 9)

x <- 1 
while(x < 3) { 
  x <- x + 1 
  } 
print(x)

# 12)

x <- 3 
switch(x, "um", "dois", "três")

# 25)

s <- 0 
for(i in 1:5) { 
  if(i %% 2 == 0) next 
  s <- s + i 
  } 
s

# 28)

x <- 1:4 
ifelse(x > 2, x^2, 0)

# 29)

bhaskara <- function(a, b = 1, c = 0) {
  delta <- b^2 - 4 * a * c
  x <- (-b + c(-1, 1) * sqrt(delta))/(2 * a)
  return(x)
}

bhaskara(a = 2)


#) 32)

x <- 0 
while(TRUE) { 
  x <- x + 1 
  if(x == 4) 
    break 
  } 
x



