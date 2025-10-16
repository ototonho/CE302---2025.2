tinytex::install_tinytex()
install.packages("lubridate")
library(lubridate)
install.packages("hms")
library(hms)


vetor_numerico <- c(1, 5, 11, 33)
vetor_caracter <- c("hello", "world")
vetor_logico <- c(TRUE, TRUE, FALSE)

vetor_combinado <- c(vetor_numerico,
                     vetor_caracter,
                     vetor_logico,
                     "boo")

vetor_combinado
typeof(vetor_combinado)
typeof(vetor_numerico)
typeof(vetor_caracter)
typeof(vetor_logico)

View(iris)
iris[, -which(names(iris) == "Species")]
View(iris)

x <- c(4, 7, 2, 9)
x%%2 == 0 | x > 8

v <- 1:10
v[v>5]
v>5
subset(v, v>5)


View(mtcars)
mtcars[mtcars$mpg %in% 20:25, ]
mtcars[mpg >= 20 & mpg <= 25]

View(mtcars[mtcars$mpg >= 20 & mtcars$mpg <= 25, ])
mtcars[mtcars$mpg >= 20 & mtcars$mpg <= 25, ]
mtcars[which(mpg >= 20 & mpg <= 25), ]

iris[seq(10, 100, 5), ]
mtcars[seq(5, nrow(mtcars), 5), ]

v <- c(3,6,9,12,15)

v[v%%6==0]
v%%6

airquality[!is.na(airquality$Ozone), ]
View(airquality)

1:9[seq(1,9,by=2)]
seq(1,9,by=2)
seq(1,9,length.out=5)
c(1,3,5,7,9)

lst <- list(a = 1:5, b = list(c = 10, d = 20))
lst$d
lst$b[2]
lst[["b"]][["b"]]
lst$b[[d]]
lst[2][2]

rep(1:3, times=3)
rep(1:3, each = 3)

f <- factor(c("alta", "baixa", "media"), levels = c("baixa", "media", "alta"), ordered = TRUE)
is.ordered(f)
f[1] > f[3]
is.factor(f)

x <- 5
y <- 8
(x > 3 & y < 10) | !(x == 5)

rep(1:3, each = 2)

m <- matrix(1:6, nrow = 2)
is.matrix(m)
is.vector(m)
is.list(m)
seq(100, 10, by=-10)

x <- c(2, 5, 0, -3)
x^2 - 2 * x + 1

TRUE + FALSE * 5
rep(7, times=10)
x <- c(1, 2, 3, 4)
is.atomic(x)
is.list(x)
x <- seq(2,20,by=2)

x[x%%2==1]
x[seq(2,length(x),by=2)]

x[seq(1,length(x),by=2)]

x[seq(0,length(x),by=2)]
x[x%%2==0]
x[x%%2==1]


m <- matrix(1:9, 3, 3)
all(m > 0)
m > 0
any(m > 0)

v <- seq(1, 20)
b.
v[v%%2==0 & v<15]

v[v>2 & v<15]
v[v%%2!=0 & v<15]
rep(2^(1:1024), 10)
seq(2, 1024, by=2)
2^(0:10)
2^(1:10)
seq(0,1,length.out=11)


a <- 10
b <- 3
a %% b * 2 + a %/% b

View(mtcars[, -grep("mpg|hp", names(mtcars))])
View(mtcars)

v <- c(2,4,6,8,10)
v[v > 5]

mtcars[cyl %% 2 == 0, ]
View(mtcars[seq(2, nrow(mtcars), 2), ])

m <- matrix(1:9, nrow = 3, ncol = 3)
m[2,3]

iris[iris$Sepal.Length > mean(iris$Sepal.Length),]
mean(iris$Sepal.Length)

rep(1:4, times=5)

seq(1,20,4)
rep(seq(1,4),20)
