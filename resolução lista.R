df <- data.frame(a = 1:3, b = letters[1:3])
is.vector(df)
is.table(df)
is.factor(df)
is.matrix(df)
is.list(df)


v <- c(2,4,6,8,10)
v[v > 5]
v>5

is.ordered(df)

fator <- factor(c("alta","baixa","baixa","media",
                  "alta","media","baixa","media","media"),
                levels = c("baixa", "media", "alta"),
                ordered = TRUE)
is.ordered(fator)


df <- data.frame(x = 1:3, y = c("A","B","C"), stringsAsFactors = FALSE)
df$y <- factor(df$y, levels = c("C","B","A"), ordered = TRUE) 
print(df$y)

arr <- array(1:12, dim = c(2,3,2))
arr[1,2,2]

iris[iris$Sepal.Length > mean(iris$Sepal.Length), ] 

mean(iris$Sepal.Length)
2^3*2


a <- 10
b <- 3
a %% b * 2 + a %/% b

x <- list(a = 1:3, b = letters[1:3])
x[["b"]][1]
seq(5, 25, by = 4)

View(mtcars)
mtcars[5:15,]

mtcars[rep(5, nrow(mtcars)), ] 

x <- 5
y <- 8

(x > 3 & y < 10) | !(x == 5)
matrix(1:6, nrow = 3, ncol = 2) 


lst <- list(a = 1:5, b = list(c = 10, d = 20))
View(lst)

lst$b[2]

mtcars
m <- mtcars[, -grep("mpg|hp", names(mtcars))] 
View(m)
rep(1:5, each=3) 

rep(1:3, times=5) 

subset(iris, Species == "versicolor" & Species == "virginica") 
iris[which(iris$Species == "versicolor" | "virginica"), ] 
iris[, iris$Species=="versicolor"|iris$Species == "virginica"] 

m <- matrix(1:9, 3, 3)
all(m>0)

TRUE + FALSE * 5

x <- c(3, 6, 7, 12)
View(x)
