### Prova 2

require(tidyverse)

# Questão 1

Notas <- c("João" = 39, "Maria" = 44, "José" = 80, "Ana" = 85, "Pedro" = 60, "Paula" = 33, "Carlos" = 72, "Mariana" = 56, "Fernando" = 89, "Luiza" = 95)
View(Notas)

Trabalhos <- c("João" = FALSE, "Maria" = FALSE, "José" = TRUE, "Ana" = TRUE, "Pedro" = FALSE, "Paula" = FALSE, "Carlos" = TRUE, "Mariana" = FALSE, "Fernando" = TRUE, "Luiza" = TRUE)
view(Trabalhos)

round(sqrt(mean(Notas)), 4)

round(median(Notas[Trabalhos]), 4)

round(max(Notas[!Trabalhos]), 4)

sum((Notas < 70) & (Notas >= 40) & Trabalhos)

round(sd(Notas[Trabalhos]), 4)

# Questão 2

Notas2 <- c("João" = 80, "Maria" = 85, "José" = 70, "Ana" = 31, "Pedro" = 48, "Paula" = 91, "Carlos" = 59, "Mariana" = 51, "Fernando" = 100, "Luiza" = 47)
View(Notas2)

Trabalhos2 <- c("João" = TRUE, "Maria" = TRUE, "José" = TRUE, "Ana" = FALSE, "Pedro" = FALSE, "Paula" = TRUE, "Carlos" = FALSE, "Mariana" = FALSE, "Fernando" = TRUE, "Luiza" = FALSE)
view(Trabalhos2)

round(sqrt(mean(Notas2)), 4)

round(median(Notas2[Trabalhos2]), 4)

round(max(Notas2[!Trabalhos2]), 4)

sum((Notas2 < 70) & (Notas2 >= 402) & Trabalhos2)

round(sd(Notas2[Trabalhos2]), 4)

# Questão 3

Notas3 <- c("João" = 59, "Maria" = 97, "José" = 66, "Ana" = 49, "Pedro" = 54, "Paula" = 100, "Carlos" = 76, "Mariana" = 93, "Fernando" = 64, "Luiza" = 95)
View(Notas3)

Trabalhos3 <- c("João" = FALSE, "Maria" = TRUE, "José" = FALSE, "Ana" = FALSE, "Pedro" = FALSE, "Paula" = TRUE, "Carlos" = TRUE, "Mariana" = TRUE, "Fernando" = FALSE, "Luiza" = TRUE)
view(Trabalhos3)

round(sqrt(mean(Notas3)), 4)

round(median(Notas3[Trabalhos3]), 4)

round(max(Notas3[!Trabalhos3]), 4)

sum((Notas3 < 70) & (Notas3 >= 402) & Trabalhos3)

round(sd(Notas3[Trabalhos3]), 4)

# Questão 4

MA <- matrix(data = c(19, 47, 39, 7, 16,
                      15, 12, 44, 29, 34,
                      2, 27, 1, 8, 48,
                      44, 15, 33, 27, 8,
                      45, 29, 20, 14, 44),
             nrow = 5, ncol = 5)

MB <- matrix(data = c(17, 16, 32, 45, 40, 
                      38, 31, 3, 47, 30, 
                      0, 45, 16, 0, 9, 
                      36, 3, 48, 7, 27, 
                      7, 30, 29, 3, 24),
             nrow = 5, ncol = 5)

MBT <- t(MB)
MBTB <- MBT * MB
MBTinv <- solve(MBTB)


MC <- MB * MBTinv * MBT

round(log10(abs(det(MC))), 4)

round(log10(abs(det(MA * MB))), 4)
log10(abs(det(MA * MB)))


round(sum(abs(diag(MB))), 4)

round(sum(MA[upper.tri(MA)]), 4)

PABT <- MA * MBT
IPABT <- solve(PABT)
diagINV <- diag(IPABT)

round(max(diagINV), 4)

# Questão 5

MA2 <- matrix(data = c(14, 49, 15, 3, 25, 
                       29, 39, 38, 15, 39, 
                       22, 41, 18, 25, 12, 
                       25, 18, 2, 20, 37, 
                       30, 21, 8, 39, 1),
             nrow = 5, ncol = 5)

MB2 <- matrix(data = c(17, 29, 3, 29, 0, 
                       2, 37, 3, 19, 14, 
                       12, 24, 29, 32, 48, 
                       20, 5, 26, 12, 5, 
                       23, 33, 20,41,50),
             nrow = 5, ncol = 5)

MBT2 <- t(MB2)
MBTB2 <- MBT2 * MB2
MBTinv2 <- solve(MBTB2)


MC2 <- MB2 * MBTinv2 * MBT2

round(log10(abs(det(MC2))), 4)

round(log10(abs(det(MA2 * MB2))), 4)
log10(abs(det(MA2 * MB2)))


round(sum(abs(diag(MB2))), 4)

round(sum(MA2[upper.tri(MA2)]), 4)

PABT2 <- MA2 * MBT2
IPABT2 <- solve(PABT2)
diagINV2 <- diag(IPABT2)

round(max(diagINV2), 4)

# Questão 6

MA3 <- matrix(data = c(48, 48, 3, 46, 47, 
                       26, 34, 30, 39, 35, 
                       50, 13, 17, 12, 12, 
                       35, 23, 0, 25, 28, 
                       48, 16, 41, 36, 18),
              nrow = 5, ncol = 5)

MB3 <- matrix(data = c(38, 10, 6, 11, 12, 
                       36, 21, 5, 2, 6, 
                       18, 33, 18, 7, 34, 
                       7, 47, 13, 22, 3, 
                       1, 47, 16, 41, 7),
              nrow = 5, ncol = 5)

MBT3 <- t(MB3)
MBTB3 <- MBT3 * MB3
MBTinv3 <- solve(MBTB3)


MC3 <- MB3 * MBTinv3 * MBT3

round(log10(abs(det(MC3))), 4)

round(log10(abs(det(MA3 * MB3))), 4)
log10(abs(det(MA3 * MB3)))


round(sum(abs(diag(MB3))), 4)

round(sum(MA3[upper.tri(MA3)]), 4)

PABT3 <- MA3 * MBT3
IPABT3 <- solve(PABT3)
diagINV3 <- diag(IPABT3)

round(max(diagINV3), 4)

# questão 7

View(airquality)
mean(airquality$Temp)

# a)
round(var(airquality$Temp, na.rm = TRUE), 4)

# b) 
mean_temp <- mean(airquality$Temp, na.rm = TRUE)
round(mean(abs(airquality$Temp - mean_temp), na.rm = TRUE), 4)
mad(airquality$Temp, na.rm = TRUE)

# c)
aq_Qind_8 <- subset(airquality, Month == 8)

meanWind_8 <- mean(aq_Qind_8$Wind, na.rm = TRUE)
meadianWind_8 <- median(aq_Qind_8$Wind, na.rm = TRUE)
sdWind_8 <- sd(aq_Qind_8$Wind, na.rm = TRUE)

as2_wind8 <- 3 * (meanWind_8 - meadianWind_8) / sdWind_8
round(as2_wind8, 4)

# d)
round(mean(abs(aq_Qind_8$Wind - meanWind_8), na.rm = TRUE), 4)

# Questão 8

# a)
round(var(CO2$uptake, na.rm = TRUE), 4)

# b) 
mean_ut <- mean(CO2$uptake, na.rm = TRUE)
round(mean(abs(CO2$uptake - mean_ut), na.rm = TRUE), 4)
mad(airquality$Temp, na.rm = TRUE)

# c)
View(CO2)
co2_Mis <- subset(CO2, Type == "Mississippi")

meancos_mis <- mean(co2_Mis$uptake, na.rm = TRUE)
meadianco2_mis <- median(co2_Mis$uptake, na.rm = TRUE)
sdco2_mis <- sd(co2_Mis$uptake, na.rm = TRUE)

as2_co2_mis <- 3 * (meancos_mis - meadianco2_mis) / sdco2_mis
round(as2_co2_mis, 4)

# d)
round(mean(abs(co2_Mis$uptake - meancos_mis), na.rm = TRUE), 4)

# Questão 9

# a)

round(var(airquality$Wind), 4)

# b)

meanWind <- mean(airquality$Wind, na.rm = TRUE)
round(mean(abs(airquality$Wind - meanWind)), 4)

# c) 

aq_month7 <- subset(airquality, Month == 7)
meanOzone7 <- mean(aq_month7$Ozone, na.rm = TRUE)
medianOzone7 <- median(aq_month7$Ozone, na.rm = TRUE)
sdOzonoe7 <- sd(aq_month7$Ozone, na.rm = TRUE)

as2Ozone7 <- 3 * (meanOzone7 - medianOzone7) / sdOzonoe7
round(as2Ozone7, 4)

# d) 

round(mean(abs(aq_month7$Ozone - meanOzone7), na.rm = TRUE), 4)

# Questão 10


