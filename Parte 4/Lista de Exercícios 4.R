## Lista de Exercícios 4

# 1)



library(dplyr)

michelin_df <- read.csv(file.choose())
glimpse(michelin_df)
head(michelin_df, 10)

R <- 6371

haversine_distance <- function(lat1, lon1, lat2, lon2) {
  rad <- function(deg) { deg * pi / 180 }
  
  phi1 <- rad(lat1)
  phi2 <- rad(lat2)
  lambda1 <- rad(lon1)
  lambda2 <- rad(lon2)
  
  d_phi <- phi2 - phi1
  d_lambda <- lambda2 - lambda1
  
  a <- sin(d_phi / 2)^2 + cos(phi1) * cos(phi2) * sin(d_lambda / 2)^2
  d <- 2 * R * asin(sqrt(a))
  
  return(d)
}



# 2. Localizar Ponto Inicial ("Citrin", Santa Monica, USA)
start <- michelin_df %>% 
  filter(Name == "Pollen", Location == "Avignon, France")

# Verificação do ponto inicial
if (nrow(start) == 0) {
  stop("Erro: Restaurante 'Pollen, Avignon, France' não encontrado no banco de dados.")
}

lat_start <- start$Latitude[1]
lon_start <- start$Longitude[1]
city_start <- start$Location[1] 

# 3. Calcular Distâncias para Todos os Restaurantes
df_with_distances <- michelin_df %>%
  rowwise() %>%
  mutate(
    dist_km = haversine_distance(lat_start, lon_start, Latitude, Longitude)
  ) %>%
  ungroup() %>%
  # Exclui o próprio restaurante
  filter(dist_km > 0.1)

# a)

proximo_2_stars <- df_with_distances %>%
  filter(grepl("2 Stars", Award)) %>% 
  summarise(min_dist = min(dist_km)) %>%
  pull(min_dist)

# Resultado:
cat("Resposta 1 Questão 1:", round(proximo_2_stars, 2), "km\n")

# b)

restaurantes_500km <- df_with_distances %>%
  filter(
    grepl("1 Star|2 Stars|3 Stars", Award), 
    dist_km <= 500
  ) %>%
  nrow()

# Resultado:
cat("Resposta 2 Questão 1:", restaurantes_500km, "\n")

# c)

restaurantes_aniversario <- df_with_distances %>%
  filter(
    grepl("1 Star|2 Stars|3 Stars", Award),
    nchar(Price) <= 4, # Filtra por até 4 dinheiros locais
    dist_km <= 3000,   # Distância máxima de 3000 km
    Location != city_start 
  ) %>%
  nrow()

# Resultado:
cat("Resposta 3 Questão 1:", restaurantes_aniversario, "\n")

# d) 

distancia_modern_cuisine <- df_with_distances %>%
  filter(Cuisine == "Modern Cuisine") %>%
  summarise(min_dist = min(dist_km)) %>%
  pull(min_dist)

# Resultado:
cat("Resposta 4 Questão 1:", round(distancia_modern_cuisine, 2), "km\n")

#2) 

tuesdata <- tidytuesdayR::tt_load(2021, week = 48)
View(tuesdata$writers)
View(tuesdata$directors)
View(tuesdata$episodes)
View(tuesdata$imdb)

dois_juntos <- inner_join(tuesdata$writers, tuesdata$directors,
                          by = c("story_number" = "story_number"))
tres_juntos <- inner_join(tuesdata$episodes, dois_juntos,
                          by = c("story_number" = "story_number"))

imdb = tuesdata$imdb %>%
  rename(season_number = season, episode_number = ep_num)

todos_juntos <- inner_join(tres_juntos, imdb,
                           by = c("season_number" = "season_number", "episode_number" = "episode_number"))

# a)

quest_a <- todos_juntos %>%
  filter(director == "Joe Ahearne" & writer == "Russell T Davies")

# b) 

quest_b <- todos_juntos %>%
  filter(writer == "Steven Moffat")

# c)

quest_c <- todos_juntos %>%
  filter(writer == "Stephen Thompson")

# d) 

quest_d <- todos_juntos %>%
  filter(writer == "Stephen Thompson")

# e) 

quest_e <- todos_juntos %>%
  filter(director == "Ben Wheatley")

media_E <- mean(quest_e$duration)
print(media_E)
