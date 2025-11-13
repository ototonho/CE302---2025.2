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
sirmione_start <- michelin_df %>% 
  filter(Name == "La Speranzina Restaurant & Relais", Location == "Sirmione, Italy")

# Verificação do ponto inicial
if (nrow(sirmione_start) == 0) {
  stop("Erro: Restaurante 'La Speranzina Restaurant & Relais, Sirmione, Italy' não encontrado no banco de dados.")
}

lat_start <- sirmione_start$Latitude[1]
lon_start <- sirmione_start$Longitude[1]
city_start <- sirmione_start$Location[1] 

# 3. Calcular Distâncias para Todos os Restaurantes
df_with_distances <- michelin_df %>%
  rowwise() %>%
  mutate(
    dist_km = haversine_distance(lat_start, lon_start, Latitude, Longitude)
  ) %>%
  ungroup() %>%
  # Exclui o próprio restaurante
  filter(dist_km > 0.1)

df_with_distances <- calculate_all_distances(michelin_df, lat_the_kitchen, lon_the_kitchen)
View(df_with_distances)

# a)

proximo_1_star <- df_with_distances %>%
  filter(grepl("1 Star", Award)) %>% 
  summarise(min_dist = min(dist_km)) %>%
  pull(min_dist)

# Resultado:
cat("Resposta 1 Questão 1:", round(proximo_1_star, 2), "km\n")

# b)

restaurantes_1000km <- df_with_distances %>%
  filter(
    grepl("1 Star|2 Stars|3 Stars", Award), 
    dist_km <= 1000
  ) %>%
  nrow()

# Resultado:
cat("Resposta 2 Questão 1:", restaurantes_1000km, "\n")

# c)

restaurantes_aniversario <- df_with_distances %>%
  filter(
    grepl("1 Star|2 Stars|3 Stars", Award),
    nchar(Price) <= 2, # Filtra por até 2 dinheiros locais
    dist_km <= 2000,
    Location != city_start 
  ) %>%
  nrow()

# Resultado:
cat("Resposta 3 Questão 1:", restaurantes_aniversario, "\n")

# d) 

distancia_italian <- df_with_distances %>%
  filter(Cuisine == "Italian") %>%
  summarise(min_dist = min(dist_km)) %>%
  pull(min_dist)

# Resultado:
cat("Resposta 4 Questão 1:", round(distancia_italian, 2), "km\n")

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
  filter(director == "Hettie MacDonald" & writer == "Steven Moffat")

# b) 

quest_b <- todos_juntos %>%
  filter(writer == "Chris Chibnall")

# c)

quest_c <- todos_juntos %>%
  filter(writer == "Matthew Graham")

# d) 

quest_d <- todos_juntos %>%
  filter(writer == "Matthew Graham")

# e) 

quest_e <- todos_juntos %>%
  filter(director == "Daniel Nettheim")

media_E <- mean(quest_e$duration)
print(media_E)
