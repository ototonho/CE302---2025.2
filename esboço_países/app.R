# Certifique-se de ter as bibliotecas instaladas:
# install.packages(c("shiny", "ggplot2", "dplyr", "DT", "readr", "tidyr", "leaflet", "rnaturalearth"))

library(shiny)
library(ggplot2)
library(dplyr)
library(DT) 
library(readr) 
library(tidyr) 
library(leaflet) # Para mapas interativos
library(rnaturalearth) # Para obter dados geográficos dos países

## --- 1. Carregamento e Preparação de Dados Geográficos ---

# Carregando os datasets reais
all_recipes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-16/all_recipes.csv', show_col_types = FALSE)
cuisines <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-16/cuisines.csv', show_col_types = FALSE)

# Mapeamento de Cozinha/País para Continente
continent_lookup <- tribble(
  ~country, ~continent,
  "American", "North America",
  "Mexican", "North America",
  "Canadian", "North America",
  "South American", "South America",
  "Brazilian", "South America",
  "Italian", "Europe",
  "French", "Europe",
  "German", "Europe",
  "Greek", "Europe",
  "British", "Europe",
  "Spanish", "Europe",
  "Japanese", "Asia",
  "Chinese", "Asia",
  "Indian", "Asia",
  "Thai", "Asia",
  "Middle Eastern", "Asia",
  "Korean", "Asia",
  "African", "Africa",
  "Australian and New Zealander", "Oceania",
  "Russian", "Europe"
)

# Dados geográficos (mapa mundial)
world_map <- ne_countries(scale = "medium", returnclass = "sf") %>%
  # Simplificação dos nomes dos continentes para corresponder ao lookup
  mutate(continent_map = case_when(
    continent == "North America" ~ "North America",
    continent == "South America" ~ "South America",
    continent == "Europe" ~ "Europe",
    continent == "Asia" ~ "Asia",
    continent == "Africa" ~ "Africa",
    continent == "Oceania" ~ "Oceania",
    TRUE ~ "Outros/Diversas" # Se não mapeado, cai em Outros
  ))

# Unindo os datasets e preparando os filtros (mantido do código anterior)
data_merged <- inner_join(
  cuisines %>% select(name, country),
  all_recipes,
  by = "name",
  relationship = "many-to-many"
) %>%
  left_join(continent_lookup, by = "country") %>%
  mutate(continent = replace_na(continent, "Outros/Diversas")) %>% 
  
  rename(avg_ranking = avg_rating) %>%
  filter(!is.na(country) & !is.na(avg_ranking) & !is.na(servings)) %>%
  
  mutate(
    avg_ranking_cat = case_when(
      avg_ranking >= 4.5 ~ "Ranking Alto (>= 4.5)",
      avg_ranking >= 4.0 ~ "Ranking Médio (4.0 - 4.4)",
      TRUE ~ "Ranking Baixo (< 4.0)"
    )
  )

# Cálculo da média de servings
media_servings <- mean(data_merged$servings, na.rm = TRUE)

# Variáveis disponíveis para o usuário
choices_continente <- unique(data_merged$continent)
choices_cozinha <- data_merged %>% count(country, sort = TRUE) %>% top_n(50, n) %>% pull(country)
choices_ranking <- unique(data_merged$avg_ranking_cat)
choices_servings <- c("Abaixo da Média", "Acima da Média")

## --- 2. Interface do Usuário (UI) ---
ui <- fluidPage(
  
  titlePanel("🌎 Análise de Receitas por Região e Cozinha (Mapa)"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h3("⚙️ Opções de Filtro"),
      
      actionButton("reset_all_filters", "Limpar Todos os Filtros", icon = icon("undo"), class = "btn-warning"),
      
      hr(),
      
      # FILTRO 1: Seleção por Região (Continente)
      selectInput(
        inputId = "filtro_continente",
        label = "1. Selecione a Região (Continente):",
        choices = c("Todas" = "", choices_continente),
        selected = ""
      ),
      
      hr(),
      
      # FILTRO 2: Seleção por Cozinha/País
      selectInput(
        inputId = "filtro_cozinha", 
        label = "2. Selecione a Cozinha/País:",
        choices = choices_cozinha,
        selected = character(0),
        multiple = TRUE
      ),
      
      hr(),
      
      # FILTRO 3: Seleção por Categoria de Ranking
      selectInput(
        inputId = "filtro_ranking",
        label = "3. Selecione a Categoria de Ranking Médio:",
        choices = c("Todas" = "", choices_ranking),
        selected = ""
      ),
      
      hr(),
      
      # FILTRO 4: Seleção por Porções (Servings)
      selectInput(
        inputId = "filtro_servings",
        label = paste0("4. Servings (Média Geral: ", round(media_servings, 1), "):"),
        choices = c("Todas" = "", choices_servings),
        selected = ""
      )
      
      # O radioButtons foi removido, pois o gráfico é fixo em mapa de média de Servings por Continente
    ),
    
    # Painel Principal para o MAPA e Tabela
    mainPanel(
      
      h3("🗺️ Média de Servings por Região (Continente)"),
      p(paste0("Total de Receitas no Dataset Base Filtrado: ", nrow(data_merged))),
      
      # NOVO OUTPUT: Mapa Interativo
      leafletOutput("mapa_visualizacao", height = 500),
      
      hr(),
      
      h3("📋 Tabela de Dados Filtrados"),
      DTOutput("tabela_dados")
    )
  )
)

## --- 3. Servidor (Lógica de Filtro e Renderização) ---
server <- function(input, output, session) {
  
  # Lógica para o Botão de Reset
  observeEvent(input$reset_all_filters, {
    updateSelectInput(session, "filtro_continente", selected = "")
    updateSelectInput(session, "filtro_cozinha", selected = character(0))
    updateSelectInput(session, "filtro_ranking", selected = "")
    updateSelectInput(session, "filtro_servings", selected = "")
  })
  
  # Lógica de Filtro Reativa
  dados_filtrados <- reactive({
    
    data_filtered <- data_merged
    
    # 1. Filtrar por Região (Continente)
    if (input$filtro_continente != "") {
      data_filtered <- data_filtered %>%
        filter(continent == input$filtro_continente)
    }
    
    # 2. Filtrar por Cozinha (Country)
    if (!is.null(input$filtro_cozinha) && length(input$filtro_cozinha) > 0) {
      data_filtered <- data_filtered %>%
        filter(country %in% input$filtro_cozinha)
    }
    
    # 3. Filtrar por Categoria de Ranking (avg_ranking_cat)
    if (input$filtro_ranking != "") {
      data_filtered <- data_filtered %>%
        filter(avg_ranking_cat == input$filtro_ranking)
    }
    
    # 4. Filtrar por Porções (Servings)
    if (input$filtro_servings != "") {
      if (input$filtro_servings == "Acima da Média") {
        data_filtered <- data_filtered %>%
          filter(servings > media_servings)
      } else { # "Abaixo da Média"
        data_filtered <- data_filtered %>%
          filter(servings <= media_servings)
      }
    }
    
    # Retorna o dataset filtrado
    data_filtered
  })
  
  # Preparação dos dados para o Mapa (Agrupamento por Continente)
  dados_mapa <- reactive({
    req(nrow(dados_filtrados()) > 0)
    
    # 1. Agrupar dados filtrados por Continente e calcular a média de servings
    servings_by_continent <- dados_filtrados() %>%
      group_by(continent) %>%
      summarise(
        media_servings = mean(servings, na.rm = TRUE),
        n_recipes = n(),
        .groups = 'drop'
      )
    
    # 2. Unir os dados de servings com os dados geográficos
    world_map %>%
      left_join(servings_by_continent, by = c("continent_map" = "continent")) %>%
      # Substitui NA por 0 em média_servings se não houver receitas para aquele continente no filtro
      mutate(media_servings = replace_na(media_servings, 0))
  })
  
  # Renderizar o MAPA Reativo
  output$mapa_visualizacao <- renderLeaflet({
    
    data_map <- dados_mapa()
    
    # Definir a paleta de cores para o mapa (baseada na média de servings)
    pal <- colorNumeric(
      palette = "YlOrRd", 
      domain = data_map$media_servings
    )
    
    # Rótulos (popups) para o mapa
    labels <- paste0(
      "<strong>Região:</strong> ", data_map$continent_map, "<br/>",
      "<strong>Média de Servings:</strong> ", round(data_map$media_servings, 2), "<br/>",
      "<strong>Total de Receitas (Filtradas):</strong> ", data_map$n_recipes
    ) %>% lapply(htmltools::HTML)
    
    # Criação do mapa Leaflet
    leaflet(data_map) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(media_servings),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 3,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(pal = pal, values = ~media_servings, opacity = 0.7, title = "Média de Servings", position = "bottomright")
  })
  
  # Renderizar a Tabela Reativa (mantido do código anterior)
  output$tabela_dados <- renderDT({
    
    dados_tabela <- dados_filtrados() %>%
      select(name, continent, country, avg_ranking, total_ratings, servings, prep_time, cook_time)
    
    datatable(dados_tabela,
              options = list(pageLength = 10, scrollX = TRUE),
              colnames = c("Nome da Receita", "Região", "Cozinha", "Ranking Médio", "Total Avaliações", "Porções", "Tempo Prep (min)", "Tempo Cook (min)"),
              caption = paste("Exibindo", nrow(dados_filtrados()), "Receitas")
    )
  })
}

## --- 4. Executar o Aplicativo Shiny ---
shinyApp(ui = ui, server = server)