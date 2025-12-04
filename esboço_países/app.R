# --- 1. CONFIGURAÇÃO E CARREGAMENTO DE PACOTES ---
# Instale os pacotes necessários se ainda não os tiver:
# install.packages(c("shiny", "tidyverse", "rnaturalearth", "sf", "DT"))

library(shiny)
library(tidyverse)
library(rnaturalearth)
library(sf)
library(DT) # Para renderizar tabelas interativas

# --- 2. PREPARAÇÃO GLOBAL DOS DADOS E PADRONIZAÇÃO DE NOMES (USANDO SEUS DADOS REAIS) ---

# 1. Dados Reais de Média de Avaliação (Baseado na sua análise original)
# Usei os 10 primeiros valores da sua tabela original para garantir a classificação correta.
dados_reais_rating <- tribble(
  ~country, ~media_ratings, ~frequencia_receitas,
  "Southern Recipes", 4.734783, 50,
  "French", 4.691935, 65, # França (país de interesse)
  "Greek", 4.677193, 62,
  "Italian", 4.654386, 64,
  "Cajun and Creole", 4.613333, 63,
  "Puerto Rican", 4.608475, 60,
  "Korean", 4.603636, 56,
  "Persian", 4.602632, 45,
  "Jewish", 4.590000, 61,
  "Chinese", 4.579688, 65,
  "Russian", 4.572308, 65,
  "Colombian", 4.566667, 11,
  "Swiss", 4.566667, 10,
  "Soul Food", 4.565079, 63,
  "Canadian", 4.547761, 67,
  "Spanish", 4.545000, 61,
  "Tex-Mex", 4.542593, 55,
  "Indonesian", 4.536364, 24,
  "Belgian", 4.533333, 6
) %>%
  # Adicionando uma coluna de porções médias simuladas para o filtro funcionar
  mutate(media_servings = sample(4:12, n(), replace = TRUE))


# 2. Tabela de Conversão Manual EXPANDIDA e Adição de Regiões
tabela_conversao <- tribble(
  ~country, ~name_long,
  "Mexican", "Mexico",
  "Italian", "Italy",
  "Greek", "Greece",
  "Japanese", "Japan",
  "German", "Germany",
  "Jewish", "Israel",
  "Thai", "Thailand",
  "French", "France",
  "Indian", "India",
  "Chinese", "China",
  "British", "United Kingdom",
  "Spanish", "Spain",
  "Cuban", "Cuba",
  "Filipino", "Philippines",
  "Korean", "South Korea",
  "Brazilian", "Brazil",
  "Irish", "Ireland",
  "American", "United States",
  "Vietnamese", "Vietnam",
  "Peruvian", "Peru",
  "Russian", "Russia",
  "Turkish", "Turkey",
  "Moroccan", "Morocco",
  "Scandinavian", "Norway",
  "Australian and New Zealander", "Australia",
  "Canadian", "Canada",
  "Caribbean", "Cuba",
  "Portuguese", "Portugal",
  "Polish", "Poland",
  "Indonesian", "Indonesia",
  "Venezuelan", "Venezuela",
  "Colombian", "Colombia",
  "Argentinian", "Argentina",
  "Cajun and Creole", "United States",
  "Soul Food", "United States",
  "Amish and Mennonite", "United States",
  "Puerto Rican", "United States",
  "Tex-Mex", "United States",
  "Lebanese", "Lebanon",
  "Southern Recipes", "United States",
  "Persian", "Iran",
  "Jamaican", "Jamaica",
  "Danish", "Denmark",
  "Swedish", "Sweden",
  "Norwegian", "Norway",
  "Pakistani", "Pakistan",
  "Malaysian", "Malaysia",
  "Israeli", "Israel",
  "Austrian", "Austria",
  "Chilean", "Chile",
  "Dutch", "Netherlands",
  "South African", "South Africa",
  "Finnish", "Finland",
  "Bangladeshi", "Bangladesh",
  "Swiss", "Switzerland",
  "Belgian", "Belgium"
)

# Adiciona regiões para facilitar a filtragem
tabela_conversao <- tabela_conversao %>%
  mutate(
    Region = case_when(
      name_long %in% c("Mexico", "Cuba", "United States", "Canada", "Brazil", "Peru", "Venezuela", "Colombia", "Argentina", "Chile", "Jamaica") ~ "Américas e Caribe",
      name_long %in% c("Italy", "Greece", "Germany", "France", "United Kingdom", "Spain", "Ireland", "Portugal", "Poland", "Netherlands", "Austria", "Switzerland", "Belgium", "Norway", "Denmark", "Sweden", "Finland") ~ "Europa",
      name_long %in% c("Japan", "China", "India", "Thailand", "South Korea", "Vietnam", "Russia", "Turkey", "Lebanon", "Iran", "Israel", "Pakistan", "Malaysia", "Indonesia", "Bangladesh", "Philippines") ~ "Ásia e Oriente Médio",
      name_long %in% c("Australia") ~ "Oceania",
      name_long %in% c("Morocco", "South Africa") ~ "África",
      .default = "Outras Regiões"
    )
  )

# 3. Unir Dados Reais de Rating com Conversão Geográfica
# ALTERADO DE INNER_JOIN PARA LEFT_JOIN para manter todos os países da conversão
dados_mapeamento_base <- tabela_conversao %>% # tabela_conversao é agora a tabela principal (esquerda)
  left_join(dados_reais_rating, by = "country") %>%
  # Adicionar colunas fictícias que não foram fornecidas, mas são úteis para o dashboard
  mutate(
    # Esses valores serão NA para países sem rating, o que é esperado
    total_ratings_somados = frequencia_receitas * 10, 
    observacoes_validas = frequencia_receitas 
  )

# Carregar dados geográficos globais (world)
mapa_mundi <- ne_countries(scale = "medium", returnclass = "sf") %>%
  select(name_long, geometry)

# Definir os limites dos sliders com base nos DADOS REAIS
# Adicionar um pequeno valor mínimo de rating (3.0) para que o slider funcione
min_rating <- 3.0 # Definindo um mínimo razoável para a escala
max_rating <- ceiling(max(dados_mapeamento_base$media_ratings, na.rm = TRUE)) # Max real
max_servings <- max(dados_mapeamento_base$media_servings, na.rm = TRUE)
regioes_unicas <- sort(unique(dados_mapeamento_base$Region))

# --- 3. UI (USER INTERFACE) ---

ui <- fluidPage(
  # Tema simples com foco na legibilidade
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;600;700&display=swap');
      body {
        font-family: 'Inter', sans-serif;
        background-color: #f7f7f9;
      }
      .well {
        background-color: #ffffff;
        border-radius: 8px;
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.05);
      }
      .title {
        color: #1a1a1a;
        font-weight: 700;
        margin-bottom: 20px;
      }
      .sidebar {
        padding-top: 20px;
      }
    "))
  ),
  
  titlePanel(div(class = "title", "Dashboard Interativo de Culinária Mundial")),
  
  sidebarLayout(
    # --- Painel Lateral (Filtros) ---
    sidebarPanel(
      class = "sidebar",
      width = 3,
      h4("Filtros de Análise"),
      
      # 1. Filtro por Região
      selectInput("regiao",
                  "Filtrar por Região:",
                  choices = c("Todas" = "", regioes_unicas),
                  selected = "",
                  multiple = FALSE),
      
      hr(),
      
      # 2. Filtro por Média de Avaliação (Avg Rating)
      sliderInput("avg_rating_range",
                  "Média de Avaliação (Min-Max):",
                  min = min_rating,
                  max = max_rating,
                  value = c(min_rating, max_rating),
                  step = 0.01),
      
      hr(),
      
      # 3. Filtro por Porções (Servings) - Usando a média simulada
      sliderInput("servings_range",
                  "Porções Médias (Min-Max):",
                  min = 1,
                  max = max_servings,
                  value = c(1, max_servings),
                  step = 1),
      
      hr(),
      
      # Seletor de Visualização (Mapa ou Tabela)
      radioButtons("visualizacao", "Selecionar Visualização:",
                   choices = c("Mapa Geográfico" = "mapa",
                               "Tabela de Dados" = "tabela"),
                   selected = "mapa")
    ),
    
    # --- Painel Principal (Output) ---
    mainPanel(
      width = 9,
      # Mostrar Mapa se "mapa" estiver selecionado
      conditionalPanel(
        condition = "input.visualizacao == 'mapa'",
        plotOutput("mapa_culinario", height = "700px")
      ),
      
      # Mostrar Tabela se "tabela" estiver selecionado
      conditionalPanel(
        condition = "input.visualizacao == 'tabela'",
        h3("Tabela de Resultados Filtrados"),
        DTOutput("tabela_dados")
      )
    )
  )
)

# --- 4. SERVER (LÓGICA) ---

server <- function(input, output, session) {
  
  # 1. Dados Reativos (Filtragem)
  dados_filtrados <- reactive({
    data <- dados_mapeamento_base
    
    # Filtrar por Região
    if (input$regiao != "") {
      data <- data %>%
        filter(Region == input$regiao)
    }
    
    # Filtrar por Média de Avaliação
    # O filtro agora é feito apenas nos valores não NA
    data <- data %>%
      filter(is.na(media_ratings) | (media_ratings >= input$avg_rating_range[1] &
                                       media_ratings <= input$avg_rating_range[2]))
    
    # Filtrar por Porções Médias - USANDO O INTERVALO COMPLETO
    # O filtro agora é feito apenas nos valores não NA
    data <- data %>%
      filter(is.na(media_servings) | (media_servings >= input$servings_range[1] &
                                        media_servings <= input$servings_range[2]))
    
    return(data)
  })
  
  # 2. Dados Reativos para o Mapa
  mapa_reativo <- reactive({
    # Usamos LEFT JOIN aqui para garantir que todos os países do mapa_mundi sejam mantidos
    # A união dos dados filtrados com o mapa_mundi é sempre um LEFT JOIN
    dados_geo <- mapa_mundi %>%
      left_join(dados_filtrados(), by = "name_long")
    
    return(dados_geo)
  })
  
  
  # 3. Renderização do Mapa
  output$mapa_culinario <- renderPlot({
    mapa_dados <- mapa_reativo()
    
    # O ggplot2 renderiza apenas países com dados preenchidos no 'fill' (media_ratings)
    ggplot(data = mapa_dados) +
      # Camada de países: cor cinza para países sem dados
      geom_sf(aes(fill = media_ratings), color = "gray80", linewidth = 0.1) +
      
      # Escala Manual de Gradiente Divergente
      scale_fill_gradient2(
        low = "#ff8247",          # Vermelho/Laranja (Baixo)
        mid = "#F2F3D9",          # Ponto médio (Claro)
        high = "#8A4FFF",         # Roxo/Azul Escuro (Alto)
        midpoint = 4.6,           # Ponto de transição mantido em 4.6 (melhor que 4.5)
        limits = c(min_rating, max_rating), # Intervalo de cor fixo
        name = "Média de Avaliação",
        na.value = "gray90"       # Países sem dados
      ) +
      
      labs(
        title = paste("Média de Avaliação das Culinárias Populares: ",
                      ifelse(input$regiao == "", "Mundo", input$regiao)),
        # Subtítulo atualizado para mostrar o range de porções
        subtitle = paste0("Filtrado por Ratings (", input$avg_rating_range[1], " - ", input$avg_rating_range[2],
                          ") e Porções (", input$servings_range[1], " - ", input$servings_range[2], ")"),
        caption = "Dados baseados na análise original do usuário (ajustado para visualização)."
      ) +
      theme_void() + # Remove eixos e grade para um visual de mapa limpo
      theme(
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12, margin = margin(b = 10)),
        plot.caption = element_text(color = "gray50")
      )
  }, res = 96) # Resolução razoável
  
  # 4. Renderização da Tabela
  output$tabela_dados <- renderDT({
    # Selecionar e formatar as colunas relevantes para a tabela
    tabela_para_mostrar <- dados_filtrados() %>%
      # Filtra linhas sem dados para evitar mostrar muitas linhas NA na tabela
      filter(!is.na(media_ratings) | !is.na(media_servings)) %>%
      select(
        Região = Region,
        Culinária = country,
        País = name_long,
        Receitas = frequencia_receitas,
        Média_Avaliação = media_ratings,
        Média_Porções = media_servings
      ) %>%
      # Ordenar por Média de Avaliação decrescente
      arrange(desc(Média_Avaliação))
    
    # Renderizar a tabela interativa
    datatable(tabela_para_mostrar,
              options = list(
                pageLength = 10,
                autoWidth = TRUE,
                dom = 'tip' # Remove 'f' (search) e 'l' (length) - usando filtros Shiny
              ),
              rownames = FALSE,
              caption = paste0("Resultados filtrados (", nrow(tabela_para_mostrar), " culinárias encontradas)")) %>%
      # Formatação condicional para realçar a Média de Avaliação
      formatStyle(
        'Média_Avaliação',
        background = styleColorBar(tabela_para_mostrar$Média_Avaliação, 'lightblue'),
        backgroundSize = '95% 80%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      # Arredonda a coluna de avaliação
      formatRound('Média_Avaliação', digits = 4)
  })
}

# --- 5. EXECUÇÃO DO APP ---
shinyApp(ui = ui, server = server)