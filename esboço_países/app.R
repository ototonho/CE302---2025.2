# app.R

# Carregamento de Pacotes
required_packages <- c("shiny", "tidyverse", "DT")
lapply(required_packages, require, character.only = TRUE)

# --- 1. PREPARAÇÃO DE DADOS ---

# Leitura dos dados
cuisines <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-16/cuisines.csv')

# Limpeza e Padronização de Países
cuisines_limpo <- cuisines %>%
  mutate(
    country_padrao = case_when(
      country %in% c("Jewish") ~ "Israeli/Jewish",
      country %in% c("Cajun and Creole", "Southern Recipes", "Tex-Mex", "Southwestern Recipes", "Amish and Mennonite", "Soul Food") ~ "US American",
      .default = country
    )
  )

# Análise por País
analise_por_pais <- cuisines_limpo %>%
  filter(!is.na(avg_rating)) %>%
  group_by(country_padrao) %>%
  summarise(
    contagem_receitas = n(),
    media_rating = mean(avg_rating, na.rm = TRUE),
    desvio_padrao_rating = sd(avg_rating, na.rm = TRUE),
    media_porcoes = mean(servings, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  filter(contagem_receitas >= 10) %>%
  arrange(desc(media_rating))

# Classificação por Continente e Agregação (SEÇÃO CORRIGIDA E EXPANDIDA)
analise_por_continente <- analise_por_pais %>%
  mutate(
    continente = case_when(
      # === ASIA ===
      # Adicionando Afghan, Middle Eastern, Persian, Turkish etc., e corrigindo Asian/Jewish
      country_padrao %in% c("Japanese", "Indian", "Chinese", "Thai", "Filipino", "Vietnamese", "Korean", "Indonesian", "Turkish", "Malaysian", "Pakistani", "Iranian", "Israeli/Jewish", "Lebanese", "Syrian", "Yemeni", "Emirati", "Kuwaiti", "Omani", "Qatari", "Saudi Arabian", "Middle Eastern", "Persian", "Turkish", "Afghan", "Bangladeshi", "Israeli") ~ "Asian",
      
      # === EUROPA ===
      # Adicionando países nórdicos (Finnish, Danish, etc.) e outras regiões
      country_padrao %in% c("British", "French", "German", "Irish", "Italian", "Spanish", "Scandinavian Region", "Greek", "Swiss", "Dutch", "Austrian", "Portuguese", "Russian", "Belgian", "Hungarian", "Polish", "Czech", "Slovakian", "Romanian", "Ukrainian", "Scandinavian", "Finnish", "Swedish", "Norwegian", "Danish", "Eastern European") ~ "European",
      
      # === AMÉRICA DO NORTE, CENTRAL e CARIBE ===
      country_padrao %in% c("US American", "Canadian", "Mexican", "Caribbean Region", "Puerto Rican", "Cuban", "Jamaican", "Dominican", "Haitian", "Costa Rican", "Panamanian", "Honduran", "Guatemalan", "Salvadoran", "Native American") ~ "North American",
      
      # === AMÉRICA DO SUL ===
      country_padrao %in% c("Chilean", "Brazilian", "Peruvian", "Argentinian", "Venezuelan", "Colombian", "Ecuadorian", "Bolivian", "Paraguayan", "Uruguayan", "Latin American") ~ "South American",
      
      # === ÁFRICA ===
      country_padrao %in% c("Moroccan", "Egyptian", "South African", "Ethiopian", "Kenyan", "Nigerian", "Algerian", "Tunisian", "Somali", "African", "Maghrebi") ~ "African",
      
      # === OCEANIA ===
      # Corrigindo "Australian and New Zealander" e adicionando Oceania em geral
      country_padrao %in% c("Australian and New Zealander", "Fijian", "Samoan", "Oceanic") ~ "Oceania",
      
      # === OUTROS / GERAL ===
      country_padrao %in% c("International/Fusion", "Regional American", "Continental", "Western") ~ "Geral/Internacional",
      
      # Mantém o .default como 'Outros' para qualquer categoria que tenha ficado de fora para facilitar a inspeção
      .default = "Outros"
    )
  )

# DataFrame final para uso no Shiny
data_shiny <- analise_por_continente %>%
  select(
    `País/Região` = country_padrao,
    Continente = continente,
    `Receitas` = contagem_receitas,
    `Média Rating` = media_rating,
    `Desvio Padrão` = desvio_padrao_rating
  ) %>%
  arrange(desc(`Média Rating`))


# --- 2. INTERFACE DO USUÁRIO (UI) ---

ui <- fluidPage(
  
  # Título do Aplicativo
  titlePanel("🌍 Análise de Culinárias por País e Continente"),
  
  # Layout com Sidebar e Painel Principal
  sidebarLayout(
    
    # Painel Lateral para Filtros
    sidebarPanel(
      h4("Filtros de Dados"),
      
      selectInput(
        inputId = "filtro_continente",
        label = "Selecione o Continente:",
        choices = unique(data_shiny$Continente),
        multiple = TRUE, 
        selected = unique(data_shiny$Continente)
      ),
      
      selectInput(
        inputId = "filtro_pais",
        label = "Selecione o País/Região:",
        choices = NULL, 
        multiple = TRUE
      ),
      
      sliderInput(
        inputId = "filtro_rating",
        label = "Média de Avaliação Mínima:",
        min = floor(min(data_shiny$`Média Rating`, na.rm = TRUE)),
        max = ceiling(max(data_shiny$`Média Rating`, na.rm = TRUE)),
        value = min(data_shiny$`Média Rating`, na.rm = TRUE),
        step = 0.1
      ),
      
      actionButton(
        inputId = "limpar_selecoes",
        label = "🧹 Limpar Todos os Filtros"
      )
    ),
    
    # Painel Principal para Gráfico e Tabela
    mainPanel(
      tabsetPanel(
        tabPanel("Gráfico Estático (ggplot)", 
                 h3("Gráfico de Barras: Média de Avaliação por País"),
                 plotOutput("grafico_ratings", height = "800px") 
        ),
        tabPanel("Tabela de Dados", 
                 h3("Tabela de Análise por País/Região"),
                 DTOutput("tabela_dados") 
        )
      )
    )
  )
)

# --- 3. LÓGICA DO SERVIDOR (SERVER) ---

server <- function(input, output, session) {
  
  # Variável Reativa para o DataFrame Filtrado
  dados_filtrados <- reactive({
    data <- data_shiny
    
    # 1. Filtrar por Continente
    if (!is.null(input$filtro_continente)) {
      data <- data %>%
        filter(Continente %in% input$filtro_continente)
    }
    
    # 2. Filtrar por Média de Rating
    data <- data %>%
      filter(`Média Rating` >= input$filtro_rating)
    
    # 3. Filtrar por País/Região
    if (!is.null(input$filtro_pais)) {
      data <- data %>%
        filter(`País/Região` %in% input$filtro_pais)
    }
    
    return(data)
  })
  
  # Observador para Filtro de País
  observe({
    paises_disponiveis <- data_shiny %>%
      filter(Continente %in% input$filtro_continente) %>%
      pull(`País/Região`) %>%
      unique()
    
    selecoes_atuais <- input$filtro_pais
    selecoes_validas <- selecoes_atuais[selecoes_atuais %in% paises_disponiveis]
    
    updateSelectInput(
      session, 
      "filtro_pais", 
      choices = paises_disponiveis,
      selected = selecoes_validas
    )
  })
  
  # Observador para o Botão "Limpar Seleções"
  observeEvent(input$limpar_selecoes, {
    updateSelectInput(
      session, 
      "filtro_continente", 
      selected = unique(data_shiny$Continente)
    )
    updateSelectInput(
      session, 
      "filtro_pais", 
      selected = character(0) 
    )
    updateSliderInput(
      session, 
      "filtro_rating", 
      value = min(data_shiny$`Média Rating`, na.rm = TRUE)
    )
  })
  
  # Renderização do Gráfico (Usando renderPlot para ggplot)
  output$grafico_ratings <- renderPlot({
    req(nrow(dados_filtrados()) > 0) 
    
    # Cria o objeto ggplot com os dados filtrados
    p <- dados_filtrados() %>%
      # Filtramos para mostrar no máximo 30 para o gráfico não ficar ilegível
      slice_max(`Média Rating`, n = 30) %>% 
      
      ggplot(aes(x = reorder(`País/Região`, `Média Rating`), 
                 y = `Média Rating`, 
                 fill = Continente)) +
      
      geom_col(alpha = 0.8) +
      
      # Adiciona as Barras de Erro (Desvio Padrão)
      geom_errorbar(aes(ymin = `Média Rating` - `Desvio Padrão`, 
                        ymax = `Média Rating` + `Desvio Padrão`),
                    width = 0.2, color = "black") +
      
      # Escala de cores 
      scale_fill_brewer(palette = "Set2") +
      
      # Inverte os eixos para facilitar a leitura
      coord_flip() +
      
      # Títulos e Rótulos
      labs(
        x = "País/Região",
        y = "Média de Avaliação (1 a 5)",
        fill = "Continente"
      ) +
      
      # Tema
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold"),
        # Coloca a legenda na parte inferior
        legend.position = "bottom"
      )
    
    print(p)
  })
  
  # Renderização da Tabela
  output$tabela_dados <- renderDT({
    dados_filtrados()
  }, options = list(
    pageLength = 10,
    language = list(url = '//cdn.datatables.net/plug-ins/1.10.25/i18n/Portuguese-Brasil.json')
  ), server = FALSE)
  
}

# --- 4. EXECUÇÃO DO APLICATIVO ---

shinyApp(ui = ui, server = server)