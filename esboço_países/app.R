# --- Bibliotecas Necessárias ---
require(shiny)
require(tidyverse)
require(tidytuesdayR)
require(DT) 
require(ggplot2)

# --- 1. Preparação dos Dados (Função que faz toda a limpeza e análise) ---

preparar_dados <- function() {
  
  # Carregar dados (URLs fornecidas)
  tryCatch({
    # Usando readr::read_csv para consistência
    cuisines <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-16/cuisines.csv', show_col_types = FALSE)
  }, error = function(e) {
    message("Erro ao carregar dados da web: ", e$message)
    return(NULL)
  })
  
  if (is.null(cuisines)) return(NULL)
  
  # 1. Limpeza de Culinárias e Padronização de Países
  cuisines_limpo <- cuisines %>%
    mutate(
      country_padrao = case_when(
        country %in% c("Jewish") ~ "Israeli/Jewish",
        country %in% c("Cajun and Creole", "Southern Recipes", "Tex-Mex", "Southwestern Recipes", "Amish and Mennonite", "Soul Food") ~ "US American", 
        .default = country
      )
    )
  
  # 2. Análise por País/Região (sem filtro de contagem por enquanto)
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
    arrange(desc(media_rating))
  
  # 3. Análise por Continente (Adicionando agrupamento geográfico)
  analise_por_continente <- analise_por_pais %>%
    mutate(
      continente = case_when(
        # === ASIA ===
        country_padrao %in% c("Japanese", "Indian", "Chinese", "Thai", "Filipino", "Vietnamese", "Korean", "Indonesian", "Turkish", "Malaysian", "Pakistani", "Iranian", "Israeli/Jewish", "Lebanese", "Syrian", "Yemeni", "Emirati", "Kuwaiti", "Omani", "Qatari", "Saudi Arabian", "Middle Eastern", "Persian", "Turkish", "Israeli", "Bangladeshi") ~ "Ásia",
        # === EUROPA ===
        country_padrao %in% c("British", "French", "German", "Irish", "Italian", "Spanish", "Scandinavian Region", "Greek", "Swiss", "Dutch", "Austrian", "Portuguese", "Russian", "Belgian", "Hungarian", "Polish", "Czech", "Slovakian", "Romanian", "Ukrainian", "Scandinavian", "Finnish", "Swedish", "Norwegian", "Danish") ~ "Europa",
        # === AMÉRICA DO NORTE e CARIBE ===
        country_padrao %in% c("US American", "Canadian", "Mexican", "Caribbean Region", "Puerto Rican", "Cuban", "Jamaican", "Dominican", "Haitian", "Costa Rican", "Panamanian", "Honduran", "Guatemalan", "Salvadoran") ~ "América do Norte",
        # === AMÉRICA DO SUL ===
        country_padrao %in% c("Chilean", "Brazilian", "Peruvian", "Argentinian", "Venezuelan", "Colombian", "Ecuadorian", "Bolivian", "Paraguayan", "Uruguayan") ~ "América do Sul",
        # === ÁFRICA ===
        country_padrao %in% c("Moroccan", "Egyptian", "South African", "Ethiopian", "Kenyan", "Nigerian", "Algerian", "Tunisian", "Somali", "African") ~ "África",
        # === OCEANIA ===
        country_padrao %in% c("Australian and New Zealander", "Fijian", "Samoan") ~ "Oceania",
        .default = "Outros" 
      )
    )
  
  # 4. Agrupamento Final por Continente
  analise_por_continente_agregado <- analise_por_continente %>%
    group_by(continente) %>%
    summarise(
      contagem_receitas = sum(contagem_receitas),
      media_rating = mean(media_rating, na.rm = TRUE), 
      desvio_padrao_rating = mean(desvio_padrao_rating, na.rm = TRUE), 
      media_porcoes = mean(media_porcoes, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    arrange(desc(media_rating))
  
  return(list(
    pais_completo = analise_por_continente, # Inclui o continente
    continente_agregado = analise_por_continente_agregado
  ))
}

dados_analise <- preparar_dados()


# --- 2. Interface do Usuário (UI) ---

ui <- fluidPage(
  
  # Título
  titlePanel("🍽️ Análise Global de Culinárias por Média de Avaliação"),
  
  # Layout com Sidebar para controles
  sidebarLayout(
    sidebarPanel(
      h4("Opções de Visualização"),
      p("Selecione o conjunto de dados e ajuste o filtro mínimo de receitas."),
      
      # O NOVO FILTRO DE ENTRADA
      sliderInput("min_receitas", 
                  "Filtro: Mínimo de Receitas por Culinária:", 
                  min = 1, max = 100, value = 10, step = 5),
      
      tags$hr(),
      
      # Os 3 Botões de Ação
      actionButton("btn_pais", "Top 15 Culinárias por País", icon = icon("flag")),
      tags$br(), tags$br(),
      actionButton("btn_todos_paises", "Todas Culinárias (Filtro Aplicado)", icon = icon("globe-americas")),
      tags$br(), tags$br(),
      actionButton("btn_continente", "Agregado por Continente", icon = icon("map")),
      
      tags$hr(),
      p(strong("Cores:"), "Representam o Continente.")
    ),
    
    # Painel principal para Gráfico e Tabela
    mainPanel(
      # Gráfico
      plotOutput("grafico_culinaria"),
      
      tags$hr(),
      
      # Tabela
      h3("Detalhamento dos Dados"),
      DTOutput("tabela_dados")
    )
  )
)

# --- 3. Servidor (Lógica) ---

server <- function(input, output, session) {
  
  # Variável reativa para armazenar o estado da visualização
  estado_visualizacao <- reactiveVal("pais") # Padrão: Top 15 Países
  
  # Observadores para os botões
  observeEvent(input$btn_pais, { estado_visualizacao("pais") })
  observeEvent(input$btn_todos_paises, { estado_visualizacao("todos_paises") })
  observeEvent(input$btn_continente, { estado_visualizacao("continente") })
  
  # Dados reativos, aplicando o filtro de receitas
  dados_filtrados_pais <- reactive({
    req(dados_analise)
    dados_analise$pais_completo %>%
      filter(contagem_receitas >= input$min_receitas)
  })
  
  # --- Output do GRÁFICO (Com cor por Continente) ---
  output$grafico_culinaria <- renderPlot({
    
    modo <- estado_visualizacao()
    
    if (modo == "pais") {
      # GRÁFICO: Top 15 Países/Regiões (Filtrado)
      dados_plot <- dados_filtrados_pais() %>%
        slice_max(media_rating, n = 15)
      
      ggplot(dados_plot, 
             aes(x = reorder(country_padrao, media_rating), 
                 y = media_rating, 
                 fill = continente)) + # Cor por Continente
        geom_col(alpha = 0.8) +
        geom_errorbar(aes(ymin = media_rating - desvio_padrao_rating, 
                          ymax = media_rating + desvio_padrao_rating),
                      width = 0.2, color = "black") +
        scale_fill_brewer(palette = "Set2") + # Paleta de cores para os continentes
        coord_flip() +
        labs(
          title = paste("Top 15 Culinárias por Média de Avaliação (Min. Receitas:", input$min_receitas, ")"),
          subtitle = "Barras de erro = Desvio Padrão. Cores indicam o Continente.",
          x = "País/Região",
          y = "Média de Avaliação (1 a 5)",
          fill = "Continente"
        ) +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold"))
      
    } else if (modo == "todos_paises") {
      # GRÁFICO: Todas as Culinárias (Filtrado)
      dados_plot <- dados_filtrados_pais()
      
      ggplot(dados_plot, 
             aes(x = reorder(country_padrao, media_rating), 
                 y = media_rating, 
                 color = continente)) + # Cor por Continente
        geom_point(aes(size = contagem_receitas), alpha = 0.7) +
        scale_color_brewer(palette = "Set2") +
        labs(
          title = paste("Avaliação Média de Culinárias (Min. Receitas:", input$min_receitas, ")"),
          subtitle = "Tamanho do ponto = Contagem de Receitas. Cor = Continente.",
          x = "País/Região",
          y = "Média de Avaliação (1 a 5)",
          color = "Continente",
          size = "Contagem"
        ) +
        coord_flip() +
        theme_minimal() +
        theme(legend.position = "bottom", 
              axis.text.y = element_text(size = 8),
              plot.title = element_text(face = "bold"))
      
    } else if (modo == "continente") {
      # GRÁFICO: Por Continente Agregado (Não afetado pelo filtro de receitas por país)
      dados_plot <- dados_analise$continente_agregado
      
      ggplot(dados_plot, 
             aes(x = reorder(continente, media_rating), 
                 y = media_rating, 
                 fill = continente)) + # Cor por Continente
        geom_col(alpha = 0.8) +
        geom_errorbar(aes(ymin = media_rating - desvio_padrao_rating, 
                          ymax = media_rating + desvio_padrao_rating),
                      width = 0.2, color = "black") +
        scale_fill_brewer(palette = "Set2") +
        labs(
          title = "Média de Avaliação por Continente Agregado",
          subtitle = "Média das médias dos países.",
          x = "Continente",
          y = "Média da Média de Avaliação (1 a 5)"
        ) +
        coord_flip() +
        guides(fill = "none") + # Remove a legenda de preenchimento redundante
        theme_minimal() +
        theme(plot.title = element_text(face = "bold"))
    }
  })
  
  # --- Output da TABELA (Interativa) ---
  output$tabela_dados <- renderDT({
    
    modo <- estado_visualizacao()
    
    if (modo == "pais" || modo == "todos_paises") {
      # Tabela de Países/Regiões (Filtrada)
      tabela <- dados_filtrados_pais() %>%
        select(
          `Culinária` = country_padrao,
          `Continente` = continente,
          `Receitas` = contagem_receitas,
          `Rating Médio` = media_rating,
          `Desvio Padrão` = desvio_padrao_rating,
          `Porções Médias` = media_porcoes
        )
    } else if (modo == "continente") {
      # Tabela de Continentes (Completa)
      tabela <- dados_analise$continente_agregado %>%
        select(
          `Continente` = continente,
          `Receitas Totais` = contagem_receitas,
          `Média dos Ratings` = media_rating,
          `Média Desvio Padrão` = desvio_padrao_rating,
          `Média Porções` = media_porcoes
        )
    }
    
    # Renderiza a tabela interativa
    datatable(tabela, 
              options = list(pageLength = 10, order = list(3, 'desc')), 
              rownames = FALSE, 
              caption = "Clique nos cabeçalhos para ordenar ou use a pesquisa para filtrar.") %>%
      formatRound(columns = c(4, 5, 6), digits = 2) 
  })
}

# --- 4. Executar o Aplicativo ---
shinyApp(ui = ui, server = server)