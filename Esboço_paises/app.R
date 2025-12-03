library(shiny)
library(tidyverse)
library(DT) # Para a tabela interativa

# --- Carregamento e Preparação Inicial ---
# (Assumindo que 'cuisines' já foi carregado)

# 1. Tabela de Métricas Resumo por País
tabela_pais_calorias <- cuisines %>%
  group_by(country) %>%
  summarise(
    frequencia_receitas = n(),
    media_ratings = mean(avg_rating, na.rm = TRUE),
    media_calories = mean(calories, na.rm = TRUE),
    media_fat = mean(fat, na.rm = TRUE),
    media_carbs = mean(carbs, na.rm = TRUE),
    media_protein = mean(protein, na.rm = TRUE)
  )

# 2. Tabela de Proporções de Macronutrientes (para o gráfico)
proporcoes_nutricionais <- tabela_pais_calorias %>%
  filter(media_calories > 100) %>%
  mutate(
    calorias_fat = media_fat * 9,
    calorias_carbs = media_carbs * 4,
    calorias_protein = media_protein * 4,
    total_macro_calories = calorias_fat + calorias_carbs + calorias_protein,
    perc_fat = (calorias_fat / total_macro_calories) * 100,
    perc_carbs = (calorias_carbs / total_macro_calories) * 100,
    perc_protein = (calorias_protein / total_macro_calories) * 100
  ) %>%
  select(country, media_ratings, frequencia_receitas, starts_with("perc_"))

# 3. Pivotar os dados para o formato longo (Gráfico de Barras Empilhadas)
proporcoes_longo <- proporcoes_nutricionais %>%
  pivot_longer(
    cols = starts_with("perc_"),
    names_to = "macronutriente",
    values_to = "percentual"
  )

# Dentro do ui <- fluidPage(...
ui <- fluidPage(
  tabsetPanel(
    tabPanel("Análise de Culinárias", 
             h2("🌎 Perfil Nutricional e Desempenho por País"),
             
             # --- Layout da Barra Lateral (Inputs Dinâmicos) ---
             sidebarLayout(
               sidebarPanel(
                 h3("Filtros de Análise"),
                 
                 # Elemento Dinâmico: Slider para Filtrar a Frequência
                 sliderInput("freq_minima", "Frequência Mínima de Receitas:",
                             min = 1, max = max(tabela_pais_calorias$frequencia_receitas),
                             value = 20, step = 5),
                 
                 # 2. NOVO Filtro de Avaliação Mínima
                 sliderInput("rating_minimo", "Avaliação Mínima (Avg. Rating):",
                             min = 1, max = 5, # Ratings vão de 1 a 5
                             value = 4.0, step = 0.1),
                 
                 hr(), # Separador visual
                 
                 # 3. NOVO: Critério de Ordenação Dinâmica
                 radioButtons("criterio_ordem", "Ordenar Gráfico por:",
                              choices = c(
                                "Média de Gordura (%)" = "perc_fat",
                                "Média de Proteína (%)" = "perc_protein",
                                "Média de Avaliação" = "media_ratings"
                              ),
                              selected = "perc_fat"),
                 
                 helpText("Use o slider para incluir apenas países com um número representativo de receitas.")
               ),
               
               # --- Painel Principal (Gráfico e Tabela) ---
               mainPanel(
                 tabsetPanel(
                   # Sub-aba para o Gráfico
                   tabPanel("Proporção de Macronutrientes", 
                            plotOutput("plot_proporcoes_dinamico")),
                   
                   # Sub-aba para a Tabela Interativa
                   tabPanel("Tabela de Métricas Detalhadas", 
                            DTOutput("tabela_paises_dt"))
                 )
               )
             )
    ),
    # Outras 5 abas aqui...
    tabPanel("Outra Aba 1", "..."),
    tabPanel("Outra Aba 2", "..."),
    tabPanel("Outra Aba 3", "..."),
    tabPanel("Outra Aba 4", "..."),
    tabPanel("Outra Aba 5", "...")
  )
)

# Dentro do server <- function(input, output) {...
server <- function(input, output) {
  
  # 1. Objeto Reativo Principal (Mudamos o nome para CLAREZA)
  dados_grafico_reativo <- reactive({
    
    # 1a. Juntar o rating (isso precisa ser feito dentro do reactive, pois o filtro de rating está sendo aplicado)
    proporcoes_com_rating <- proporcoes_longo %>%
      left_join(tabela_pais_calorias %>% 
                  select(country, media_ratings), 
                by = "country")
    
    # 1b. Aplicar os Filtros de Frequência e Avaliação
    dados_base_filtrados <- proporcoes_com_rating %>%
      filter(
        frequencia_receitas >= input$freq_minima,
        media_ratings >= input$rating_minimo
      )
    
    # 2. Calcular o VALOR DE ORDENAÇÃO ÚNICO por País
    tabela_para_ordenar <- dados_base_filtrados %>%
      # Filtramos para obter o valor que o usuário escolheu
      # Usamos uma condição OR para incluir a métrica media_ratings que não tem 'macronutriente'
      filter(macronutriente == input$criterio_ordem | input$criterio_ordem == "media_ratings") %>%
      mutate(
        # Se for media_ratings, usa media_ratings; se for macro, usa percentual.
        valor_ordem = if_else(input$criterio_ordem == "media_ratings", media_ratings, percentual)
      ) %>%
      group_by(country) %>%
      # max() garante que o valor seja único para cada país
      summarise(valor_final_ordem = max(valor_ordem, na.rm = TRUE)) 
    
    # 3. Juntar a ordem final de volta nos dados longos (com todos os 3 macros)
    dados_finais_ordenados <- dados_base_filtrados %>%
      left_join(tabela_para_ordenar, by = "country") %>%
      # 4. Reordenar o País no ggplot
      mutate(country = reorder(country, valor_final_ordem))
    
    return(dados_finais_ordenados)
  })
  
  # 2. Gráfico de Proporção de Macronutrientes (Agora funcionando)
  output$plot_proporcoes_dinamico <- renderPlot({
    # CHAMANDO A FUNÇÃO REATIVA CORRETA: dados_grafico_reativo()
    dados_grafico_reativo() %>%
      ggplot(aes(x = country, y = percentual, fill = macronutriente)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(
        title = paste("Composição Calórica Média por País (Ordenado por:", input$criterio_ordem, ")"),
        x = "País / Culinária",
        y = "Percentual da Média Calórica Total",
        fill = "Macronutriente"
      ) +
      scale_fill_manual(values = c("perc_carbs" = "#4e79a7", "perc_fat" = "#f28e2b", "perc_protein" = "#e15759"),
                        labels = c("Carboidratos", "Gordura", "Proteína")) +
      theme_minimal()
  })
  
  # 3. Tabela Detalhada Interativa (DT) - A tabela é mais simples e não precisa da lógica complexa de ordenação
  output$tabela_paises_dt <- renderDT({
    
    tabela_pais_calorias %>%
      filter(
        frequencia_receitas >= input$freq_minima,
        media_ratings >= input$rating_minimo
      ) %>%
      select(
        País = country,
        Frequência = frequencia_receitas,
        Média_Ratings = media_ratings,
        Média_Calorias = media_calories,
        Média_Gordura = media_fat,
        Média_Proteína = media_protein
      ) %>%
      mutate(across(starts_with("Média_"), ~ round(., 2))) %>%
      datatable(options = list(pageLength = 15, dom = 'tip'), 
                rownames = FALSE)
  })
}

# Cria o objeto Shiny App
shinyApp(ui = ui, server = server)