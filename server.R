server <- function(input, output) {
  
  ### Atentados por ano
  output$atentados_por_ano <- renderPlot({
    ## pega parâmetros
    ano_min <- min(input$periodo_dashboard)
    ano_max <- max(input$periodo_dashboard)
    
    ## Gera data frame com os dados agrupados por ano
    atentados_por_ano <- dataset %>% 
      group_by(ano) %>% 
      summarize(atentados = n() / 1000)
    
    ## Gera o gráfico...
    ggplot(data = atentados_por_ano, aes(x = ano, y = atentados)) +
      
      # Define traçado da linha
      geom_line(color = 'gray60', size = 1) +
      
      # Define pontos para cada ano
      geom_point(color = 'gray30', size = 2.5) +
      
      # Define labels
      labs(x = 'Ano', 
           y = 'Nº ataques (em milhares)', 
           caption = 'Fonte: Global Terrorism - https://www.kaggle.com/START-UMD/gtd') + 
      
      # Define escala de X
      scale_x_continuous(limits = c(ano_min, ano_max), 
                         breaks = seq(ano_min, ano_max, 5)) +
      
      # Define escala de Y
      scale_y_continuous(limits = c(0, 20), 
                         breaks = seq(0, 20, 2)) +
      
      # Aplica o tema
      line_theme()
  })
  
  ### Vítimas por ano
  output$vitimas_por_ano <- renderPlot({
    ## pega parâmetros
    ano_min <- min(input$periodo_dashboard)
    ano_max <- max(input$periodo_dashboard)
    
    ## Gera data frame com os dados agrupados por ano
    vitimas_por_ano <- dataset %>% 
      group_by(ano) %>% 
      summarize(mortos = sum(mortes_confirmadas_vitimas, na.rm = TRUE) / 1000)
    
    ## Gera o gráfico...
    ggplot(data = vitimas_por_ano, aes(x = ano, y = mortos)) +
      
      # Define traçado da linha
      geom_line(color = 'gray60', size = 1) +
      
      # Define pontos para cada ano
      geom_point(color = 'gray30', size = 2.5) +
      
      # Define labels
      labs(x = 'Ano', 
           y = 'Nº vítimas (em milhares)', 
           caption = 'Fonte: Global Terrorism - https://www.kaggle.com/START-UMD/gtd') + 
      
      # Define escala de X
      scale_x_continuous(limits = c(ano_min, ano_max), 
                         breaks = seq(ano_min, ano_max, 5)) +
      
      # Define escala de Y
      scale_y_continuous(limits = c(0, 50), 
                         breaks = seq(0, 50, 5)) +
      
      # Aplica o tema
      line_theme()
  })
  
  ### Feridos por ano
  output$feridos_por_ano <- renderPlot({
    ## pega parâmetros
    ano_min <- min(input$periodo_dashboard)
    ano_max <- max(input$periodo_dashboard)
    
    ## Gera data frame com os dados agrupados por ano
    feridos_por_ano <- dataset %>% 
      group_by(ano) %>% 
      summarize(feridos = sum(numero_vitimas_feridas, na.rm = TRUE) / 1000)
    
    ## Gera o gráfico...
    ggplot(data = feridos_por_ano, aes(x = ano, y = feridos)) +
      
      # Define traçado da linha
      geom_line(color = 'gray60', size = 1) +
      
      # Define pontos para cada ano
      geom_point(color = 'gray30', size = 2.5) +
      
      # Define labels
      labs(x = 'Ano', 
           y = 'Nº vítimas (em milhares)', 
           caption = 'Fonte: Global Terrorism - https://www.kaggle.com/START-UMD/gtd') + 
      
      # Define escala de X
      scale_x_continuous(limits = c(ano_min, ano_max), 
                         breaks = seq(ano_min, ano_max, 5)) +
      
      # Define escala de Y
      scale_y_continuous(limits = c(0, 50), 
                         breaks = seq(0, 50, 5)) +
      
      # Aplica o tema
      line_theme()
  })
  
  ### Vítimas por tipo de atentado
  output$vitimas_por_tipo_atentado <- renderPlot({
    vitimas_por_tipo_atentado <- dataset %>%
      group_by(tipo_ataque) %>%
      summarise(mortos = sum(mortes_confirmadas_vitimas, na.rm = TRUE))
    
    ggplot(data = vitimas_por_tipo_atentado, 
           aes(x = reorder(tipo_ataque, mortos), y = mortos)) +
      geom_bar(stat = 'identity', aes(fill = tipo_ataque)) + 
      coord_flip() + 
      bar_theme()
  })
  
  ### Contabiliza atentados por tipo de ataque
  output$atentados_por_tipo_ataque <- renderPlot({
    ## Gera data frame com os dados agrupados por ano
    atentados_por_tipo <- dataset %>% 
      group_by(tipo_ataque) %>% 
      summarize(atentados = sum(n(), na.rm = TRUE) / 1000) %>%
      mutate(atentados = round(atentados, digits = 2), 
             atentados_p = round((atentados / sum(atentados)) * 100, digits = 2))
    
    ## Gera gráfico...
    ggplot(data = atentados_por_tipo, 
           aes(x = reorder(tipo_ataque, atentados), y = atentados)) +
      
      ## Define barra do gráfico
      geom_bar(stat = 'identity',
               aes(fill = tipo_ataque),
               width = 0.8) +
      
      ## Define label sobre a barra quando houver espaço
      geom_label(data = atentados_por_tipo[atentados_por_tipo$atentados_p >= 20,],
                 aes(label = paste(atentados, 'K')),
                 size = 4,
                 hjust = 1.1,
                 vjust = 0.5) +

      ## Define label ao lado da barra quando não houver espaço
      geom_label(data = atentados_por_tipo[atentados_por_tipo$atentados_p < 20,],
                 aes(label = paste(atentados, 'K')),
                 size = 4,
                 hjust = -0.2,
                 vjust = 0.5) +
      
      ## Rotaciona gráfico de barras para melhorar a legibilidade
      coord_flip() +
      
      ## Define labels
      labs(x = '',
           y = 'Nº ataques (em milhares)',
           caption = 'Fonte: Global Terrorism - https://www.kaggle.com/START-UMD/gtd') +
      
      ## Aplica o tema
      bar_theme()
  })
  
  ### Contabiliza atentados por grupo...
  output$grupos_mais_atuantes <- renderPlot({
    ## pega parâmetros
    ano_min <- min(input$periodo_terrorist)
    ano_max <- max(input$periodo_terrorist)
    
    ## Gera data frame com os dados agrupados por organização
    atentados_por_grupo <- dataset %>%
      filter(organizacao_terrorista != 'Unknown' & 
             ano >= ano_min & 
             ano <= ano_max) %>%
      group_by(organizacao_terrorista) %>%
      summarise(atentados = sum(n(), na.rm = TRUE) / 1000) %>%
      mutate(atentados = round(atentados, digits = 2),
             atentados_p = round((atentados / sum(atentados)) * 100, digits = 2)) %>%
      top_n(n = 10, wt = atentados)
    
    ## Gera o gráfico
    ggplot(data = atentados_por_grupo, 
           aes(x = reorder(organizacao_terrorista, atentados), y = atentados)) +
      
      # Define barra do gráfico
      geom_bar(stat = 'identity',
               aes(fill = organizacao_terrorista),
               width = 0.8) +
      
      ## Define texto ao lado da barra
      geom_label(aes(label = paste(atentados, 'K')),
                size = 4,
                hjust = -0.2,
                vjust = 0.5) +
      
      # Define labels
      labs(x = '', 
           y = 'Nº atentados (em milhares)', 
           caption = 'Fonte: Global Terrorism - https://www.kaggle.com/START-UMD/gtd') +
      
      # Define escala do eixo Y
      scale_y_continuous(limits = c(0, 10), 
                         breaks = seq(0, 10, 2)) +
      
      # Inverte o gráfico
      coord_flip() +
      
      # Aplica o tema
      bar_theme()
  })
  
  ### Contabiliza atividade por grupo em cada ano...
  output$atividades_grupos_mais_atuantes <- renderPlot({
    ## pega parâmetros
    ano_min <- min(input$periodo_terrorist)
    ano_max <- max(input$periodo_terrorist)
    
    ## Gera data frame com os dados agrupados por organização
    grupos <- dataset %>%
      filter(organizacao_terrorista != 'Unknown' & 
             ano >= ano_min & 
             ano <= ano_max) %>%
      group_by(organizacao_terrorista) %>%
      summarise(atentados = n() / 1000) %>%
      top_n(n = 10, wt = atentados)

    ## Gera data frame com os dados agrupados por ano e organização
    atividade_por_grupo <- dataset %>%
      filter(organizacao_terrorista %in% grupos$organizacao_terrorista) %>%
      group_by(ano, organizacao_terrorista) %>%
      summarise(atentados = n() / 1000) %>%
      mutate(atentados = round(atentados, digits = 2),
             atentados_p = round((atentados / sum(atentados)) * 100, digits = 2))
    
    ## Gera o gráfico
    ggplot(data = atividade_por_grupo, 
           aes(x = ano, y = atentados, col = organizacao_terrorista)) +
      
      # Define linhas no gráfico
      geom_line(size = 1) +
      
      geom_point(size = 2.5) +
      
      # Define labels
      labs(x = 'Ano', 
           y = 'Nº atentados (em milhares)', 
           color = 'Organização terrorista', 
           caption = 'Fonte: Global Terrorism - https://www.kaggle.com/START-UMD/gtd') +
      
      # Define escala de X
      scale_x_continuous(limits = c(ano_min, ano_max),
                         breaks = seq(ano_min, ano_max, 3)) +
      
      # Define escala de Y
      scale_y_continuous(limits = c(0, 1.5), 
                         breaks = seq(0, 1.5, 0.3)) +
      
      # Aplica o tema
      line_theme()
  })
}
