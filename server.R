server <- function(input, output) {
  
  ### Contabiliza atentados por ano...
  output$atentados_por_ano <- renderPlot({
    ## pega parâmetros
    ano_min <- min(input$periodo_dashboard)
    ano_max <- max(input$periodo_dashboard)
    
    ## Gera data frame com os dados agrupados por ano
    atentados_por_ano <- dataset %>% 
      group_by(ano) %>% 
      summarize(atentados = sum(n(), na.rm = TRUE) / 1000)
    
    ## Gera o gráfico...
    ggplot(data = atentados_por_ano, aes(x = ano, y = atentados)) +
      
      # Define traçado da linha
      geom_line(size = 1, colour = 'gray20') +
      
      # Define pontos para cada ano
      geom_point(pch = 21, 
                 colour = "gray20",
                 fill = "white",
                 size = 3) +
      
      # Define labels
      labs(x = 'Ano', 
           y = 'Número de ataques (em milhares)', 
           caption = 'Fonte: Global Terrorism - https://www.kaggle.com/START-UMD/gtd') + 
      
      # Define escala de X
      scale_x_continuous(limits = c(ano_min, ano_max), 
                         breaks = seq(ano_min, ano_max, 3)) +
      
      # Define escala de Y
      scale_y_continuous(limits = c(0, 20), 
                         breaks = seq(0, 20, 2)) +
      
      # Aplica o tema
      line_theme()
  })
  
  ### Contabiliza atentados por tipo de ataque
  output$atentados_por_tipo_ataque <- renderPlot({
    ## Gera data frame com os dados agrupados por ano
    atentados_por_tipo <- dataset %>% 
      group_by(id_tipoataque, tipo_ataque) %>% 
      summarize(atentados = sum(n(), na.rm = TRUE) / 1000)
    
    ## Ordena por número de atentados
    atentados_por_tipo <- atentados_por_tipo[order(atentados_por_tipo$atentados),]
    
    ## Transforma tipo de ataque em fator
    atentados_por_tipo$tipo_ataque <- factor(atentados_por_tipo$tipo_ataque,
                                                         levels = atentados_por_tipo$tipo_ataque)

    ## Gera gráfico...
    ggplot(data = atentados_por_tipo, aes(x = tipo_ataque, y = atentados, label = atentados)) +
      
      ## Define barra do gráfico
      geom_bar(stat = 'identity', aes(fill = tipo_ataque), width = 0.8) +
      
      ## Define a coloração das barras manualmente
      scale_fill_manual(values = c('#5FA7F4',
                                   '#5594D8',
                                   '#4A82BE',
                                   '#4170A6',
                                   '#38608E',
                                   '#2F5178',
                                   '#264162',
                                   '#1F344E',
                                   '#18273C')) +
      
      ## Define label sobre a barra quando houver espaço
      geom_label(data = atentados_por_tipo[atentados_por_tipo$atentados >= 20,],
                 aes(label = paste(atentados, 'K')),
                 size = 4,
                 hjust = 1.1,
                 vjust = 0.5) +
      
      ## Define label ao lado da barra quando não houver espaço
      geom_label(data = atentados_por_tipo[atentados_por_tipo$atentados < 20,],
                 aes(label = paste(atentados, 'K')),
                 size = 4,
                 hjust = -0.2,
                 vjust = 0.5) +
      
      ## Rotaciona gráfico de barras para melhorar a legibilidade
      coord_flip() +
      
      ## Define labels
      labs(x = 'Tipo de ataque',
           y = 'Número de ataques (em milhares)',
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
      filter(atentados >= 1)
      
    ## Ordena os grupos pelo número de atentados
    atentados_por_grupo <- atentados_por_grupo[order(atentados_por_grupo$atentados),]
    
    ## Transforma a organização em fator
    atentados_por_grupo$organizacao_terrorista <- factor(atentados_por_grupo$organizacao_terrorista,
                                                         levels = atentados_por_grupo$organizacao_terrorista)
    
    ## Gera o gráfico
    ggplot(data = atentados_por_grupo, 
           aes(x = organizacao_terrorista, y = atentados, fill = organizacao_terrorista)) +
      
      # Define barra do gráfico
      geom_bar(stat = 'identity', 
               width = 0.8) +
      
      ## Define texto ao lado da barra
      geom_text(aes(label = paste(atentados, 'K')),
                size = 4,
                hjust = -0.2,
                vjust = 0.5,
                fontface = 'bold') +
      
      # Define labels
      labs(x = '', 
           y = 'Número de atentados (em milhares)', 
           caption = 'Fonte: Global Terrorism - https://www.kaggle.com/START-UMD/gtd') +
      
      # Define escala do eixo Y
      scale_y_continuous(limits = c(0, 10), 
                         breaks = seq(0, 10, 1)) +
      
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
    atentados_por_grupo <- dataset %>%
      filter(organizacao_terrorista != 'Unknown' & 
             ano >= ano_min & 
             ano <= ano_max) %>%
      group_by(organizacao_terrorista) %>%
      summarise(atentados = sum(n(), na.rm = TRUE) / 1000) %>%
      filter(atentados >= 1)

    ## Gera data frame com os dados agrupados por ano e organização
    atividade_por_grupo <- dataset %>%
      filter(organizacao_terrorista %in% atentados_por_grupo$organizacao_terrorista) %>%
      group_by(ano, organizacao_terrorista) %>%
      summarise(atentados = sum(n(), na.rm = TRUE) / 1000)
    
    ## Gera o gráfico
    ggplot(data = atividade_por_grupo, 
           aes(x = ano, y = atentados, col = organizacao_terrorista)) +
      
      # Define linhas no gráfico
      geom_line(size = 0.8) +
      
      # Define labels
      labs(x = 'Ano', 
           y = 'Número de atentados (em milhares)', 
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
