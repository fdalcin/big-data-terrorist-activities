server <- function(input, output) {
  
  ### Atentados por ano
  output$atentados_por_ano <- renderPlotly({
    ## pega parâmetros
    ano_min <- min(input$periodo_dashboard)
    ano_max <- max(input$periodo_dashboard)
    
    ## Gera data frame com os dados agrupados por ano
    atentados_por_ano <- dataset %>% 
      filter(ano >= ano_min & ano <= ano_max) %>%
      group_by(ano) %>% 
      summarize(atentados = n() / 1000)
    
    ## Gera o gráfico...
    gg <- ggplot(data = atentados_por_ano, 
                 aes(x = ano, y = atentados)) +
      
      # Define traçado da linha
      geom_line(color = 'gray60', 
                size = 0.5) +
      
      # Define pontos para cada ano
      geom_point(color = 'gray30', 
                 size = 1.5) +
      
      # Define escala de X
      scale_x_continuous(limits = c(ano_min, ano_max), 
                         breaks = seq(ano_min, ano_max, 10)) +
      
      # Define labels
      labs(x = 'Ano', 
           y = 'Nº ataques (em milhares)') + 
      
      # Aplica o tema
      line_theme()
    
      ggplotly(gg)
  })
  
  output$mortos_feridos_por_ano <- renderPlotly({
    ## pega parâmetros
    ano_min <- min(input$periodo_dashboard)
    ano_max <- max(input$periodo_dashboard)
    
    ## Gera data frame com os dados agrupados por ano
    atentados_por_ano <- dataset %>% 
      filter(ano >= ano_min & ano <= ano_max) %>%
      group_by(ano)
    
    mortos_por_ano <- atentados_por_ano %>% 
      summarise(nvitimas = sum(mortes_confirmadas_vitimas, na.rm = TRUE) / 1000) %>%
      mutate(situacao = 'Mortos')
    
    feridos_por_ano <- atentados_por_ano %>%
      summarise(nvitimas = sum(numero_vitimas_feridas, na.rm = TRUE) / 1000) %>%
      mutate(situacao = 'Feridos')
    
    vitimas_por_ano <- rbind.fill(mortos_por_ano, feridos_por_ano)
    
    ## Gera gráfico...
    gg <- ggplot(data = vitimas_por_ano, 
                 aes(x = ano, y = nvitimas, col = situacao)) +
      
      ## Define linhas
      geom_line(size = 0.5) +
      
      ## Define pontos
      geom_point(size = 1.5) +
      
      # Define escala de X
      scale_x_continuous(limits = c(ano_min, ano_max), 
                         breaks = seq(ano_min, ano_max, 10)) +
      
      # Define labels
      labs(x = 'Ano', 
           y = 'Nº vítimas (em milhares)', 
           color = 'Situação') + 
      
      ## Aplica o tema
      line_theme()
    
    ggplotly(gg)
  })
  
  ### Atentados por tipo
  output$atentados_por_tipo_ataque <- renderPlotly({
    ## Gera data frame com os dados agrupados por ano
    atentados_por_tipo <- dataset %>% 
      group_by(tipo_ataque) %>% 
      summarize(atentados = n() / 1000) %>%
      mutate(atentados = round(atentados, digits = 2), 
             atentados_p = round((atentados / sum(atentados)) * 100, digits = 2))
    
    ## Gera gráfico...
    gg <- ggplot(data = atentados_por_tipo, 
                 aes(x = reorder(tipo_ataque, atentados), 
                     y = atentados, 
                     text = paste(atentados, 'K'))) +
      
      ## Define barra do gráfico
      geom_bar(stat = 'identity',
               aes(fill = tipo_ataque),
               width = 0.8) +
      
      ## Rotaciona gráfico de barras para melhorar a legibilidade
      coord_flip() +
      
      ## Define labels
      labs(x = '',
           y = 'Nº ataques (em milhares)') +
      
      ## Aplica o tema
      bar_theme()
    
    ggplotly(gg, tooltip = c('text'))
  })
  
  ### Sucesso x falha por ano
  output$atentados_sucesso_falha <- renderPlotly({
    ## pega parâmetros
    ano_min <- min(input$periodo_dashboard)
    ano_max <- max(input$periodo_dashboard)
    
    ## Gera data frame com os dados agrupados por ano
    atentados_sucesso_falha <- dataset %>%
      filter(ano >= ano_min & ano <= ano_max) %>%
      group_by(ano, sucesso) %>%
      summarise(atentados = n() / 1000) %>%
      mutate(sucesso = ifelse(sucesso == 1, 'Sucesso', 'Falha'))
    
    ## Gera gráfico...
    gg <- ggplot(data = atentados_sucesso_falha, 
                 aes(x = ano, y = atentados, col = sucesso)) +
      
      ## Define linhas
      geom_line(size = 0.5) +
      
      ## Define pontos
      geom_point(size = 1.5) +
      
      # Define escala de X
      scale_x_continuous(limits = c(ano_min, ano_max), 
                         breaks = seq(ano_min, ano_max, 10)) +
      
      # Define labels
      labs(x = 'Ano', 
           y = 'Nº ataques (em milhares)', 
           color = 'Situação') + 
      
      ## Aplica o tema
      line_theme()
    
    ggplotly(gg)
  })
  
  ### Atentados por grupo
  output$grupos_mais_atuantes <- renderPlotly({
    ## pega parâmetros
    ano_min <- min(input$periodo_terrorist)
    ano_max <- max(input$periodo_terrorist)
    
    ## Gera data frame com os dados agrupados por organização
    atentados_por_grupo <- dataset %>%
      filter(organizacao_terrorista != 'Unknown' & ano >= ano_min & ano <= ano_max) %>%
      group_by(organizacao_terrorista) %>%
      summarise(atentados = n() / 1000) %>%
      mutate(atentados = round(atentados, digits = 2),
             atentados_p = round((atentados / sum(atentados)) * 100, digits = 2)) %>%
      top_n(n = 10, wt = atentados)
    
    ## Gera o gráfico
    gg <- ggplot(data = atentados_por_grupo, 
           aes(x = reorder(organizacao_terrorista, atentados), y = atentados, 
               text = paste(atentados, 'K'))) +
      
      # Define barra do gráfico
      geom_bar(stat = 'identity',
               aes(fill = organizacao_terrorista),
               width = 0.8) +
      
      # Define labels
      labs(x = '', 
           y = 'Nº atentados (em milhares)') +
      
      # Define escala do eixo Y
      scale_y_continuous(limits = c(0, 10), 
                         breaks = seq(0, 10, 2)) +
      
      # Inverte o gráfico
      coord_flip() +
      
      # Aplica o tema
      bar_theme()
      
      ggplotly(gg, tooltip = c('text'))
  })
  
  ### Atividade por grupo por ano
  output$atividades_grupos_mais_atuantes <- renderPlotly({
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
    gg <- ggplot(data = atividade_por_grupo, 
           aes(x = ano, y = atentados, col = organizacao_terrorista)) +
      
      # Define linhas no gráfico
      geom_line(size = 0.5) +
      
      geom_point(size = 1.5) +
      
      # Define labels
      labs(x = 'Ano', 
           y = 'Nº atentados (em milhares)', 
           color = 'Organização terrorista') +
      
      # Define escala de X
      scale_x_continuous(limits = c(ano_min, ano_max),
                         breaks = seq(ano_min, ano_max, 3)) +
      
      # Define escala de Y
      scale_y_continuous(limits = c(0, 1.5), 
                         breaks = seq(0, 1.5, 0.3)) +
      
      # Aplica o tema
      line_theme()
    
    ggplotly(gg)
  })
}
