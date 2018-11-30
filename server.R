server <- function(input, output) {
  ### Correlação de variáveis
  output$correlacao <- renderPlot({
    data <- dataset %>% select(ano, mes, dia, duracao_maior_24h, id_pais, 
                               id_regiao, multiplos_alvos, sucesso, ataque_suicida, 
                               id_tipoataque, id_tipoalvo, id_subtipoalvo, id_tipoarma, 
                               mortes_confirmadas_vitimas, mortes_terroristas, 
                               numero_vitimas_feridas, numero_terroristas_feridos)
    
    names(data) <- c('Ano', 'Mes', 'Dia', 'Mais 24h', 'Pais', 
                     'Regiao', 'Multiplos Alvos', 'Sucesso', 'Ataque Suicida', 
                     'Tipo Ataque', 'Tipo Alvo', 'Subtipo Alvo', 'Tipo Arma',
                     'Vitimas Mortas', 'Terroristas Mortos', 'Vitimas Feridas', 'Terroristas Feridos')
    
    correlacao <- round(cor(data), 1)
    
    gg <- ggcorrplot(correlacao,
                     type = 'lower',
                     lab = TRUE,
                     lab_size = 3,
                     method = "circle",
                     colors = c('#ff0000', '#ffffff', '#007f00'),
                     title = 'Correlaciograma',
                     insig = 'blank',
                     ggtheme = default_theme)
    
    gg
  })
  
  ### Atentados por ano
  output$atentados_por_ano <- renderPlotly({
    ## pega parâmetros
    ano_min <- min(input$interval)
    ano_max <- max(input$interval)
    
    ## Gera data frame com os dados agrupados por ano
    atentados_por_ano <- dataset %>% 
      filter( ano >= ano_min & 
               ano <= ano_max) %>%
      group_by(ano) %>% 
      summarize(atentados = n())
    
    ## Gera o gráfico...
    gg <- ggplot(data = atentados_por_ano, 
                 aes(x = ano, y = atentados)) +
      
      # Define traçado da linha
      geom_line(size = 0.5, color = 'gray60') +
      
      # Define pontos para cada ano
      geom_point(size = 1.5, color = 'gray30') +
      
      # Define escala de X
      scale_x_continuous(limits = c(ano_min, ano_max), 
                         breaks = seq(ano_min, ano_max, 10)) +
      
      # Define labels
      labs(x = 'Ano', 
           y = 'Eventos') + 
      
      # Aplica o tema
      default_theme()
    
    ggplotly(gg)
  })
  
  ### Atentados por pais
  output$atentados_por_pais <- renderPlotly({
    ## pega parâmetros
    ano_min <- min(input$interval)
    ano_max <- max(input$interval)
    countries <- input$countries
    
    ## Gera data frame com os dados agrupados por ano
    atentados_por_pais <- dataset %>% 
      filter(pais %in% countries & 
               ano >= ano_min & 
               ano <= ano_max) %>%
      group_by(ano, pais) %>% 
      summarize(atentados = n())
    
    ## Gera o gráfico...
    gg <- ggplot(data = atentados_por_pais, 
                 aes(x = ano, y = atentados, col = pais)) +
      
      # Define traçado da linha
      geom_line(size = 0.5) +
      
      # Define pontos para cada ano
      geom_point(size = 1.5) +
      
      # Define escala de X
      scale_x_continuous(limits = c(ano_min, ano_max), 
                         breaks = seq(ano_min, ano_max, 10)) +
      
      # Define labels
      labs(x = 'Ano', 
           y = 'Eventos',
           color = 'Países') + 
      
      # Aplica o tema
      default_theme()
    
    ggplotly(gg)
  })
  
  ### Mortos e feridos por ano
  output$mortos_feridos_por_ano <- renderPlotly({
    ## pega parâmetros
    ano_min <- min(input$interval)
    ano_max <- max(input$interval)
    countries <- input$countries
    
    if (is.null(countries)) {
      atentados_por_ano <- dataset %>% 
        filter(ano >= ano_min & ano <= ano_max)
    } else {
      atentados_por_ano <- dataset %>% 
        filter(pais %in% countries & 
                 ano >= ano_min & 
                 ano <= ano_max)
    }
    
    ## Gera data frame com os dados agrupados por ano
    atentados_por_ano <- atentados_por_ano %>% group_by(ano)
    
    ## Gera data frame de mortes nos atentados
    mortos_por_ano <- atentados_por_ano %>% 
      summarise(nvitimas = sum(mortes_confirmadas_vitimas, na.rm = TRUE)) %>%
      mutate(situacao = 'Mortos')
    
    ## Gera data frame de feridos nos atentados
    feridos_por_ano <- atentados_por_ano %>%
      summarise(nvitimas = sum(numero_vitimas_feridas, na.rm = TRUE)) %>%
      mutate(situacao = 'Feridos')
    
    ## Combina os 2 data frames em um novo
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
           y = 'Vítimas', 
           color = 'Situação') + 
      
      ## Aplica o tema
      default_theme()
    
    ggplotly(gg)
  })
  
  ### Sucesso x falha por ano
  output$atentados_sucesso_falha <- renderPlotly({
    ## pega parâmetros
    ano_min <- min(input$interval)
    ano_max <- max(input$interval)
    countries <- input$countries
    
    if (is.null(countries)) {
      atentados_sucesso_falha <- dataset %>% 
        filter(ano >= ano_min & ano <= ano_max)
    } else {
      atentados_sucesso_falha <- dataset %>% 
        filter(pais %in% countries & 
                 ano >= ano_min & 
                 ano <= ano_max)
    }
    
    ## Gera data frame com os dados agrupados por ano
    atentados_sucesso_falha <- atentados_sucesso_falha %>%
      group_by(ano, sucesso) %>%
      summarise(atentados = n()) %>%
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
           y = 'Eventos', 
           color = 'Efetividade') + 
      
      ## Aplica o tema
      default_theme()
    
    ggplotly(gg)
  })
  
  ### Atentados por tipo
  output$atentados_por_tipo_ataque <- renderPlotly({
    ## pega parâmetros
    ano_min <- min(input$interval2)
    ano_max <- max(input$interval2)
    countries <- input$countries2
    
    if (is.null(countries)) {
      atentados_por_tipo <- dataset %>% 
        filter(ano >= ano_min & ano <= ano_max)
    } else {
      atentados_por_tipo <- dataset %>% 
        filter(pais %in% countries & 
                 ano >= ano_min & 
                 ano <= ano_max)
    }
    
    ## Gera data frame com os dados agrupados por ano
    atentados_por_tipo <- atentados_por_tipo %>%
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
           y = 'Eventos') +
      
      ## Aplica o tema
      no_arrow_theme()
    
    ggplotly(gg, tooltip = c('text'))
  })
  
  ### Atentados por grupo
  output$grupos_mais_atuantes <- renderPlotly({
    ## pega parâmetros
    ano_min <- min(input$interval3)
    ano_max <- max(input$interval3)
    countries <- input$countries3
    
    if (is.null(countries)) {
      atentados_por_grupo <- dataset %>% filter(organizacao_terrorista != 'Unknown' & 
                                                  ano >= ano_min & 
                                                  ano <= ano_max)
    } else {
      atentados_por_grupo <- dataset %>% filter(pais %in% countries &
                                                  organizacao_terrorista != 'Unknown' & 
                                                  ano >= ano_min & 
                                                  ano <= ano_max)
    }
    
    ## Gera data frame com os dados agrupados por organiza??o
    atentados_por_grupo <- atentados_por_grupo %>%
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
           y = 'Eventos') +
      
      # Define escala do eixo Y
      scale_y_continuous(limits = c(0, 10), 
                         breaks = seq(0, 10, 2)) +
      
      # Inverte o gráfico
      coord_flip() +
      
      # Aplica o tema
      no_arrow_theme()
    
    ggplotly(gg, tooltip = c('text'))
  })
  
  ### Atividade por grupo por ano
  output$atividades_grupos_mais_atuantes <- renderPlotly({
    ## pega parâmetros
    ano_min <- min(input$interval3)
    ano_max <- max(input$interval3)
    countries <- input$countries3
    
    if (is.null(countries)) {
      grupos <- dataset %>% 
        filter(organizacao_terrorista != 'Unknown' & 
                 ano >= ano_min & 
                 ano <= ano_max)
    } else {
      grupos <- dataset %>% 
        filter(pais %in% countries &
                 organizacao_terrorista != 'Unknown' & 
                 ano >= ano_min & 
                 ano <= ano_max)
    }
    
    ## Gera data frame com os dados agrupados por organização
    grupos <- grupos %>%
      group_by(organizacao_terrorista) %>%
      summarise(atentados = n()) %>%
      top_n(n = 10, wt = atentados)
    
    ## Gera data frame com os dados agrupados por ano e organização
    atividade_por_grupo <- dataset %>%
      filter(organizacao_terrorista %in% grupos$organizacao_terrorista) %>%
      group_by(ano, organizacao_terrorista) %>%
      summarise(atentados = n()) %>%
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
           y = 'Eventos', 
           color = 'Grupo terrorista') +
      
      # Define escala de X
      scale_x_continuous(limits = c(ano_min, ano_max),
                         breaks = seq(ano_min, ano_max, 3)) +
      
      # Aplica o tema
      default_theme()
    
    ggplotly(gg)
  })
  
  output$mapa <- renderLeaflet({
    ano_min <- min(input$interval)
    ano_max <- max(input$interval)
    countries <- input$countries
    
    if (is.null(countries)) {
      grupos <- dataset %>% 
        filter(organizacao_terrorista != 'Unknown' & 
                 ano >= ano_min & 
                 ano <= ano_max)
    } else {
      grupos <- dataset %>% 
        filter(pais %in% countries &
                 organizacao_terrorista != 'Unknown' & 
                 ano >= ano_min & 
                 ano <= ano_max)
    }
    
    ## Gera data frame com os dados agrupados por organização
    grupos <- grupos %>%
      group_by(organizacao_terrorista) %>%
      summarise(atentados = n()) %>%
      top_n(n = 10, wt = atentados)
    
    atividade_por_cidade <- dataset %>%
      filter(organizacao_terrorista %in% grupos$organizacao_terrorista) %>%
      group_by(ano, cidade, latitude, longitude) %>%
      summarise(atentados = n()) %>%
      mutate(atentados = round(atentados, digits = 2),
             atentados_p = round((atentados / sum(atentados)) * 100, digits = 2))    
    
      leaflet() %>% addTiles() %>%  # Add default OpenStreetMap map tiles
        addMarkers(lng=atividade_por_cidade$longitude, lat=atividade_por_cidade$latitude, popup=atividade_por_cidade$cidade)
  })
}
