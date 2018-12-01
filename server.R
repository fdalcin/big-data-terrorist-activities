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
                     method = 'circle',
                     colors = c('#ff0000', '#ffffff', '#007f00'),
                     title = 'Correlaciograma',
                     insig = 'blank',
                     ggtheme = default_theme)
    
    gg
  })
  
  ### Atentados por ano
  output$atentados_por_ano <- renderPlot({
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
      # geom_point(size = 1.5, color = 'gray30') +
      
      # Define escala de X
      scale_x_continuous(limits = c(ano_min, ano_max), 
                         breaks = seq(ano_min, ano_max, 10)) +
      
      # Define labels
      labs(x = 'Ano', 
           y = 'Eventos') + 
      
      # Aplica o tema
      default_theme()
    
    gg
  })
  
  ### Atentados por pais
  output$atentados_por_pais <- renderPlot({
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
      # geom_point(size = 1.5) +
      
      # Define escala de X
      scale_x_continuous(limits = c(ano_min, ano_max), 
                         breaks = seq(ano_min, ano_max, 10)) +
      
      # Define labels
      labs(x = 'Ano', 
           y = 'Eventos',
           color = 'Países') + 
      
      # Aplica o tema
      default_theme()
    
    gg
  })
  
  ### Mortos e feridos por ano
  output$mortos_feridos_por_ano <- renderPlot({
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
      # geom_point(size = 1.5) +
      
      # Define escala de X
      scale_x_continuous(limits = c(ano_min, ano_max), 
                         breaks = seq(ano_min, ano_max, 10)) +
      
      # Define labels
      labs(x = 'Ano', 
           y = 'Vítimas', 
           color = 'Situação') + 
      
      ## Aplica o tema
      default_theme()
    
    gg
  })
  
  ### Sucesso x falha por ano
  output$atentados_sucesso_falha <- renderPlot({
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
      # geom_point(size = 1.5) +
      
      # Define escala de X
      scale_x_continuous(limits = c(ano_min, ano_max), 
                         breaks = seq(ano_min, ano_max, 10)) +
      
      # Define labels
      labs(x = 'Ano', 
           y = 'Eventos', 
           color = 'Efetividade') + 
      
      ## Aplica o tema
      default_theme()
    
    gg
  })
  
  ### Atentados por tipo
  output$atentados_por_tipo_ataque <- renderPlot({
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
               fill = 'cadetblue',
               width = 0.8) +
      
      ## Rotaciona gráfico de barras para melhorar a legibilidade
      coord_flip() +
      
      ## Define labels
      labs(x = '',
           y = 'Eventos') +
      
      ## Aplica o tema
      no_arrow_theme()
    
      gg
  })
  
  ### Atentados por grupo
  output$grupos_mais_atuantes <- renderPlot({
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
      summarise(atentados = n()) %>%
      mutate(atentados = round(atentados, digits = 2),
             atentados_p = round((atentados / sum(atentados)) * 100, digits = 2)) %>%
      top_n(n = 10, wt = atentados)
    
    ## Gera o gráfico
    gg <- ggplot(data = atentados_por_grupo, 
                 aes(x = reorder(organizacao_terrorista, atentados), y = atentados, 
                     text = paste(atentados, 'K'))) +
      
      # Define barra do gráfico
      geom_bar(stat = 'identity',
               fill = 'cadetblue',
               width = 0.8) +
      
      # Define labels
      labs(x = '', 
           y = 'Eventos') +
      
      # # Define escala do eixo Y
      # scale_y_continuous(limits = c(0, 10), 
      #                    breaks = seq(0, 10, 2)) +
      
      # Inverte o gráfico
      coord_flip() +
      
      # Aplica o tema
      no_arrow_theme()
    
      gg
  })
  
  ### Atividade por grupo por ano
  output$atividades_grupos_mais_atuantes <- renderPlot({
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
      
      # geom_point(size = 1.5) +
      
      # Define labels
      labs(x = 'Ano', 
           y = 'Eventos', 
           color = 'Grupo terrorista') +
      
      # Define escala de X
      scale_x_continuous(limits = c(ano_min, ano_max),
                         breaks = seq(ano_min, ano_max, 10)) +
      
      # Aplica o tema
      default_theme()
    
    gg
  })
  
  output$mapa_novo <- renderLeaflet({
    ano_min <- min(input$interval5)
    ano_max <- max(input$interval5)
    countries <- input$countries5
    
    if (is.null(countries)) {
      grupos <- dataset %>% 
        filter(organizacao_terrorista != 'Unknown' & 
                 ano >= ano_min & 
                 ano <= ano_max) %>%
        group_by(organizacao_terrorista) %>%
        summarise(mortes = sum(mortes_confirmadas_vitimas, na.rm = TRUE),
                  feridos = sum(numero_vitimas_feridas, na.rm = TRUE)) %>%
        arrange(desc(mortes), desc(feridos)) %>%
        head(15)
      
      atividade_por_cidade <- dataset %>%
        filter(organizacao_terrorista %in% grupos$organizacao_terrorista)
    } else {
      grupos <- dataset %>% 
        filter(pais %in% countries &
                 organizacao_terrorista != 'Unknown' & 
                 ano >= ano_min & 
                 ano <= ano_max) %>%
        group_by(organizacao_terrorista) %>%
        summarise(mortes = sum(mortes_confirmadas_vitimas, na.rm = TRUE),
                  feridos = sum(numero_vitimas_feridas, na.rm = TRUE)) %>%
        arrange(desc(mortes), desc(feridos)) %>%
        head(15)
      
      atividade_por_cidade <- dataset %>%
        filter(organizacao_terrorista %in% grupos$organizacao_terrorista &
                 pais %in% countries)
    }
    
    factorPal <- colorFactor(
      palette = 'inferno',
      domain = factor(grupos$organizacao_terrorista)
    )
    
    leaflet(data = atividade_por_cidade) %>% 
      addTiles() %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        popup = ~paste(sep = '<br/>',
                       paste('<strong>', atividade_por_cidade$organizacao_terrorista, '</strong>'),
                       paste('<strong>Cidade:</strong> ', atividade_por_cidade$cidade, atividade_por_cidade$ano),
                       paste('<strong>Mortes:</strong> ', atividade_por_cidade$mortes_confirmadas_vitimas),
                       paste('<strong>Feridos:</strong> ', atividade_por_cidade$numero_vitimas_feridas)),
        stroke = FALSE,
        radius = ~ifelse(mortes_confirmadas_vitimas > 100, 15, 5),
        color = ~factorPal(organizacao_terrorista),
        fillOpacity = 0.8) %>%
      addLegend(
        'bottomleft', 
        pal = factorPal, 
        values = ~organizacao_terrorista,
        title = 'Organizações')
    
  })
  
  ### Média de mortos por região
  output$regiao_media_mortos <- renderPlot({
    ## pega parâmetros
    regioes <- input$region
    
    # Carregar dados de mortes por regiao
    ataques_mortes_regiao <- dataset %>% 
      group_by(ano, regiao) %>% 
      summarise(atentados = n(),
                mortes = sum(mortes_confirmadas_vitimas, na.rm = TRUE),
                media_mortes = round(mortes / atentados, digits = 2)) %>% 
      filter(atentados >= 20)
    
    md_ataques_mortes_regiao <- data[data$media_mortes >= 10,]
    
    if(!is.null(regioes)){
      ataques_mortes_regiao <- md_ataques_mortes_regiao %>% filter(regiao %in% regioes)  
    }
    
    # Geração do gráfico
    gg <- ggplot(ataques_mortes_regiao, aes(x = mortes, y = atentados)) +
      
      geom_point(aes(col = regiao, size = media_mortes)) +
      
      geom_smooth(method = 'loess', se = FALSE) +
      
      guides(size = guide_legend(title = 'Proporção de Mortes'),
             colour = guide_legend(title = 'Região')) +
      
      geom_encircle(aes(x = mortes, y = atentados),
                    data = data_select,
                    color = 'red',
                    size = 1,
                    expand = 0.08) +
      
      labs(x = 'Mortes',
           y = 'Eventos',
           title = 'Eventos x Proporção de Mortes') +
      
      scale_x_continuous(limits = c(0, 20000), 
                         breaks = seq(0, 20000, 2000)) +
      
      default_theme()
    
    gg
  })
  
  ### Vitimas por organização
  output$vitimas_organizacao <- renderPlot({
    
    # Carregar dados de mortos por organização
    vitimas_organizacao <- dataset %>% 
      group_by(organizacao_terrorista) %>% 
      summarise(mortos = sum(mortes_confirmadas_vitimas, na.rm = TRUE),
                feridos = sum(numero_vitimas_feridas, na.rm = TRUE)) %>%
      filter(organizacao_terrorista != 'Unknown') %>%
      arrange(desc(mortos), desc(feridos)) %>%
      head(10)
    
    vitimas_organizacao <- gather(vitimas_organizacao, classe, cont, 2:3)
    
    ## Gera gráfico...
    gg <- ggplot(data = vitimas_organizacao, 
                 aes(x = reorder(organizacao_terrorista, cont), 
                     y = cont)) +
      
      ## Define barra do gráfico
      geom_bar(stat = 'identity',
               width = 0.8,
               color = 'white',
               aes(fill=classe)) +
      
      ## Rotaciona gráfico de barras para melhorar a legibilidade
      coord_flip()+ 
      
      ## Define labels
      labs(x = '',
           y = '') +
      
      ## Aplica o tema
      default_theme()
    
      gg
  })
  
  output$rate1 <- renderValueBox({
    valueBox(
      value = "89%",
      subtitle = "Dos ataques tiveram sucesso",
      icon= icon("bomb"),
      color="yellow"
    )
  })  
  
  output$rate2 <- renderValueBox({
    valueBox(
      value = "4%",
      subtitle = "Dos ataques são suicidas",
      icon= icon("bomb"),
      color="blue"
    )
  })
  
  output$rate3 <- renderValueBox({
    valueBox(
      value = "1570",
      subtitle = "Mortos em um único ataque terrorista",
      icon= icon("bomb"),
      color="green"
    )
  })
  
  output$rate4 <- renderValueBox({
    valueBox(
      value = "49%",
      subtitle = "Ataques com bomba",
      icon= icon("bomb"),
      color="pink"
    )
  })
  
  output$rate4 <- renderValueBox({
    valueBox(
      value = "3000",
      subtitle = "Mortos em um ataque terrorista (WTC)",
      icon= icon("bomb"),
      color="red"
    )
  })
  
  output$rate5 <- renderValueBox({
    valueBox(
      value = "49%",
      subtitle = "Ataques com bomba",
      icon= icon("bomb"),
      color="aqua"
    )
  })
  
  output$rate6 <- renderValueBox({
    valueBox(
      value = "82%",
      subtitle = "Aumento ataques após 2011",
      icon= icon("bomb"),
      color="purple"
    )
  })
}
