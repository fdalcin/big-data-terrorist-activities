server <- function(input, output) {
  
  ### Contabiliza atentados por ano...
  output$atentados_por_ano <- renderPlot({
    ## pega parâmetros
    ano_min <- min(input$periodo_dashboard)
    ano_max <- max(input$periodo_dashboard)
    
    ## Gera data frame com os dados agrupados por ano
    atentados_por_ano <- dataset %>% 
      group_by(ano) %>% 
      summarize(atentados = sum(n(), na.rm = TRUE))
    
    ## Filtra de acordo com o intervalo selecionado
    atentados_por_ano_filtro <- atentados_por_ano[atentados_por_ano$ano >= ano_min & atentados_por_ano$ano <= ano_max,]
    
    ## Gera o gráfico...
    gg <- ggplot(data = atentados_por_ano_filtro, 
                 aes(x = ano, y = atentados)) +
      
      # Gera linha no gráfico
      geom_line() +
      
      # Gera pontos em cada ano
      geom_point(data = atentados_por_ano_filtro, 
                 aes(x = ano, y = atentados), 
                 pch = 21, 
                 fill = 'white', 
                 color = 'black', 
                 alpha = 1, 
                 size = 3) +
      
      # Define labels
      labs(x = 'Ano', 
           y = 'Número de atentados', 
           caption = 'Fonte: Global Terrorism - https://www.kaggle.com/START-UMD/gtd') +
      
      # Define escala de X
      scale_x_continuous(limits = c(ano_min, ano_max), 
                         breaks = seq(ano_min, ano_max, 3)) +
      #scale_x_continuous(limits = c(1970, 2017), breaks = seq(1970, 2017, 3)) +
      
      # Define escala de Y
      scale_y_continuous(limits = c(0, 20000), 
                         breaks = seq(0, 20000, 2000)) +
      
      # Aplica o tema pré configurado
      line_theme()
    
    gg
  })
  
  ### Contabiliza atentados por grupo...
  output$grupos_mais_atuantes <- renderPlot({
    ## pega parâmetros
    ano_min <- min(input$periodo_terrorist)
    ano_max <- max(input$periodo_terrorist)
    
    ## Gera data frame com os dados agrupados por organização
    ## Considera número de atentados >= 1000
    atentados_por_grupo <- dataset %>%
      filter(organizacao_terrorista != 'Unknown' & 
               ano >= ano_min & 
               ano <= ano_max) %>%
      group_by(organizacao_terrorista) %>%
      summarise(atentados = sum(n(), na.rm = TRUE)) %>%
      filter(atentados >= 1000)
      
    ## Ordena os grupos pelo número de atentados
    atentados_por_grupo <- atentados_por_grupo[order(atentados_por_grupo$atentados),]
    
    ## Transforma a organização em fator
    atentados_por_grupo$organizacao_terrorista <- factor(atentados_por_grupo$organizacao_terrorista,
                                                         levels = atentados_por_grupo$organizacao_terrorista)
    
    ## Gera o gráfico
    gg <- ggplot(data = atentados_por_grupo, 
                 aes(x = organizacao_terrorista, 
                     y = atentados, 
                     fill = atentados)) +
      
      # Gera barras no gráfico
      geom_bar(stat = 'identity', 
               width = 0.8) +
      
      # Define labels
      labs(x = '', 
           y = 'Número de atentados', 
           caption = 'Fonte: Global Terrorism - https://www.kaggle.com/START-UMD/gtd') +
      
      # Define escala do eixo Y
      scale_y_continuous(limits = c(0, 8000), 
                         breaks = seq(0, 8000, 500)) +
      
      # Inverte o gráfico
      coord_flip() +
      
      # Aplica o tema pré configurado
      bar_theme()
    
    gg
  })
  
  ### Contabiliza atividade por grupo em cada ano...
  output$atividades_grupos_mais_atuantes <- renderPlot({
    ## pega parâmetros
    ano_min <- min(input$periodo_terrorist)
    ano_max <- max(input$periodo_terrorist)
    
    ## Gera data frame com os dados agrupados por organização
    ## Considera número de atentados >= 1000
    atentados_por_grupo <- dataset %>%
      filter(organizacao_terrorista != 'Unknown' & 
               ano >= ano_min & 
               ano <= ano_max) %>%
      group_by(organizacao_terrorista) %>%
      summarise(atentados = sum(n(), na.rm = TRUE)) %>%
      filter(atentados >= 1000)

    ## Gera data frame com os dados agrupados por ano e organização
    atividade_por_grupo <- dataset %>%
      filter(organizacao_terrorista %in% atentados_por_grupo$organizacao_terrorista) %>%
      group_by(ano, organizacao_terrorista) %>%
      summarise(atentados = sum(n(), na.rm = TRUE))
    
    ## Gera o gráfico
    gg <- ggplot(data = atividade_por_grupo, 
                  aes(x = ano, 
                      y = atentados, 
                      col = organizacao_terrorista)) +
      
      # Gera linhas no gráfico
      geom_line(size = 0.8) +
      
      # Define labels
      labs(x = '', 
           y = '', 
           color = 'Organização terrorista', 
           caption = 'Fonte: Global Terrorism - https://www.kaggle.com/START-UMD/gtd') +
      
      # Define escala de X
      scale_x_continuous(limits = c(ano_min, ano_max), 
                         breaks = seq(ano_min, ano_max, 3)) +
      
      # Define escala de Y
      scale_y_continuous(limits = c(0, 1500), 
                         breaks = seq(0, 1500, 300)) +
      
      # Aplica o tema pré configurado
      line_theme()
    
    gg
  })
}
