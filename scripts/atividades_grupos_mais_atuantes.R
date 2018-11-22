atividades_grupos_mais_atuantes <- function(input) {
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
         y = 'Atentados (em milhares)', 
         color = 'Grupo terrorista') +
    
    # Define escala de X
    scale_x_continuous(limits = c(ano_min, ano_max),
                       breaks = seq(ano_min, ano_max, 3)) +
    
    # Define escala de Y
    scale_y_continuous(limits = c(0, 1.5), 
                       breaks = seq(0, 1.5, 0.3)) +
    
    # Aplica o tema
    line_theme()
  
  ggplotly(gg)
}