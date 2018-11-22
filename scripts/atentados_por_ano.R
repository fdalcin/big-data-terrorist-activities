atentados_por_ano <- function(input) {
  ## pega par?metros
  ano_min <- min(input$periodo_dashboard)
  ano_max <- max(input$periodo_dashboard)
  
  ## Gera data frame com os dados agrupados por ano
  atentados_por_ano <- dataset %>% 
    filter(ano >= ano_min & ano <= ano_max) %>%
    group_by(ano) %>% 
    summarize(atentados = n() / 1000)
  
  ## Gera o gr?fico...
  gg <- ggplot(data = atentados_por_ano, 
               aes(x = ano, y = atentados)) +
    
    # Define tra?ado da linha
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
         y = 'Ataques (em milhares)') + 
    
    # Aplica o tema
    line_theme()
  
  ggplotly(gg)
}