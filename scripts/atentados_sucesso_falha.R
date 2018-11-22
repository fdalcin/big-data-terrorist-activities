atentados_sucesso_falha <- function(input) {
  ## pega par?metros
  ano_min <- min(input$periodo_dashboard)
  ano_max <- max(input$periodo_dashboard)
  
  ## Gera data frame com os dados agrupados por ano
  atentados_sucesso_falha <- dataset %>%
    filter(ano >= ano_min & ano <= ano_max) %>%
    group_by(ano, sucesso) %>%
    summarise(atentados = n() / 1000) %>%
    mutate(sucesso = ifelse(sucesso == 1, 'Sucesso', 'Falha'))
  
  ## Gera gr?fico...
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
         y = 'Ataques (em milhares)', 
         color = 'Efetividade') + 
    
    ## Aplica o tema
    line_theme()
  
  ggplotly(gg)
}