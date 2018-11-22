grupos_mais_atuantes <- function(input) {
  ## pega par?metros
  ano_min <- min(input$periodo_terrorist)
  ano_max <- max(input$periodo_terrorist)
  
  ## Gera data frame com os dados agrupados por organiza??o
  atentados_por_grupo <- dataset %>%
    filter(organizacao_terrorista != 'Unknown' & ano >= ano_min & ano <= ano_max) %>%
    group_by(organizacao_terrorista) %>%
    summarise(atentados = n() / 1000) %>%
    mutate(atentados = round(atentados, digits = 2),
           atentados_p = round((atentados / sum(atentados)) * 100, digits = 2)) %>%
    top_n(n = 10, wt = atentados)
  
  ## Gera o gr?fico
  gg <- ggplot(data = atentados_por_grupo, 
               aes(x = reorder(organizacao_terrorista, atentados), y = atentados, 
                   text = paste(atentados, 'K'))) +
    
    # Define barra do gr?fico
    geom_bar(stat = 'identity',
             aes(fill = organizacao_terrorista),
             width = 0.8) +
    
    # Define labels
    labs(x = '', 
         y = 'Atentados (em milhares)') +
    
    # Define escala do eixo Y
    scale_y_continuous(limits = c(0, 10), 
                       breaks = seq(0, 10, 2)) +
    
    # Inverte o gr?fico
    coord_flip() +
    
    # Aplica o tema
    bar_theme()
  
  ggplotly(gg, tooltip = c('text'))
}