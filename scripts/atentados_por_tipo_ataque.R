atentados_por_tipo_ataque <- function(input) {
  ## Gera data frame com os dados agrupados por ano
  atentados_por_tipo <- dataset %>% 
    group_by(tipo_ataque) %>% 
    summarize(atentados = n() / 1000) %>%
    mutate(atentados = round(atentados, digits = 2), 
           atentados_p = round((atentados / sum(atentados)) * 100, digits = 2))
  
  ## Gera gr?fico...
  gg <- ggplot(data = atentados_por_tipo, 
               aes(x = reorder(tipo_ataque, atentados), 
                   y = atentados, 
                   text = paste(atentados, 'K'))) +
    
    ## Define barra do gr?fico
    geom_bar(stat = 'identity',
             aes(fill = tipo_ataque),
             width = 0.8) +
    
    ## Rotaciona gr?fico de barras para melhorar a legibilidade
    coord_flip() +
    
    ## Define labels
    labs(x = '',
         y = 'Ataques (em milhares)') +
    
    ## Aplica o tema
    bar_theme()
  
  ggplotly(gg, tooltip = c('text'))
}