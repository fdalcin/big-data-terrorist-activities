mortos_feridos_por_ano <- function(input) {
  ## pega par?metros
  ano_min <- min(input$periodo_dashboard)
  ano_max <- max(input$periodo_dashboard)
  
  ## Gera data frame com os dados agrupados por ano
  atentados_por_ano <- dataset %>% 
    filter(ano >= ano_min & ano <= ano_max) %>%
    group_by(ano)
  
  ## Gera data frame de mortes nos atentados
  mortos_por_ano <- atentados_por_ano %>% 
    summarise(nvitimas = sum(mortes_confirmadas_vitimas, na.rm = TRUE) / 1000) %>%
    mutate(situacao = 'Mortos')
  
  ## Gera data frame de feridos nos atentados
  feridos_por_ano <- atentados_por_ano %>%
    summarise(nvitimas = sum(numero_vitimas_feridas, na.rm = TRUE) / 1000) %>%
    mutate(situacao = 'Feridos')
  
  ## Combina os 2 data frames em um novo
  vitimas_por_ano <- rbind.fill(mortos_por_ano, feridos_por_ano)
  
  ## Gera gr?fico...
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
         y = 'Vítimas (em milhares)', 
         color = 'Estado') + 
    
    ## Aplica o tema
    line_theme()
  
  ggplotly(gg)
}