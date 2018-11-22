server <- function(input, output) {
  
  ### Atentados por ano
  output$atentados_por_ano <- renderPlotly({
    atentados_por_ano(input)
  })
  
  ### Mortos e feridos por ano
  output$mortos_feridos_por_ano <- renderPlotly({
    mortos_feridos_por_ano(input)
  })
  
  ### Atentados por tipo
  output$atentados_por_tipo_ataque <- renderPlotly({
    atentados_por_tipo_ataque(input)
  })
  
  ### Sucesso x falha por ano
  output$atentados_sucesso_falha <- renderPlotly({
    atentados_sucesso_falha(input)
  })
  
  ### Atentados por grupo
  output$grupos_mais_atuantes <- renderPlotly({
    grupos_mais_atuantes(input)
  })
  
  ### Atividade por grupo por ano
  output$atividades_grupos_mais_atuantes <- renderPlotly({
    atividades_grupos_mais_atuantes(input)
  })
}
