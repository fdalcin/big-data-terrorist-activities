anos = unique(dataset$ano)
ano_min = min(anos)
ano_max = max(anos)

dashboardPage(
  dashboardHeader(title = 'Dashboard'),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem(text = 'Dashboard', tabName = 'tabDashboard', icon = icon('dashboard')),
      menuItem(text = 'Grupos terroristas', tabName = 'tabTerroristGroups', icon = icon('fire'))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Dashboard tab
      tabItem(
        tabName = 'tabDashboard', 
        
        fluidRow(
          box(
            title = 'Selecione um período',
            width = 6,
            sliderInput('periodo_dashboard', 'Intervalo:', ano_min, ano_max, c(dataset), step = 1, dragRange = TRUE)
          )
        ),
        
        fluidRow(
          box(
            title = 'Atentados ao longo dos anos',
            width = 12,
            plotlyOutput('atentados_por_ano')
          ),
          
          box(
            title = 'Vítimas ao longo dos anos',
            width = 12,
            plotlyOutput('mortos_feridos_por_ano')
          ),
          
          box(
            title = 'Efetividade dos ataques',
            width = 12,
            plotlyOutput('atentados_sucesso_falha')
          ),
          
          box(
            title = 'Atentados por tipo',
            width = 12,
            plotlyOutput('atentados_por_tipo_ataque')
          )
        )
      ),
      
      # Teste tab
      tabItem(
        tabName = 'tabTerroristGroups',
        fluidRow(
          box(
            title = 'Selecione um período',
            width = 6,
            sliderInput('periodo_terrorist', 'Intervalo:', ano_min, ano_max, c(dataset), step = 1, dragRange = TRUE)
          )
        ),
        
        fluidRow(
          box(
            title = 'Grupos mais atuantes',
            width = 12,
            plotlyOutput('grupos_mais_atuantes')
          ),
          
          box(
            title = 'Atividades por grupo ao longo dos anos',
            width = 12,
            plotlyOutput('atividades_grupos_mais_atuantes')
          )
        )
      ) 
    )
  )
)
