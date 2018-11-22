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
            title = 'Atentados ao longo dos anos',
            width = 8,
            plotOutput('atentados_por_ano')
          ),

          box(
            title = 'Selecione um período',
            width = 4,
            sliderInput('periodo_dashboard', 'Intervalo:', ano_min, ano_max, c(dataset), step = 1, dragRange = TRUE)
          )
        ),
        
        fluidRow(
          box(
            title = 'Vítimas ao longo dos anos',
            width = 8,
            plotOutput('vitimas_por_ano')
          )
        ),
        
        fluidRow(
          box(
            title = 'Feridos ao longo dos anos',
            width = 8,
            plotOutput('feridos_por_ano')
          )
        ),
        
        fluidRow(
          box(
            title = 'Atentados por tipo',
            width = 8,
            plotOutput('atentados_por_tipo_ataque')
          )
        )
      ),
      
      # Teste tab
      tabItem(
        tabName = 'tabTerroristGroups',
        fluidRow(
          box(
            title = 'Grupos mais atuantes',
            width = 8,
            plotOutput('grupos_mais_atuantes')
          ),
          
          box(
            title = 'Selecione um período',
            width = 4,
            sliderInput('periodo_terrorist', 'Intervalo:', ano_min, ano_max, c(dataset), step = 1, dragRange = TRUE)
          )
        ),
        
        fluidRow(
          box(
            title = 'Atividades por grupo ao longo dos anos',
            width = 8,
            plotOutput('atividades_grupos_mais_atuantes')
          )
        )
      ) 
    )
  )
)
