# Gera lista de anos 
anos <- unique(dataset$ano)
ano_min <- min(anos)
ano_max <- max(anos)

# Gera lista de países
paises <- dataset %>% group_by(pais) %>% summarise(atentados = n()) %>% filter(atentados > 50)
paises <- paises[order(paises$pais), ] %>% select(pais)

# Gera lista de regiões
regioes <- distinct(dataset, regiao)

dashboardPage(
  dashboardHeader(title = 'Global Terrorism'),
  
  dashboardSidebar(
    sidebarMenu(
      # Home
      menuItem('Home', tabName = 'tabHome', icon = icon('home')),
      
      # Introducao
      menuItem('Introducao', tabName = 'tabIntroducao', icon = icon('comments-o')),
      
      # Database
      menuItem('Base de dados', tabName = 'tabBasedados', icon = icon('database')),
      
      # Atentados
      menuItem(text = 'Eventos',
               tabName = 'atentados',
               icon = icon('fire'),
               
               # Ao longo dos anos
               menuSubItem(text= 'Ao longo dos anos',
                           tabName = 'atentados_anos',
                           icon = icon('line-chart')),
               
               # Tipo de ataque
               menuSubItem(text = 'Tipo de ataque',
                           tabName = 'tipo_ataque',
                           icon = icon('bar-chart')),
               
               # Média de mortes por região
               menuSubItem(text = 'Mortes por região',
                           tabName = 'media_mortos_regiao',
                           icon = icon('calendar'))),
      
      # Grupos Terroristas
      menuItem(text = 'Grupos Terroristas',
               tabName = 'grupos',
               icon = icon('fire'),
               
               # Mais atuantes
               menuSubItem(text = 'Mais atuantes',
                           tabName = 'grupos_mais_atuantes',
                           icon = icon('bar-chart'))),
      
      # Mapa 
      menuItem(text = 'Mapa',
               tabName = 'mapa_novo',
               icon = icon('map-marker'))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Home
      tabItem(
        tabName = 'tabHome',
        fluidRow(
          column(12, includeHTML('html/home.html'))
        )
      ),
      
      # Introducao
      tabItem(
        tabName = 'tabIntroducao',
        fluidRow(
          box(
            width = 12,
            column(12, includeHTML('html/introducao.html')),
            column(12, valueBoxOutput("rate1"), 
                   valueBoxOutput("rate2"),
                   valueBoxOutput("rate3"), 
                   valueBoxOutput("rate4"),
                   valueBoxOutput("rate5"),
                   valueBoxOutput("rate6")),
            column(12, plotOutput('correlacao'))
          )
        )
      ),
      
      # Base de dados
      tabItem(
        tabName = 'tabBasedados',
        fluidRow(
          column(12, includeHTML('html/base.html'))
        )
      ),
      
      # Atentados ao longo dos anos
      tabItem(
        tabName = 'atentados_anos',
        
        fluidRow(
          column(
            width = 4,
            box(
              width = 12,
              h2('Filtros'),
              sliderInput('interval', 'Intervalo:', ano_min, ano_max, c(dataset), step = 1, dragRange = TRUE),
              selectInput('countries', 'País', paises, selected = 'United States', multiple = TRUE, selectize = TRUE),
              includeHTML("html/imagem.html")
            )
          ),
          
          column(
            width = 8,
            
            fluidRow(
              box(
                title = 'Eventos ao longo dos anos',
                width = 12,
                plotOutput('atentados_por_ano')
              ),
              
              box(
                title = 'Comparação de eventos por país',
                width = 12,
                plotOutput('atentados_por_pais')
              )
            ),
            
            fluidRow(
              box( 
                title = 'Vítimas e Feridos por ano',
                width = 12,
                plotOutput('mortos_feridos_por_ano')
              ),
              
              box(
                title = 'Efetividade dos ataques por ano',
                width = 12,
                plotOutput('atentados_sucesso_falha')
              )
            )
          )
        )
      ),
      
      # Tipo de ataque
      tabItem(
        tabName = 'tipo_ataque',
        
        fluidRow(
          column(
            width = 4,
            box(
              width = 12,
              h2('Filtros'),
              sliderInput('interval2', 'Intervalo:', ano_min, ano_max, c(dataset), step = 1, dragRange = TRUE),
              selectInput('countries2', 'País', paises, selected = 'United States', multiple = TRUE, selectize = TRUE),
              includeHTML("html/imagem.html")
            )
          ),
          
          column(
            width = 8,
            fluidRow(
              box(
                title = 'Eventos por tipo de ataque',
                width = 12,
                plotOutput('atentados_por_tipo_ataque')
              )
            )
          )
        )
      ),
      
      # Mortes por região
      tabItem(
        tabName = 'media_mortos_regiao',
        
        fluidRow(
          column(
            width = 4,
            box(
              width = 12,
              h2('Filtros'),
              selectInput('region','Região', regioes, multiple = TRUE, selectize = TRUE),
              includeHTML("html/imagem.html")
            )
          ),
          
          column(
            width = 8,
            fluidRow(
              box(
                title = 'Proporção de mortos por região',
                width = 12,
                plotOutput('regiao_media_mortos')
              )
            )
          )
        )
      ),
      
      # Grupos mais atuantes
      tabItem(
        tabName = 'grupos_mais_atuantes',
        
        fluidRow(
          column(
            width = 4,
            box(
              width = 12,
              h2('Filtros'),
              sliderInput('interval3', 'Intervalo:', ano_min, ano_max, c(dataset), step = 1, dragRange = TRUE),
              selectInput('countries3', 'País', paises, selected = 'United States', multiple = TRUE, selectize = TRUE),
              includeHTML("html/imagem.html")
            )
          ),
        
          column(
            width = 8,
            fluidRow(
              box(
                title = 'Grupos mais atuantes',
                width = 12,
                plotOutput('grupos_mais_atuantes')
              ),
              
              box(
                title = 'Atividades por grupo ao longo dos anos',
                width = 12,
                plotOutput('atividades_grupos_mais_atuantes')
              ),
              
              box(
                title = 'Vítimas por organização',
                width = 12,
                plotOutput('vitimas_organizacao')
              )
            )
          )
        )
      ),
      
      tabItem(
        tabName = 'mapa_novo',
        
        div(
          class = 'outer',
          
          tags$head(
            includeCSS('styles.css')
          ),
            
          leafletOutput(
            'mapa_novo', 
            width='100%', 
            height='100%'
          ),
          
          absolutePanel(
            class = 'panel panel-default absolute-panel', 
            fixed = TRUE, 
            draggable = TRUE, 
            top = 60, 
            right = 20, 
            width = 400,
            height = 'auto',
            h2('Filtros'),
            sliderInput('interval5', 'Intervalo:', ano_min, ano_max, c(dataset), step = 1, dragRange = TRUE),
            selectInput('countries5', 'País', paises, selected = 'United States', multiple = TRUE, selectize = TRUE)
          )
        )
      )
    )
  )
)
