# install.packages(c('rsconnect',
#                    'tidyverse',
#                    'plotly',
#                    'plyr',
#                    'scales',
#                    'arules',
#                    'arulesViz',
#                    'car',
#                    'caret',
#                    'zoo',
#                    'ggcorrplot',
#                    'ggalt',
#                    'ggmap',
#                    'ggpubr',
#                    'ggExtra',
#                    'ggfortify',
#                    'ggplotify',
#                    'rworldmap',
#                    'treemapify',
#                    'shiny',
#                    'shinydashboard'),
#                  dependencies = TRUE)



### Carregando pacotes
library(plyr)
library(tidyverse)
library(plotly)
library(scales)
library(arules)
library(arulesViz)
library(car)
library(caret)
library(zoo)
library(ggcorrplot)
library(ggalt)
library(ggmap)
library(ggpubr)
library(ggExtra)
library(ggfortify)
library(ggplotify)
library(rworldmap)
library(treemapify)
library(shiny)
library(shinydashboard)
library(leaflet)
library(ggplot2)
library(dplyr)
library(ggmap)
library(tidyverse)

### Definição do tema padrão para utilização dos gráficos
seta <- grid::arrow(length = grid::unit(0.2, 'cm'), type = 'open')

default_theme <- function (base_size = 14, base_family = 'Arial') {
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(axis.ticks = element_blank(),
          axis.line = element_line(arrow = seta, color = 'gray20'),
          legend.background = element_blank(),
          legend.key = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          strip.background = element_blank(),
          plot.background = element_blank(),
          plot.title = element_text(hjust = 1),
          complete = TRUE)
}

no_arrow_theme <- function (base_size = 14, base_family = 'Arial') {
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(axis.ticks = element_blank(),
          legend.background = element_blank(),
          legend.key = element_blank(),
          legend.position = 'none',
          panel.background = element_blank(),
          panel.border = element_blank(),
          strip.background = element_blank(),
          plot.background = element_blank(),
          plot.title = element_text(hjust = 1),
          complete = TRUE)
}


### Carregando base de dados...
dataset <- read.csv(file = 'data/terrorismo_csv.csv', header = TRUE, sep = ';', dec = ',')

data <- dataset %>% 
  group_by(ano, regiao) %>% 
  summarise(atentados = n(),
            mortes = sum(mortes_confirmadas_vitimas, na.rm = TRUE),
            media_mortes = round(mortes / atentados, digits = 2)) %>% 
  filter(atentados >= 20)

data_select <- data[data$media_mortes >= 10,]

# Eventos x Ano
gg <- ggplot(data, aes(x = ano, y = atentados)) +
  
  geom_point(aes(col = regiao, size = mortes)) +
  
  geom_smooth(method = 'loess', se = FALSE) +
  
  guides(size = guide_legend(title = 'Mortes'),
         colour = guide_legend(title = 'Região')) +
  
  labs(x = 'Ano',
       y = 'Eventos',
       title = 'Eventos x Ano') +
  
  default_theme()

gg

# Eventos x Média de Mortes
gg <- ggplot(data, aes(x = mortes, y = atentados)) +
  
  geom_point(aes(col = regiao, size = media_mortes)) +
  
  geom_smooth(method = 'loess', se = FALSE) +
  
  guides(size = guide_legend(title = 'Média de Mortes'),
         colour = guide_legend(title = 'Região')) +
  
  geom_encircle(aes(x = mortes, y = atentados),
                data = data_select,
                color = 'red',
                size = 1,
                expand = 0.08) +
  
  labs(x = 'Mortes',
       y = 'Eventos',
       title = 'Eventos x Média de Mortes') +
  
  scale_x_continuous(limits = c(0, 20000), 
                     breaks = seq(0, 20000, 2000)) +
  
  default_theme()

gg

# Boxplot da relação entre eventos x mortes
ggMarginal(gg, type = 'boxplot', fill = 'transparent')
 
# Mortos por organizações terroristas
vitimas_organizacao <- dataset %>% 
  group_by(organizacao_terrorista) %>% 
  summarise(mortos = sum(mortes_confirmadas_vitimas, na.rm = TRUE),
            feridos = sum(numero_vitimas_feridas, na.rm = TRUE),
            vitimas = mortos+feridos) %>%
  filter(organizacao_terrorista != 'Unknown') %>%
  arrange(desc(vitimas)) %>%
  head(10)

  ## Gera gráfico...
  gg <- ggplot(data = vitimas_organizacao, 
          aes(x = organizacao_terrorista, 
          y = vitimas)) +
  
  ## Define barra do gráfico
  geom_bar(stat = 'identity',
           width = 0.8,
           aes(fill = organizacao_terrorista)) +
  
  ## Rotaciona gráfico de barras para melhorar a legibilidade
  coord_flip()+ 
  
  ## Define labels
  labs(x = '',
       y = '',
       title = 'Vítimas por Organização') +
  
  ## Aplica o tema
  no_arrow_theme()

  ggplotly(gg, tooltip = c('text'))

  

