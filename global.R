# install.packages(c('rsconnect',
#                    'tidyverse',
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

### Definição do tema padrão para utilização dos gráficos
seta <- grid::arrow(length = grid::unit(0.2, 'cm'), type = 'open')

line_theme <- function (base_size = 14, base_family = 'Arial') {
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

bar_theme <- function (base_size = 14, base_family = 'Arial') {
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

# ## Adiciona coluna de década
# dataset$decada <- ifelse(dataset$ano < 1980, '70s', 
#                          ifelse(dataset$ano < 1990, '80s', 
#                                 ifelse(dataset$ano < 2000, '90s', 
#                                        ifelse(dataset$ano < 2010, '2000s', '2010s'))))

