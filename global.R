#install.packages(c("ggplot2", "ggcorrplot", "ggalt", 
#                   "ggExtra", "ggthemes", "ggplotify",
#                   "treemapify", "plyr", "dplyr", "scales",
#                   "zoo", "shiny", "shinydashboard"), 
#                 dependencies = TRUE)

### Carregando pacotes
library(ggplot2)
library(ggcorrplot)
library(ggalt)
library(ggExtra)
library(ggthemes)
library(ggplotify)
library(treemapify)
library(plyr)
library(dplyr)
library(scales)
library(zoo)
library(shiny)
library(shinydashboard)
library(shinythemes)

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
dataset <- read.csv(file = 'data/terrorismo_csv.csv', header = TRUE, sep = ";", dec = ",")
