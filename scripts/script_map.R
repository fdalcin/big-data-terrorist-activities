library(ggplot2)
library(dplyr)
library(ggmap)
library(tidyverse)

dataset <- read.csv(file = 'data/terrorismo_csv.csv', header = TRUE, sep = ';', dec = ',')

dataGroup <- dataset %>% 
  group_by(pais) %>% 
  summarize("Mortes" = sum(mortes_confirmadas_vitimas, na.rm=TRUE), 
            "Feridos" = sum(numero_vitimas_feridos, na.rm=TRUE),
            "Terroristas Mortos" = sum(mortes_terroristas, na.rm=TRUE),
            "Terroristas Feridos" = sum(numero_terroristas_feridos, na.rm=TRUE))

countries <- as.character(dataGroup$pais)
latlon <- geocode(countries, source = "dsk")

WorldData <- map_data('world')
WorldData %>% filter(region != "Antarctica") -> WorldData
WorldData <- fortify(WorldData)

mp <- ggplot()
mp <- mp + geom_map(data=WorldData, map=WorldData, 
                    aes(x=long, y=lat, group=group, map_id=region),
                    fill="white", colour="#7f7f7f", size=0.5)

#
mp <- mp+ geom_point(aes(x=latlon$lon, y=latlon$lat) ,color="red", size=(dataGroup$Feridos * 0.0001))
mp <- mp+ geom_point(aes(x=latlon$lon, y=latlon$lat) ,color="blue", size=(dataGroup$Mortes * 0.0001))
mp <- mp+ geom_point(aes(x=latlon$lon, y=latlon$lat) ,color="green", size=(dataGroup$`Terroristas Mortos` * 0.0001))
mp <- mp+ geom_point(aes(x=latlon$lon, y=latlon$lat) ,color="yellow", size=(dataGroup$`Terroristas Feridos` * 0.0001))

mp

