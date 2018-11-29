library(leaflet)
library(ggplot2)
library(dplyr)
library(ggmap)
library(tidyverse)

dataset <- read.csv(file = 'data/terrorismo_csv.csv', header = TRUE, sep = ';', dec = ',')

dataGroup <- dataset %>% 
  group_by(pais) %>% 
  summarize("Mortes" = sum(mortes_confirmadas_vitimas, na.rm=TRUE), 
            "Feridos" = sum(numero_vitimas_feridas, na.rm=TRUE),
            "Terroristas Mortos" = sum(mortes_terroristas, na.rm=TRUE),
            "Terroristas Feridos" = sum(numero_terroristas_feridos, na.rm=TRUE))

countries <- as.character(dataGroup$pais)
latlon <- geocode(countries, source = "dsk")

m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=dataset$longitude, lat=dataset$latitude, popup="The birthplace of R")
m

smoke <- table(latlon$lat,latlon$lon)

leaflet(smoke) %>% addTiles() %>% addMarkers(
  clusterOptions = markerClusterOptions()
)

leaflet(countries) %>% addTiles() %>%
  addCircles(lng=latlon$lon, lat=latlon$lat, weight=1, radius=dataGroup$Mortes * 10, 
             popup=dataGroup$pais, color = "red") %>%
  addCircles(lng=latlon$lon, lat=latlon$lat, weight=1, radius=dataGroup$Feridos * 10, 
             popup=dataGroup$pais, color = "blue") %>%
  addCircles(lng=latlon$lon, lat=latlon$lat, weight=1, radius=dataGroup$`Terroristas Mortos` * 10, 
           popup=dataGroup$pais, color = "orange") %>%
  addCircles(lng=latlon$lon, lat=latlon$lat, weight=1, radius=dataGroup$`Terroristas Feridos` * 10, 
           popup=dataGroup$pais, color = "green")
