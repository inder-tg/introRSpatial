
# -----------------------------------------------------------------------------
# Elaborado por Inder Tecuapetla Septiembre 12, 2023
# Actualizado Septiembre 20, 2023
#
# En este script mostramos como crear un mapa
# interactivo de la distribución espacial de las salas de cine
# en la Ciudad de México usando algunas de las magias del tidyverse

# --- DATASETS (en /data/cinesCDMX)
# --- CINES_DENUE_09082021.csv
# --- coloniascdmx.shp
# -----------------------------------------------------------------------------

# ---
library(raster)
library(gtools)
library(tidyverse)

library(mapview)

library(leaflet)
library(leafpop)
library(RColorBrewer)
# ---

dirDATA <- list.files( path = paste0( getwd(), "/data/cinesCDMX" ),
                       pattern = ".csv", 
                       full.names = TRUE )

CINES_tidy <- read_csv(dirDATA[1], 
                     locale = locale( encoding = "latin1" ))

CINES_tidy_CDMX <- CINES_tidy |>
  filter(`Entidad federativa` == "CIUDAD DE MÉXICO")

SHP <- shapefile( paste0( getwd(), "/data/cinesCDMX/coloniascdmx/coloniascdmx.shp" ) )

SHP_alcaldia <- SHP[,"alcaldi"]

# ---

cineByMunicipio <- count(CINES_tidy_CDMX, Municipio)

# ---

# cinesNUM <- numeric(16)
# 
# for(i in 1:16){
#   cinesNUM[i] <- sum( CDMX$Municipio == NAMES_CDMX[i] )
# }
# 
# names(cinesNUM) <- NAMES_CDMX
# 
# 
numCINES <- numeric( length( SHP_alcaldia@data$alcaldi ) )

unique( SHP_alcaldia$alcaldi )

unique( cineByMunicipio$Municipio )

numCINES[ SHP_alcaldia$alcaldi == "MIGUEL HIDALGO" ] <- as.numeric(unlist(cineByMunicipio[9,2]))

# --- código eficiente

# ---

namesSHP <- mixedsort( unique(SHP_alcaldia$alcaldi) )

namesCINES <- mixedsort( c(unique(cineByMunicipio$Municipio), 
                           "Iztacalco", "Milpa Alta") )

namesCINES <- mixedsort(unique(cineByMunicipio$Municipio))


# --

temp <- cineByMunicipio[cineByMunicipio$Municipio==namesCINES[1],2]

numCINES[ SHP_alcaldia$alcaldi == namesSHP[1] ] <- as.numeric(unlist(temp))

namesSHP_aux <- namesSHP[-c(8,12)]

for(i in 1:length(namesSHP_aux)){
  
  temp <- cineByMunicipio[cineByMunicipio$Municipio==namesCINES[i],2]
  
  numCINES[ SHP_alcaldia$alcaldi == namesSHP_aux[i] ] <- as.numeric(unlist(temp))
  
}

SHP_alcaldia@data$numeroCines <- numCINES

mp <- mapview(SHP_alcaldia, zcol=c("alcaldi"),
              col.regions= c(brewer.pal(12, "Set3"), 
                             brewer.pal(4, "Set1")) )

mp

# --- Agregando puntos con info de los cines

XY <- SpatialPoints( coords = cbind( CINES_tidy_CDMX$Longitud, 
                                     CINES_tidy_CDMX$Latitud ),
                     proj4string = crs(SHP_alcaldia) )

POINTS <- data.frame(x=XY@coords[,1],
                     y=XY@coords[,2],
                     z=CINES_tidy_CDMX$`Razón social`)

cinesPoints <- leaflet( POINTS ) %>%
  addTiles(group = "OSM") %>%
  # addProviderTiles(providers$OpenStreetMap, group = "Open Street") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Esri") %>%
  addLayersControl(baseGroups = c("OSM","Esri")) %>%
  addCircles(~x, ~y, radius = ~4, col="purple",
             popup = paste0("<b> lon: </b>", round(POINTS$x), "<br>",
                            "<b> lat: </b>", round(POINTS$y)) )

cinesPoints
# ---


mp + XY

# ---

str(XY)

XY$nombre <- CINES_tidy_CDMX$`Razón social`

mp_points <- mapview(XY)


mp + mp_points


# --- Mapa final

mp <- mapview(SHP_alcaldia, zcol=c("alcaldi"),
              layer.name="Alcaldías CDMX",
              col.regions= c(brewer.pal(12, "Set3"), brewer.pal(4, "Set1")))


tipo <- rep("Privada", 113)
tipo[101] <- "Pública"

XY$tipo <- tipo

mp_points <- mapview(XY, legend=FALSE,
                     layer.name="Cines")


mp + mp_points
