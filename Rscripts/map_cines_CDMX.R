
# -----------------------------------------------------------------------------
# Elaborado por Inder Tecuapetla Septiembre 12, 2023
#
# En este script mostramos como crear un mapa
# interactivo de la distribución espacial de las salas de cine
# en la Ciudad de México

# --- DATASETS (en /data/cinesCDMX)
# --- CINES_DENUE_09082021.csv
# --- coloniascdmx.shp
# -----------------------------------------------------------------------------

# ---
library(raster)
library(gtools)
library(mapview)

library(leaflet)
library(leafpop)

library(RColorBrewer)
# ---

dirDATA <- list.files( path = paste0( getwd(), "/data/cinesCDMX" ),
                       pattern = ".csv", 
                       full.names = TRUE )

CINES <- read.csv( file = dirDATA[1], fileEncoding = 'latin1' )

CDMX <- CINES[CINES$Entidad.federativa=="CIUDAD DE MÉXICO",]

SHP <- shapefile( paste0( getwd(), "/data/cinesCDMX/coloniascdmx/coloniascdmx.shp" ) )

SHP_small <- SHP[,-(1:7)]

SHP_smaller <- SHP_small[,-c(2:4,6)]

# ---

NAMES_SHP <- mixedsort( unique(SHP$alcaldi) )

NAMES_CDMX <- mixedsort( c( unique(CDMX$Municipio), 
                            "Iztacalco", "Milpa Alta" ) )

# ---

cinesNUM <- numeric(16)

for(i in 1:16){
  cinesNUM[i] <- sum( CDMX$Municipio == NAMES_CDMX[i] )
}

names(cinesNUM) <- NAMES_CDMX


numCINES <- numeric( length( SHP_smaller@data$alcaldi ) )

for(i in 1:length(NAMES_SHP)){
  numCINES[ SHP_smaller@data$alcaldi == NAMES_SHP[i] ] <- cinesNUM[i]
}

SHP_smaller@data$numeroCines <- numCINES

# paleta <- c("#8dd3c7")

mp <- mapview(SHP_smaller, zcol=c("alcaldi"),
              col.regions= c(brewer.pal(12, "Set3"), 
                             brewer.pal(4, "Set1")) )

mp

# ---

XY <- SpatialPoints( coords = cbind( CDMX$Longitud, CDMX$Latitud ),
                     proj4string = crs(SHP_smaller) )

POINTS <- data.frame(x=XY@coords[,1],
                     y=XY@coords[,2],
                     z=CDMX$Razón.social)

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

XY$nombre <- CDMX$Razón.social

mp_points <- mapview(XY)


mp + mp_points


# ---

mp <- mapview(SHP_smaller, zcol=c("alcaldi"),
              layer.name="Alcaldías CDMX",
              col.regions= c(brewer.pal(12, "Set3"), brewer.pal(4, "Set1")))


tipo <- rep("Privada", 113)
tipo[101] <- "Pública"

XY$tipo <- tipo

mp_points <- mapview(XY, legend=FALSE)


mp + mp_points
