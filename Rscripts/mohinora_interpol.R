
# ------------------------------------------------------------------------------
# Elaborado por Inder Tecuapetla en Mayo 22, 2026

# En este script presentamos varias ideas vinculadas al rellenado de datos
# faltantes en imágenes satelitales

# DATASETS. Los principales archivos utilizados en este script están en la carpeta
# /data/mohinora:
# 1. Folder 250m_16_days_NDVI
# 2. folder 250m_16_days_pixel_reliability
# ------------------------------------------------------------------------------

# Preamble
library(here)
library(terra)
library(sf)
library(dplyr)
library(RColorBrewer)
library(mapview)
library(imputeTS)
library(doParallel)
library(geoTS)

source("Rscripts/auxFUN.R")

# Carga de datos
directorios <- list.dirs( path = here("data") )

archivos_NDVI <- list.files( path = directorios[8],
                             pattern = ".tif",
                             full.names = TRUE )

archivos_QA <- list.files( path = directorios[11],
                           pattern = ".tif",
                           full.names = TRUE )

archivos_SHP <- list.files( path = directorios[2],
                            pattern = ".shp",
                            full.names = TRUE )

NDVI <- rast( archivos_NDVI )
QA <- rast( archivos_QA )

SHP <- st_read(archivos_SHP)

# Inspección visual raster

plot( subset(NDVI, 150), main = "Capa 150 de NDVI" )
plot( subset(QA, 150), main = "Capa 150 de QA" )

par( mfrow = c(1,2) )
plot( subset(NDVI, 150), main = "Capa 150 de NDVI" )
plot( subset(QA, 150), main = "Capa 150 de QA" )

# Manipulando SHP

mohinora_polygon <- SHP %>% filter( NOMBRE == "Cerro Mohinora" )
mohinora_sinu <- st_transform( mohinora_polygon, crs = crs(NDVI) )

# Inspección visual raster + sf

par( mfrow = c(1,2) )
plot( subset(NDVI, 150), main = "Capa 150 de NDVI" )
lines( mohinora_sinu, lwd = 3 )

plot( subset(QA, 150), main = "Capa 150 de QA" )
lines( mohinora_sinu, lwd = 3 )

# Recortes

NDVI_mohinora <- crop( NDVI, mohinora_sinu, mask = TRUE )
QA_mohinora <- crop( QA, mohinora_sinu, mask = TRUE )

# Inspección visual de los recortes

par( mfrow = c(1,2) )
plot( subset(NDVI_mohinora, 150), main = "Capa 150 de NDVI" )
lines( mohinora_sinu, lwd = 3 )

plot( subset(QA_mohinora, 150), main = "Capa 150 de QA" )
lines( mohinora_sinu, lwd = 3 )

# Ejemplo máscara NDVI + QA

ndviTEMP <- subset( NDVI_mohinora, 150 )
qaTEMP <- subset( QA_mohinora, 150 )

# Pixel reliability codes:
# • 	0 = good data
# • 	1 = marginal
# • 	2 = snow/ice
# • 	3 = cloudy

# Descartando pixels con baja calidad
ndviTEMP[ qaTEMP >= 2 ] <- NA

par( mfrow = c(1,2) )
plot( ndviTEMP, main = "Capa 150 de NDVI+QA" )
lines( mohinora_sinu, lwd = 3 )

plot( subset(QA_mohinora, 150), main = "Capa 150 de QA" )
lines( mohinora_sinu, lwd = 3 )

# --- Aplicando las ideas del Ejemplo a todo el dataset

# Una función para aplicar la idea de arriba de forma genérica

getReliableNDVI <- function(rasterNDVI, rasterReliability, 
                            layer, ndviNAMES, dirToSave) {
  
  if ( !dir.exists(dirToSave) ) { 
    dir.create( dirToSave ) 
  }
  
  tempNDVI <- subset(rasterNDVI, layer)
  tempRELIA <- subset(rasterReliability, layer)
  
  tempNDVI[ tempRELIA >= 2 ] <- NA
  
  NAME <- strsplit( basename( ndviNAMES[layer] ), ".tif" )[[1]][1]
  
  writeRaster( tempNDVI,
               filename = paste0( dirToSave, "/", NAME, "_QA.tif" ) ,
               datatype = "INT2S",
               overwrite = TRUE )
} 

whereToSave <- here( "data", "mohinora", "250m_16_days_NDVI_reliability" )

getReliableNDVI(rasterNDVI = NDVI_mohinora, 
                rasterReliability = QA_mohinora,
                layer = 150, 
                ndviNAMES = archivos_NDVI,
                dirToSave = whereToSave )

# Comprobando que el resultado es correcto
directorios <- list.dirs( path = here("data") )

archivo_NDVI_QA <- list.files( path = directorios[10],
                             pattern = ".tif",
                             full.names = TRUE )

ndviTEST <- rast(archivo_NDVI_QA)

par( mfrow = c(1,2) )
plot( ndviTEMP, main = "Capa 150 de NDVI+QA" )
lines( mohinora_sinu, lwd = 3 )

plot( ndviTEST, main = "Capa NDVI+QA via getReliableNDVI" )
lines( mohinora_sinu, lwd = 3 )
# ---

# Aplicando la función de arriba 549 veces
for ( i in 1:nlyr(NDVI_mohinora) ) {
  getReliableNDVI(rasterNDVI = NDVI_mohinora, 
                  rasterReliability = QA_mohinora,
                  layer = i, 
                  ndviNAMES = archivos_NDVI,
                  dirToSave = whereToSave)
}

# --- Cuál es la calidad del dataset

directorios <- list.dirs( path = here("data") )

archivos_NDVI_QA <- list.files( path = directorios[10],
                                pattern = ".tif",
                                full.names = TRUE )

NDVI_QA <- rast(archivos_NDVI_QA)

# --- función auxiliar para calcular porcentaje de dato faltante por pixel
percent_na_at_pixel <- function(pixel){
  
  s <- sum( is.na(pixel) ) / length(pixel)
    
  return(s)
}

# --- Usando función app para aplicar percent_na_at_pixel a los pixels de NDVI_QA
system.time({
  percent_NA_NDVI <- app(NDVI_QA, percent_na_at_pixel)
})

plot(percent_NA_NDVI)
hist(percent_NA_NDVI, breaks = 30)

par(mfrow=c(1,1))
plot(percent_NA_NDVI * 100, 
     main = "% missing values",
     breaks=c(0,2.5,5,10,15,20,25,50,100), 
     col = brewer.pal(n=8, "Spectral"))


mp_percent_missing_vals <- percent_NA_NDVI * 100
mp_percent_missing_vals[ mp_percent_missing_vals == 100 ] <- NA 
mp <- mapview(mp_percent_missing_vals, na.col = "transparent")

# --- Cómo rellenar los espacios faltantes de información?

# Interpolar

# --- PRELIM

NDVI_QA_rTp <- spRast_valueCoords(NDVI_QA)

plot( subset(NDVI_QA, 150) )

xy <- locator()

XY <- get_timeSeries_byClicking( c( xy$x, xy$y ),
                                 df = NDVI_QA_rTp$coords )

pixel <- NDVI_QA_rTp$values[XY$coord, ]

plot(pixel, type = "l")

pixel_interpol <- na_interpolation(pixel)

plot(pixel_interpol, type = "l")

# --- Aplicando esta idea a todos los pixels
# --- COMPUTO en PARALELO

dirToSaveProgressReport <- here( "RData", "progressReports", "mohinora" )
dir.create( dirToSaveProgressReport, recursive = TRUE )

progressReportFile <- paste0( dirToSaveProgressReport, "/progress_temp_interpol.txt" )
file.create(path=progressReportFile, showWarnings=FALSE)

write("===TEMPORAL GAPFILLING began at===",
      file=progressReportFile, append=TRUE)
write(as.character(Sys.time()[1]), file=progressReportFile,
      append=TRUE)

numCores <- detectCores()

kluster <- parallel::makeCluster(numCores-1, outfile="")
registerDoParallel(kluster)

output <- foreach(i=1:nrow(NDVI_QA_rTp$values), .combine="rbind",
                  .packages="imputeTS") %dopar% { 
                    
                    pixel <- NDVI_QA_rTp$values[i,]
                    
                    out_linear <- pixel
                    
                    if( sum( !is.na(pixel) ) > 2 ){
                      out_linear <- na_interpolation(pixel) 
                    }
                    
                    s <- c(as.numeric(out_linear))
                    
                    if(i %% 100 ==0){
                      texto <- paste0("Working on ROW: ", i)
                      write(texto, file=progressReportFile, append=TRUE)
                    }
                    
                    return(s)
                  }
stopCluster(kluster)

write("===TEMPORAL GAPFILLING ended at===",
      file=progressReportFile, append=TRUE)
write(as.character(Sys.time()[1]), file=progressReportFile,
      append=TRUE)

str(output)

# --- Rasterizando "output"

PROJECTION <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs"

whereToSaveInterpol <- here( "data", "mohinora", "250m_16_days_NDVI_interpol" )
dir.create( whereToSaveInterpol )

for(i in 1:length(archivos_NDVI)){
  
  if( i %% 100 == 0){
    cat("Working on layer ", i, "\n")
  }
  
  mat <- cbind(NDVI_QA_rTp$coords, output[,i])
  
  interpolatedLayer <- matrixToRaster(matrix=mat, projection=PROJECTION) 
  
  NAME <- strsplit( basename( archivos_NDVI[i] ), ".tif" )[[1]][1]
  
  raster::writeRaster(x=interpolatedLayer,
                      filename = paste0(whereToSaveInterpol, "/", NAME, "_interpol.tif"),
                      format="GTiff", datatype="INT2S", overwrite=TRUE)
  
}

# --- Comprobación de resultados

directorios <- list.dirs( path = here("data") )

archivos_NDVI_interpol <- list.files( path = directorios[9],
                                      pattern = ".tif",
                                      full.names = TRUE )

NDVI_interpol <- rast(archivos_NDVI_interpol)

par(mfrow=c(1,2))

plot(subset(NDVI_QA, 150), main = "Capa 150 NDVI_QA (sin interpolar)")
plot(subset(NDVI_interpol, 150))


# system.time({
#   NDVI_QA_interpol <- app(NDVI_QA, na_interpolation)
# })

system.time({
  percent_NDVI_QA_interpol <- app(NDVI_interpol, percent_na_at_pixel)
})

plot(percent_NDVI_QA_interpol)

mp_percent_missing_vals_interpol <- percent_NDVI_QA_interpol * 100
mp_percent_missing_vals_interpol[ mp_percent_missing_vals_interpol == 100 ] <- NA 
mp_interpol <- mapview( mp_percent_missing_vals_interpol, 
                        na.col = "transparent")

mp + mp_interpol

# --- igapfill














































