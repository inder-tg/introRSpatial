
# -----------------------------------------------------------------------------
#
# Elaborado por Inder Tecuapetla Septiembre 6, 2023
#
# En este script damos una breve introducción al lenguaje de programación
# estadística R con aplicaciones básicas para explorar datos geo-referenciados
# -----------------------------------------------------------------------------

# ----------------
# CONOCE RStudio #
# ----------------

# -----------
# PREAMBULO #
# -----------

# --- Instalando todos los paquetes a utilizar en este script

library(raster)
library(terra)
library(mapview)
library(geoTS)
library(RColorBrewer)

# ------------------------------------------------------
# Objetos en R: vector, matrix, dataframe, array, list #
# ------------------------------------------------------

Prec <- c(15, 40, 37, 0, 0, 0, 0, 0, 7, 3, 77)
meses <- c("Enero", "Febrero", "Marzo", "Abril")
numeros <- 1:40

# -------------------------------------------
# LISTA: un objeto para contenerlos a todos #
# -------------------------------------------

LISTA <- list(Prec, numeros, "listas")

LISTA; lista

# ---------------------------------
# Funciones, documentación, Help! #
# ---------------------------------

help(c)
?c

class(meses); length(meses); str(meses)

str(numeros)

getwd()

?paste0

# -------------------------------
# Lectura de archivos TIF y SHP #
# -------------------------------

# --- Warm up! Mohinora

dirMOHINORA <- paste0( getwd(), "/data/mohinora" )

# --- completar esta parte manualmente
mohinoraFILE <- paste0( dirMOHINORA, "/MOD13Q1.006._250m_16_days_NDVI.linear.tif" ) 

mohinora_stack <- stack( mohinoraFILE )

mohinora_stack

plot(subset(mohinora_stack, 10))

mp_mohinora <- mapview(subset(mohinora_stack, 10))

SHP_anp <- list.files( path = paste0( getwd(), "/data/anp_2021" ),
                       pattern = ".shp", 
                       full.names = TRUE)

shp_anp <- shapefile( SHP_anp[1] )

# ------------------------------
# --- cambio de proyección --- #
# ------------------------------

plot( subset(mohinora_stack, 100) )

plot( shp_anp[165,] )

crs(subset(mohinora_stack, 100))

crs(shp_anp)

plot( subset(mohinora_stack, 100) )

plot( shp_anp[165,], add=TRUE )

# crs shp original es distinto a crs de stack_NDVI_Mohinora
# spTranform ayuda a realizar la reproyección
# crsTEST <- crs(stack_NDVI_Mohinora)
shp_anp_sinu <- spTransform(shp_anp, crs(mohinora_stack))

plot( subset(mohinora_stack, 100) )
plot( shp_anp_sinu[165,], add=TRUE, lwd=4)

# ¿Cómo encontrar el polígono que corresponde a Cerro Mohinora?

str(shp_anp) # demasiada info

str(shp_anp@data)

shp_anp@data$NOMBRE

shp_anp@data$NOMBRE[165]

shp_anp_sinu[165,]

mp_shp_mohinora <- mapview(shp_anp_sinu[165,])

mp_mohinora + mp_shp_mohinora

# ---

DIRS <- list.dirs(path = getwd(), recursive = FALSE)

# --- OJO: checar si 2 es correcto después de la clonación del repo
data_DIR <- dir(path=DIRS[2], full.names = TRUE)

# --- TIF

Landsat_FILES <- list.files( path = data_DIR[2],
                             pattern = ".tif",
                             full.names = TRUE )

b3 <- rast(Landsat_FILES[1])

Landsat7_stack <- stack(Landsat_FILES)

# --- SHP

shp_FILES <- list.files(path = data_DIR[4],
                        pattern = ".shp",
                        full.names = TRUE)

SHP_LP <- shapefile( shp_FILES[1] )

# -------------------------------------------------
# Funciones útiles para el manejo y visualización #
# de objetos georeferenciados                     #
# -------------------------------------------------

# --- PLOTS

plot(b3)

plot(SHP_LP)

plot(b3)
plot(SHP_LP, add=TRUE, border="blue")

plot(Landsat7_stack)

mp_b3 <- mapview(b3)

# --- MANEJO

b3_SHP <- raster_intersect_sp(x=b3, y=SHP_LP)

plot(b3_SHP)

stack_SHP <- raster_intersect_sp(x=Landsat7_stack, y=SHP_LP)

plot(stack_SHP)

# --- EXTRACCION

b3_SHP_rTp <- rasterToPoints(b3_SHP) 

nrow(b3_SHP_rTp) < nrow(b3_SHP) * ncol(b3_SHP)

# --- EXPORTACION

NAME <- strsplit(x=data_DIR[1], split="/")

NOMBRE <- unlist(NAME[[1]])[length(NAME[[1]])]

# --- Definir /TIF antes de ejecutar siguiente linea
writeRaster(stack_SHP, 
            filename = paste0( getwd(), "/TIF/", NOMBRE, "_stack_LP" ),
            datatype = "INT2S",
            format = "GTiff")

# save(b3_SHP_rTp, 
#      file = paste0( getwd(), "/RData/", names(b3), "_LP_points.RData" ))

# --- 

# -------------------------
# Definición de funciones #
# -------------------------

# --- NDVI = (NIR - RED) / (NIR + RED)
# En el objeto "stack_SHP", NIR = 2, RED = 1

ndvi <- (stack_SHP[[2]] - stack_SHP[[1]])/(stack_SHP[[2]] + stack_SHP[[1]])

plot(ndvi)

ndvi_subset <- (subset(stack_SHP,2) - subset(stack_SHP,1)) / (subset(stack_SHP,2) + subset(stack_SHP,1))

compareRaster(ndvi, ndvi_subset)

# SINTAXIS
# nameFunction <- function(parametros) {
#   comandos que dependen de parametros
# salida
# }

# Esta función calcula el NDVI utilizando como parámetros "imagen" y el número
# de dos bandas, a saber "k" e "i"
spectralIndex <- function(stack, i, j){
  band_i <- stack[[i]] # get i-th band from stack
  band_j <- stack[[j]] # get j-th band from stack
  index <- (band_i - band_j)/(band_i + band_j)
  index
}

# En el objeto "stack_SHP", NIR = 2, RED = 1
ndvi_FUN <- spectralIndex(stack = stack_SHP, i = 2, j = 1)
plot(ndvi_FUN, col = rev(terrain.colors(10)), main = "Landsat-NDVI")

# Ejercicio 1. Si NBR = (NIR - SWIR) / (NIR + SWIR) y en el objeto
# "stack_SHP", NIR = 2, y SWIR = 3, utiliza la función spectralIndex
# para calcular el NBR de la escena LE70290462004033EDC01

display.brewer.all()
myGreens <- brewer.pal(9, "Greens")

plot(ndvi_FUN, col = myGreens, main = "Landsat-NDVI")
# -----------------------------------------------------------------------------

# ----------------------------
# Aritmetica sobre un ráster #
# ----------------------------

# histogram of ndvi
hist(ndvi_FUN)

hist(ndvi_FUN,
     main = "NDVI's distribution",
     xlab = "NDVI",
     ylab="Frequency",
     col = "wheat",
     xlim = c(-0.5, 1),
     breaks = 30,
     xaxt = 'n')
axis(side = 1, 
     at = seq(-0.05, 1, 0.05), 
     labels = seq(-0.05, 1, 0.05))

# Ejercicio 2. Haz un histograma del NBR que calculaste en el Ejercicio 1

# --------------
# Thresholding #
# --------------

# Plot the vegetation cover
# Assuming that values of ndvi above 0.4 represent "healthy" vegetation.

ndvi_healthy_veg <- ndvi_FUN
ndvi_healthy_veg[ ndvi_healthy_veg < 0.4 ] <- NA

plot(ndvi_healthy_veg, main = "Vegetation cover")

par(mar = c(4.5, 5, 1.5, 2))
plot(testVeg, main = "Vegetation cover (basic arithmetic)")

calc_veg <- calc(ndvi_FUN, function(x){ x[x < 0.4] <- NA; x})
plot(cal_veg, main = "Vegetation cover (calc)")

compareRaster(calc_veg, ndvi_healthy_veg)

# The histogram of ndvi_FUN has a "peak" around the interval (0.6, 0.65)
# Let's try to pin point that region

peakVeg <- ndvi_FUN
peakVeg[ndvi_FUN > -Inf & ndvi_FUN < 0.6] <- NA
peakVeg[ndvi_FUN >= 0.6 & ndvi_FUN <= 0.65] <- 1
peakVeg[ndvi_FUN > 0.65 & ndvi_FUN < Inf] <- NA

plot(peakVeg, main="Highest frequency of NDVI's values")

peak_recla <- reclassify(ndvi, c(-Inf, 0.6, NA, 0.6, 0.65, 1, 0.65, Inf, NA))
plot(peak_recla, main = "Qué es esto?")

compareRaster(peak_recla, peakVeg)

# # Overlapping land and landsatRGB
# plotRGB(landsatRGB, r = 1, g = 2, b = 3, axes = TRUE, stretch = "lin",
#         main = "Landsat False Color Composite")
# plot(land, add = TRUE, legend = FALSE)
# 
# # creating classes for different values of NDVI
# 
# vegc <- reclassify(veg, c(-Inf, 0.25, 1, 0.25, 0.3, 2, 0.3, 0.4, 3, 0.4, 0.5,
#                           4, 0.5, Inf, 5))
# plot(vegc, col = rev(terrain.colors(4)), main = 'NDVI based thresholding')

# ---

usv_shp_FILES <- list.files(path = data_DIR[3],
                            pattern = ".shp",
                            full.names = TRUE)

usv_SHP_LP <- shapefile( usv_shp_FILES[1] )

plot(usv_SHP_LP, border="blue")

plot(b3_SHP)
plot(usv_SHP_LP, add=TRUE, border="blue")

usv_SHP_LP_utm <- spTransform(x = usv_SHP_LP, CRSobj = crs(b3_SHP))

plot(b3_SHP)
plot(usv_SHP_LP_utm, add=TRUE, border="blue")

