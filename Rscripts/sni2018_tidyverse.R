
# -----------------------------------------------------------------------------
# Elaborado por Inder Tecuapetla Septiembre 20, 2023
#

# En la carpeta examenFinal encontrarás el archivo Investigadores-SNI-Vigentes-2018.csv el
# cual contiene la base de datos de los miembros del Sistema Nacional de Investigadores (SNI) vigentes
# en el año 2018.

# En este script usamos algunas funciones de los paquetes
# incluidos en el "tidyverse" para responder las siguientes preguntas

# (a) (0.5 puntos) ¿Cuáles son las variables de esta base de datos? ¿Cuántos investigadores
# estaban vigentes en el SNI en 2018?
# (b) (2 puntos) ¿Cuál es el área de conocimiento con más investigadoras e investigadores? De
# esta área, ¿cuáles son las 5 instituciones con más investigadores adscritos?
# (c) (1 punto) En 2018 ¿cuántos investigadores de la UNAM pertenecían al SNI? De este grupo,
# ¿cuántos investigadores pertenecían al Área I? Finalmente, de este subgrupo ¿cuántos pertenecían al
# Nivel 3?
# (d) (0.5 puntos) Considera la sub-base de datos correspondiente a los investigadores de la
# UNAM que en 2018 pertenecían a Área I en el Nivel 3 del SNI. Ordena alfabéticamente esta sub-base
# de datos.

# --- DATASETS (en /data/examenFinal)
# --- Investigadores-SNI-Vigentes-2018.csv
# -----------------------------------------------------------------------------

# ---

library(tidyverse)

fileSNI <- paste0( getwd(), "/data/examenFinal/Investigadores-SNI-Vigentes-2018.csv" )

sni_tidy <- read_csv(fileSNI, 
                     locale = locale( encoding = "latin1" ))

# ---

str(sni_tidy)

?rename

?"|>"

sni_tidy |> 
  rename(
    apellido_paterno = `Apellido Paterno`,
    apellido_materno = `Apellido Materno`,
    institucion_adscripcion = `Institución de Adscripción`,
    area_conocimiento = `Área del Conocimiento`
  )

names(sni_tidy)

sni_tidy <- sni_tidy |> 
  rename(
    apellido_paterno = `Apellido Paterno`,
    apellido_materno = `Apellido Materno`,
    institucion_adscripcion = `Institución de Adscripción`,
    area_conocimiento = `Área del Conocimiento`
  )

names(sni_tidy)

janitor

sni_tidy |>
  janitor::clean_names()

sni_tidy <- sni_tidy |> 
  janitor::clean_names()

# (a)

sni_tidy

# (b)

count(sni_tidy, area_conocimiento, sort=TRUE)

sni_tidy_mayoria <- sni_tidy %>%
  filter(area_conocimiento == "Área V: CIENCIAS SOCIALES")

count(sni_tidy_mayoria, institucion_adscripcion, sort=TRUE)

# (c) 

sni_tidy |>
  filter( institucion_adscripcion == "UNIVERSIDAD NACIONAL AUTONOMA DE MEXICO" )


sni_tidy |>
  filter( institucion_adscripcion == "UNIVERSIDAD NACIONAL AUTONOMA DE MEXICO",
          area_conocimiento == "Área I: FISICO-MATEMÁTICAS Y CIENCIAS DE LA TIERRA" )

sni_tidy |>
  filter( institucion_adscripcion == "UNIVERSIDAD NACIONAL AUTONOMA DE MEXICO",
          area_conocimiento == "Área I: FISICO-MATEMÁTICAS Y CIENCIAS DE LA TIERRA",
          nivel == "3"
  )

sni_unam_area1_nivel3 <- sni_tidy |>
  filter( institucion_adscripcion == "UNIVERSIDAD NACIONAL AUTONOMA DE MEXICO",
          area_conocimiento == "Área I: FISICO-MATEMÁTICAS Y CIENCIAS DE LA TIERRA",
          nivel == "3"
  )

# (d)

sni_unam_area1_nivel3 |>
  arrange(apellido_paterno)

# ---

unique(sni_tidy$institucion_adscripcion)

sni_tidy |>
  filter(institucion_adscripcion == "EL COLEGIO DE MICHOACAN, A.C.")










