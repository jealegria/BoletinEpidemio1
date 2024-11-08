#Carga librerias
library(readxl)
library(tidyr)
library(dplyr)
library(gt)
library(ggplot2)
library(lubridate)

#Carga base Guardia Adultos y Pediatria Consolidado raw, con Fecha,SE,Rango etario
base <- read_excel("baseraw/Consolidado Guardias Intranet.xlsx")

#Cambiar columnas problematicas a sus formatos
base <- base %>%
  mutate(
    Edad2 = as.numeric(Edad2),  # Convertir Edad2 a numérico, los valores no numéricos se convertirán en NA
    Unidad2 = as.character(Unidad2)  # Convertir Unidad2 a tipo texto (carácter)
  )

str(base)

(warnings())
