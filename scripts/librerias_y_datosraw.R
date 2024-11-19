#Carga librerias
library(readxl)
library(tidyr)
library(dplyr)
library(gt)
library(ggplot2)
library(lubridate)

#Carga base Guardia Adultos y Pediatria Consolidado raw, con Fecha,SE,Rango etario
#Carga base de camas disponibles por servicio
base <- read_excel("baseraw/Consolidado Guardias Intranet.xlsx")
base_mc <- read_excel("baseraw/QueryCamasServicios.xlsx")

