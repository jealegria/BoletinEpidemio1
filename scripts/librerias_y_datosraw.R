#Carga librerias
library(readxl)
library(tidyr)
library(dplyr)
library(gt)
library(ggplot2)
library(lubridate)

#Carga base Guardia Adultos y Pediatria Consolidado raw, con Fecha,SE,Rango etario
base <- read_excel("baseraw/Consolidado Guardias Intranet.xlsx")

