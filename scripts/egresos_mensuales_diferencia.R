# Cargar las librerías necesarias
library(dplyr)
library(lubridate)
library(gt)

# Definir el año actual y el mes anterior
año_actual <- year(Sys.Date())
mes_anterior <- month(Sys.Date()) - 1

# 1. Dividir el dataset base según el Servicio
base_p1 <- base %>% filter(Servicio == "Pediatria")
base_a1 <- base %>% filter(Servicio == "Adultos")

# 2. Contar egresos por mes y organizar por año
# Para Pediatria
egresos_p1 <- base_p1 %>%
  mutate(Año = year(Fecha), Mes = month(Fecha, label = TRUE)) %>%
  filter((Año == año_actual & month(Fecha) <= mes_anterior) | Año == (año_actual - 1)) %>%
  group_by(Año, Mes) %>%
  summarise(Cantidad_Egresos = n()) %>%
  ungroup()

# Para Adultos
egresos_a1 <- base_a1 %>%
  mutate(Año = year(Fecha), Mes = month(Fecha, label = TRUE)) %>%
  filter((Año == año_actual & month(Fecha) <= mes_anterior) | Año == (año_actual - 1)) %>%
  group_by(Año, Mes) %>%
  summarise(Cantidad_Egresos = n()) %>%
  ungroup()

# Función para calcular la diferencia y reorganizar en formato horizontal
calcular_tabla_horizontal <- function(egresos_data) {
  # Calcular la diferencia y el cambio porcentual
  egresos_diff <- egresos_data %>%
    pivot_wider(names_from = Año, values_from = Cantidad_Egresos, values_fill = 0) %>%
    mutate(
      Diferencia = ifelse(
        get(as.character(año_actual)) == 0, NA,
        get(as.character(año_actual)) - get(as.character(año_actual - 1))
      ),
      Cambio_Porcentual = ifelse(
        get(as.character(año_actual)) == 0 | get(as.character(año_actual - 1)) == 0, NA,
        (Diferencia / get(as.character(año_actual - 1))) * 100
      )
    )
  
  # Convertir a formato horizontal con los años y diferencias en las filas
  tabla_horizontal <- egresos_diff %>%
    pivot_longer(cols = c(as.character(año_actual - 1), as.character(año_actual), Diferencia, Cambio_Porcentual), 
                 names_to = "Año", values_to = "Cantidad_Egresos") %>%
    pivot_wider(names_from = Mes, values_from = Cantidad_Egresos) %>%
    mutate(Año = recode(Año, 
                        `Diferencia` = "Diferencia", 
                        `Cambio_Porcentual` = "%"))

  return(tabla_horizontal)
}

# Generar tablas horizontales para Pediatria y Adultos
tabla_horizontal_p1 <- calcular_tabla_horizontal(egresos_p1)
tabla_horizontal_a1 <- calcular_tabla_horizontal(egresos_a1)

# Crear las tablas gt en formato horizontal
tabla_p1_final <- tabla_horizontal_p1 %>%
  gt(rowname_col = "Año") %>%
  tab_header(title = "Egresos de Pediatria por Mes y Año con Diferencias") %>%
  fmt_number(columns = everything(), decimals = 0) %>%  # Sin decimales para todas las columnas
  fmt_number(columns = starts_with("Cambio"), decimals = 2) %>% # Dos decimales solo para Cambio %
  fmt_missing(columns = everything(), missing_text = "-")

tabla_a1_final <- tabla_horizontal_a1 %>%
  gt(rowname_col = "Año") %>%
  tab_header(title = "Egresos de Adultos por Mes y Año con Diferencias") %>%
  fmt_number(columns = everything(), decimals = 0) %>%
  fmt_number(columns = starts_with("Cambio"), decimals = 2) %>%
  fmt_missing(columns = everything(), missing_text = "-")

# Mostrar las tablas en tu reporte
#tabla_p1_final
#tabla_a1_final
