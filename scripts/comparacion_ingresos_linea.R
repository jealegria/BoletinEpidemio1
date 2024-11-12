library(dplyr)
library(ggplot2)
library(lubridate)

# Definir el año actual y el año anterior
año_actual <- year(Sys.Date())
año_anterior <- año_actual - 1

# Filtrar el dataset para incluir solo el año anterior y el año actual, hasta el mes anterior al actual
mes_actual <- month(Sys.Date())
base_filtrada <- base %>%
  filter((year(Fecha) == año_actual & month(Fecha) < mes_actual) |
           year(Fecha) == año_anterior)

# Extrae el mes y crea una columna para el mes sin el año
ingresos_por_mes2 <- base_filtrada %>%
  mutate(Año = format(Fecha, "%Y"),
         Mes = format(Fecha, "%m")) %>%
  group_by(Año, Mes) %>%
  summarise(Cantidad_ingresos = n(), .groups = "drop") %>%
  ungroup() %>%
  mutate(Mes_num = as.numeric(Mes))  # Convierte el mes en numérico para graficar

# Genera el gráfico comparativo
linea_ingresos <- ggplot(ingresos_por_mes2, aes(x = Mes_num, y = Cantidad_ingresos, color = as.factor(Año), group = Año)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) + # Etiquetas de mes abreviadas
  labs(title = "Comparación Mensual de Ingresos entre 2023 y 2024",
       x = "Mes",
       y = "Cantidad de Ingresos",
       color = "Año") +
  theme_minimal()

# Mostrar gráfico
#linea_ingresos


