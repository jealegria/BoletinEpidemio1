# Definir el año actual y los tres años anteriores
año_actual <- year(Sys.Date())
años_incluidos <- (año_actual - 3):año_actual

# Filtrar el dataset para incluir solo los años seleccionados hasta el mes anterior al actual en el año actual
mes_actual <- month(Sys.Date())
base_filtrada <- base %>%
  filter((year(Fecha) == año_actual & month(Fecha) < mes_actual) |
           year(Fecha) %in% (año_actual - 3):(año_actual - 1))

# Agrupar por año y mes, y calcular la cantidad de ingresos por mes
ingresos_por_mes2 <- base_filtrada %>%
  mutate(Año = format(Fecha, "%Y"),
         Mes = format(Fecha, "%m")) %>%
  group_by(Año, Mes) %>%
  summarise(Cantidad_ingresos = n(), .groups = "drop") %>%
  ungroup() %>%
  mutate(Mes_num = as.numeric(Mes))  # Convierte el mes en numérico para graficar

# Crear la variable del promedio de ingresos de los tres años anteriores
promedio_ingresos_3años <- ingresos_por_mes2 %>%
  filter(Año %in% as.character((año_actual - 3):(año_actual - 1))) %>%
  group_by(Mes) %>%
  summarise(Promedio_ingresos = mean(Cantidad_ingresos), .groups = "drop") %>%
  ungroup()

# Añadir una columna "Año" ficticia para la línea de tendencia
promedio_ingresos_3años$Año <- "Promedio 3 años anteriores"

# Definir colores para cada año
colores <- setNames(
  c("#FD77F4", "#77FD78", "#FF000080", "#0000FF80"),
  c(as.character(año_actual - 3), as.character(año_actual - 2),
    as.character(año_actual - 1), as.character(año_actual))
)

# Generar el gráfico comparativo
linea_ingresos <- ggplot(ingresos_por_mes2, aes(x = Mes_num, y = Cantidad_ingresos, color = Año, group = Año)) +
  geom_line(size = 1) +
  # Añadir la línea de tendencia como el promedio de los 3 años anteriores
  geom_smooth(
    data = promedio_ingresos_3años,
    aes(x = as.numeric(Mes), y = Promedio_ingresos),
    method = "loess",
    se = TRUE,
    color = "gray70",    # Color de la línea de tendencia
    linetype = "dashed", # Línea discontinua
    size = 1             # Tamaño de la línea
  ) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) + # Etiquetas de mes abreviadas
  scale_color_manual(values = colores) +  # Aplicar colores personalizados para cada año
  labs(title = "Comparación Mensual de Ingresos entre los Últimos 4 Años",
       x = "Mes",
       y = "Cantidad de Ingresos",
       color = "Año") +
  theme_minimal() +
  theme(panel.grid = element_blank())  # Eliminar cuadrícula de fondo

# Mostrar gráfico
linea_ingresos



