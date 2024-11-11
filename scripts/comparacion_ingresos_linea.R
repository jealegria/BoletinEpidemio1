
# Extrae el mes y crea una columna para el mes sin el año
ingresos_por_mes2 <- base %>%
  mutate(Año = format(Fecha, "%Y"),
         Mes = format(Fecha, "%m")) %>%
  group_by(Año, Mes) %>%
  summarise(Cantidad_ingresos = n()) %>%
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



