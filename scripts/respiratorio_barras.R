# Determinar el mes y año actuales
fecha_actual <- Sys.Date()
mes_anterior <- floor_date(fecha_actual, "month") - months(1)

# Filtrar las semanas del mes anterior
base_rp_mes_anterior <- base_rp %>%
  filter(month(Fecha) == month(mes_anterior) & year(Fecha) == year(mes_anterior)) %>%
  mutate(Semana = isoweek(Fecha))

conteo_dx <- base_rp_mes_anterior %>%
  group_by(Semana, Dx, Servicio) %>%
  summarise(Cantidad = n(), .groups = "drop")



library(ggplot2)

ggplot(conteo_dx, aes(x = Semana, y = Cantidad, fill = Dx)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Servicio, nrow = 1, scales = "free_x") +
  scale_fill_manual(values = c("NR" = "green", "R" = "blue", "R - IRAG" = "red")) +  
  labs(title = "Distribución de diagnósticos por semanas y servicio",
       x = "Semana",
       y = "Cantidad",
       fill = "Diagnóstico") +
  theme_minimal() +
  theme(panel.spacing = unit(1, "lines"),  # Espaciado entre servicios
        strip.text = element_text(face = "bold"),  # Títulos destacados para servicios
        panel.grid = element_blank())  # Elimina las líneas del grid

