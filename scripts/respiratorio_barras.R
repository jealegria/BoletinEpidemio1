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



##################### Grafico

# Crear la columna Orden_Servicio con valores numéricos según el orden deseado
conteo_dx$Orden_Servicio <- factor(conteo_dx$Servicio, 
                                   levels = c("Clinica", "UTIA", "Pediatría", "UTIP", "Neonatología"), 
                                   labels = c(1, 2, 3, 4, 5))

# Asegurarse de que la columna 'Orden_Servicio' sea un factor ordenado
conteo_dx$Orden_Servicio <- as.numeric(conteo_dx$Orden_Servicio)

# Crear el gráfico de barras apiladas con los nombres de las variables modificados
resp_barras1 <- ggplot(conteo_dx, aes(x = Semana, y = Cantidad, fill = Dx)) +
  geom_bar(stat = "identity", position = "stack") +  # Barras apiladas
  facet_wrap(~ Servicio, nrow = 1, scales = "free_x", 
             labeller = labeller(Servicio = function(x) factor(x, levels = c("Clinica", "UTIA", "Pediatría", "UTIP", "Neonatología")))) +  # Orden por número
  scale_fill_manual(values = c("NR" = "#77FD78", "R" = "#0000FF80", "R - IRAG" = "#FF000080"),
                    labels = c("NR" = "No Respiratoria", "R" = "Respiratoria", "R - IRAG" = "IRAG")) +  # Modificar nombres de las variables
  labs(title = "Distribución de diagnósticos por semanas y servicio (Barras Apiladas)",
       x = "Semana",
       y = "Cantidad",
       fill = "Diagnóstico") +
  theme_minimal() +
  theme(panel.spacing = unit(1, "lines"),  # Espaciado entre servicios
        strip.text = element_text(face = "bold"),  # Títulos destacados para servicios
        panel.grid = element_blank())  # Elimina las líneas del grid

#resp_barras1


