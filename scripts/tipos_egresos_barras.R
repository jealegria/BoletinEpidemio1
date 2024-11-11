# Obtener la fecha actual
fecha_actual <- Sys.Date()

# Determinar el primer día del mes anterior
primer_dia_mes_anterior <- as.Date(paste(year(fecha_actual), month(fecha_actual) - 1, "01", sep = "-"))

# Filtrar los datos del mes anterior
base_mes_anterior <- base %>%
  filter(Fecha >= primer_dia_mes_anterior & Fecha < primer_dia_mes_anterior %m+% months(1))

# Contar la cantidad total de egresos en el mes anterior
total_egresos <- nrow(base_mes_anterior)

# Contar los egresos por tipo (Alta médica y Derivación) por semana epidemiológica
egresos_tipo_semana <- base_mes_anterior %>%
  filter(`Tipo de egreso` %in% c("Derivación", "Internación")) %>%
  group_by(Semana, `Tipo de egreso`) %>%
  summarise(Cantidad_egresos = n(), .groups = "drop")

# Ver los resultados
#total_egresos

#egresos_tipo_semana

barras_tipo_egresos <- ggplot(egresos_tipo_semana, aes(x = as.factor(Semana), y = Cantidad_egresos, fill = `Tipo de egreso`)) +
  geom_bar(stat = "identity", position = "dodge") +  # "dodge" pone las barras lado a lado
  labs(title = "Cantidad de Egresos por Semana Epidemiológica y Tipo de Egreso",
       x = "Semana Epidemiológica",
       y = "Cantidad de Egresos",
       fill = "Tipo de Egreso") +
  theme_minimal()


# Determinar el mes evaluado
mes_evaluado <- format(primer_dia_mes_anterior, "%B")  # Nombre completo del mes (Octubre, etc.)

# Crear el texto con el total de egresos
texto_informe <- paste("En el Servicio de Emergencias del HPN en el mes de ", mes_evaluado, 
                       " hubieron ", total_egresos, " egresos", sep = "")

# Ver el texto generado
#texto_informe



