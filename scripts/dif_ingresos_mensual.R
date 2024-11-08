# Vector de meses en español para el orden correcto
meses_orden <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                 "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

# Paso 1: Agrupar y calcular ingresos por año y mes
ingresos_por_mes <- base %>%
  group_by(Año, Mes) %>%
  summarise(Cantidad_ingresos = n()) %>%
  ungroup()

# Paso 2: Crear dos columnas separadas para los años 2023 y 2024
comparacion_mensual <- ingresos_por_mes %>%
  filter(Año %in% c(2023, 2024)) %>%
  pivot_wider(names_from = Año, values_from = Cantidad_ingresos, 
              names_prefix = "Cantidad_ingresos_") %>%
  mutate(
    Diferencia = Cantidad_ingresos_2024 - Cantidad_ingresos_2023,
    Porcentaje_cambio = (Diferencia / Cantidad_ingresos_2023) * 100
  ) %>%
  mutate(Mes = factor(Mes, levels = meses_orden)) %>%  # Usar niveles en español
  arrange(Mes)









# Paso 1: Redondear y agregar el símbolo de porcentaje
comparacion_mensual <- comparacion_mensual %>%
  mutate(
    Porcentaje_cambio = round(Porcentaje_cambio, 0),  # Redondear a 0 decimales
    Porcentaje_cambio = paste0(Porcentaje_cambio, "%")  # Añadir el símbolo de porcentaje
  )

# Paso 2: Crear la tabla con `gt`
tabla_resultado_mensual <- comparacion_mensual %>%
  gt() %>%
  tab_header(
    title = "Comparación de Ingresos entre 2024 y 2023 por Mes"
  ) %>%
  cols_label(
    Mes = "Mes",
    Cantidad_ingresos_2024 = "2024",
    Cantidad_ingresos_2023 = "2023",
    Diferencia = "Diferencia",
    Porcentaje_cambio = "Cambio (%)"
  ) %>%
  tab_spanner(
    label = "Ingresos", 
    columns = c("Cantidad_ingresos_2024", "Cantidad_ingresos_2023")
  ) %>%
  tab_spanner(
    label = "Diferencia y Cambio",  
    columns = c("Diferencia", "Porcentaje_cambio")
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#f0f0f0"),  # Color de fondo gris claro para todas las celdas
      cell_text(weight = "bold")  # Poner en negrita el texto
    ),
    locations = cells_body(columns = everything())
  ) %>%
  cols_align(
    align = "center", 
    columns = c("Mes", "Cantidad_ingresos_2024", "Cantidad_ingresos_2023", "Diferencia", "Porcentaje_cambio")
  ) %>%
  # Establecer color verde claro para valores negativos y rojo claro para valores positivos
  tab_style(
    style = list(
      cell_text(color = "green")  # Color verde claro para los números negativos
    ),
    locations = cells_body(
      columns = "Porcentaje_cambio",
      rows = Porcentaje_cambio < 0  # Filtrar solo los negativos
    )
  ) %>%
  tab_style(
    style = list(
      cell_text(color = "red")  # Color rojo claro para los números positivos
    ),
    locations = cells_body(
      columns = "Porcentaje_cambio",
      rows = Porcentaje_cambio > 0  # Filtrar solo los positivos
    )
  )

# Mostrar la tabla formateada
tabla_resultado_mensual
