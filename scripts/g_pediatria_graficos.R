# Crear el gráfico de línea para Pediatría (todo el año hasta el mes anterior)
base_servicio_pediatria_anio <- base %>%
  filter(Servicio == "Pediatria", 
         year(Fecha) == year(fecha_actual), 
         Fecha < primer_dia_mes_anterior)  # Filtramos hasta el mes anterior

grafico_linea_p <- base_servicio_pediatria_anio %>%
  filter(`Tipo de egreso` %in% c("Derivación", "Internación")) %>%
  group_by(Semana, `Tipo de egreso`) %>%
  summarise(Cantidad_egresos = n(), .groups = "drop") %>%
  ggplot(aes(x = Semana, y = Cantidad_egresos, color = `Tipo de egreso`, group = `Tipo de egreso`)) +
  geom_line(size = 1) +
  labs(title = "Egresos por Semana Epidemiológica (Pediatría)",
       x = "Semana Epidemiológica",
       y = "Cantidad de Egresos",
       color = "Tipo de Egreso") +
  scale_x_continuous(breaks = seq(min(base_servicio_pediatria_anio$Semana), 
                                  max(base_servicio_pediatria_anio$Semana), 
                                  by = 1)) +
  theme_minimal()

# Mostrar el gráfico
#grafico_linea_p

# Tabla con 4 semanas del mes anterior para Pediatría
base_servicio_pediatria_mes_anterior <- base %>%
  filter(Servicio == "Pediatria", 
         Fecha >= primer_dia_mes_anterior, 
         Fecha <= ultimo_dia_mes_anterior)

tabla_egresos_mes_anterior_p <- base_servicio_pediatria_mes_anterior %>%
  filter(`Tipo de egreso` %in% c("Derivación", "Internación")) %>%
  group_by(Semana, `Tipo de egreso`) %>%
  summarise(Cantidad_egresos = n(), .groups = "drop") %>%
  pivot_wider(names_from = `Tipo de egreso`, values_from = Cantidad_egresos, values_fill = list(Cantidad_egresos = 0)) %>%
  ungroup() %>%
  mutate(Ingresos = rowSums(select(., Derivación, Internación), na.rm = TRUE))

# Crear la tabla con la librería gt para Pediatría
tabla_egresos_mes_anterior_p <- tabla_egresos_mes_anterior_p %>%
  gt() %>%
  tab_header(
    title = "Egresos del Servicio de Pediatría por Semana"
  ) %>%
  cols_label(
    Semana = "Semana",
    Derivación = "Derivación",
    Internación = "Internación",
    Ingresos = "Egresos"
  ) %>%
  tab_style(
    style = list(
      cell_borders(sides = "all", weight = px(1), color = "gray")
    ),
    locations = cells_body()
  ) %>%
  tab_style(
    style = cell_fill(color = "lightgray"),
    locations = cells_row_groups()
  ) %>%
  tab_options(
    table.width = pct(100),
    column_labels.border.top.color = "gray",
    column_labels.border.bottom.color = "gray",
    row.striping.include_table_body = TRUE
  )

# Mostrar la tabla
tabla_egresos_mes_anterior_p
