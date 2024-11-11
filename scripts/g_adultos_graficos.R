# 1. Fecha del mes anterior====

# Obtener la fecha actual
fecha_actual <- Sys.Date()

# Determinar el primer día del mes anterior
primer_dia_mes_anterior <- as.Date(paste(year(fecha_actual), month(fecha_actual) - 1, "01", sep = "-"))
ultimo_dia_mes_anterior <- primer_dia_mes_anterior %m+% months(1) - days(1)


# 2. Grafico linea ====

# Creacion Grafico de linea Guardia Adultos todo el año hasta el mes anterior del actual


# 1. Gráfico de línea (todo el año hasta el mes anterior)
base_servicio_adultos_anio <- base %>%
  filter(Servicio == "Adultos", 
         year(Fecha) == year(fecha_actual), 
         Fecha < primer_dia_mes_anterior)  # Filtramos hasta el mes anterior

grafico_linea_a <- base_servicio_adultos_anio %>%
  filter(`Tipo de egreso` %in% c("Derivación", "Internación")) %>%
  group_by(Semana, `Tipo de egreso`) %>%
  summarise(Cantidad_egresos = n(), .groups = "drop") %>%
  ggplot(aes(x = Semana, y = Cantidad_egresos, color = `Tipo de egreso`, group = `Tipo de egreso`)) +
  geom_line(size = 1) +
  labs(title = "Egresos por Semana Epidemiológica (Adultos)",
       x = "Semana Epidemiológica",
       y = "Cantidad de Egresos",
       color = "Tipo de Egreso") +
  scale_x_continuous(breaks = seq(min(base_servicio_adultos_anio$Semana), 
                                  max(base_servicio_adultos_anio$Semana), 
                                  by = 1)) +  # Esto ajusta los incrementos a 1
  theme_minimal()

# Mostrar el gráfico
#grafico_linea_a

#3. Tabla con 4 semanas====

#Creacion tabla internacion/derivacion/total ingresos con 4 semanas del mes anterior


# 2. Tabla con las 4 semanas del mes anterior (solo Derivación e Internación)
base_servicio_adultos_mes_anterior <- base %>%
  filter(Servicio == "Adultos", 
         Fecha >= primer_dia_mes_anterior, 
         Fecha <= ultimo_dia_mes_anterior)

tabla_egresos_mes_anterior <- base_servicio_adultos_mes_anterior %>%
  filter(`Tipo de egreso` %in% c("Derivación", "Internación")) %>%
  group_by(Semana, `Tipo de egreso`) %>%
  summarise(Cantidad_egresos = n(), .groups = "drop") %>%
  pivot_wider(names_from = `Tipo de egreso`, values_from = Cantidad_egresos, values_fill = list(Cantidad_egresos = 0)) %>%
  ungroup() %>%
  mutate(Ingresos = rowSums(select(., Derivación, Internación), na.rm = TRUE))

# Mostrar la tabla
#print(tabla_egresos_mes_anterior)

# Crear la tabla con la librería gt
tabla_egresos_mes_anterior_a <- tabla_egresos_mes_anterior %>%
  gt() %>%
  tab_header(
    title = "Egresos del Servicio de Adultos por Semana"
  ) %>%
  cols_label(
    Semana = "Semana",
    Derivación = "Derivación",
    Internación = "Internación"
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
#print(tabla_egresos_mes_anterior_gt)







