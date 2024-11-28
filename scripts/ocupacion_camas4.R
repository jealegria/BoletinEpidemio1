# Filtrar el mes anterior
base_mc_filtro <- base_mc %>%
  filter(month(Fecha) == month(Sys.Date()) - 1, year(Fecha) == year(Sys.Date()))

# Calcular promedios semanales utilizando la columna "Semana"
promedio_semanal <- base_mc_filtro %>%
  group_by(Servicio, Semana) %>% # Usar la columna "Semana" directamente
  summarise(
    promedio_camas = mean(Camas, na.rm = TRUE),
    promedio_pacientes = mean(`Existencia a las 0`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    porcentaje_ocupacion = if_else(
      promedio_camas > 0, 
      (promedio_pacientes / promedio_camas) * 100, 
      0
    )
  )

# Tabla de valores absolutos
tabla_valores_absolutos <- promedio_semanal %>%
  group_by(Servicio) %>%
  summarise(
    promedio_camas = round(mean(promedio_camas, na.rm = TRUE), 0),
    .groups = "drop"
  ) %>%
  mutate(
    promedio_camas = if_else(promedio_camas == 0 | is.na(promedio_camas), "-", as.character(promedio_camas))
  ) %>%
  left_join(
    promedio_semanal %>%
      select(Servicio, Semana, promedio_pacientes) %>%
      mutate(
        promedio_pacientes = round(promedio_pacientes, 0)
      ) %>%
      pivot_wider(names_from = Semana, values_from = promedio_pacientes, names_prefix = "Semana "),
    by = "Servicio"
  ) %>%
  gt(rowname_col = "Servicio") %>%
  cols_label(promedio_camas = "Camas") %>%
  tab_spanner(
    label = "Promedio de pacientes por semana",
    columns = starts_with("Semana ")
  ) %>%
  cols_align(align = "center", columns = everything()) %>%
  tab_options(
    table.font.size = "small",
    data_row.padding = px(5),
    heading.align = "center"
  )

# Mostrar la tabla
tabla_valores_absolutos


#################




# Tabla de porcentajes con colores
tabla_porcentajes <- promedio_semanal %>%
  group_by(Servicio) %>%
  summarise(
    promedio_camas = round(mean(promedio_camas, na.rm = TRUE), 0),
    .groups = "drop"
  ) %>%
  mutate(
    promedio_camas = if_else(promedio_camas == 0 | is.na(promedio_camas), "-", as.character(promedio_camas))
  ) %>%
  left_join(
    promedio_semanal %>%
      select(Servicio, Semana, porcentaje_ocupacion) %>%
      mutate(
        porcentaje_ocupacion = round(porcentaje_ocupacion, 0) # Sin decimales
      ) %>%
      pivot_wider(names_from = Semana, values_from = porcentaje_ocupacion, names_prefix = "Semana "),
    by = "Servicio"
  ) %>%
  gt(rowname_col = "Servicio") %>%
  cols_label(promedio_camas = "Camas") %>%
  tab_spanner(
    label = "% de ocupación por semana",
    columns = starts_with("Semana ")
  ) %>%
  cols_align(align = "center", columns = everything()) %>%
  tab_options(
    table.font.size = "small",
    data_row.padding = px(5),
    heading.align = "center"
  ) %>%
  # Aplicar colores según el rango de porcentaje
  data_color(
    columns = starts_with("Semana "),
    colors = scales::col_bin(
      bins = c(-Inf, 64, 84, Inf),
      palette = c("#77FD78", "yellow", "#FF000080"),
      domain = NULL
    )
  )

# Mostrar la tabla
tabla_porcentajes

