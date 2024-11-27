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


################################################




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
      palette = c("green", "yellow", "red"),
      domain = NULL
    )
  )

# Mostrar la tabla
tabla_porcentajes

