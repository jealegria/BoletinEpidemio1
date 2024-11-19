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

# Crear la tabla gt ajustada
tabla_ocupacion <- promedio_semanal %>%
  group_by(Servicio) %>%
  summarise(
    promedio_camas = round(mean(promedio_camas, na.rm = TRUE), 0), # Promedio de camas por servicio
    .groups = "drop"
  ) %>%
  mutate(
    promedio_camas = if_else(promedio_camas == 0 | is.na(promedio_camas), "-", as.character(promedio_camas)) # Reemplazar 0 o NA con "-"
  ) %>%
  left_join(
    promedio_semanal %>%
      mutate(
        resumen_columna = paste0(
          round(ifelse(is.na(promedio_pacientes), 0, promedio_pacientes), 0), " pacientes | ",
          round(ifelse(is.na(porcentaje_ocupacion), 0, porcentaje_ocupacion), 0), "%"
        )
      ) %>%
      select(Servicio, Semana, resumen_columna) %>%
      pivot_wider(names_from = Semana, values_from = resumen_columna, names_prefix = "Semana ")
    , by = "Servicio"
  ) %>%
  gt(rowname_col = "Servicio") %>%
  cols_label(promedio_camas = "Promedio de camas") %>%
  tab_spanner(
    label = "Promedio pacientes y ocupación semanal",
    columns = starts_with("Semana ")
  ) %>%
  cols_align(align = "center", columns = everything()) %>%
  tab_options(
    table.font.size = "small",
    data_row.padding = px(5),
    heading.align = "center"
  )

# Mostrar la tabla
#tabla_ocupacion

