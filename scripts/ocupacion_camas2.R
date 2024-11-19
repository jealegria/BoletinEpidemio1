library(dplyr)
library(lubridate)
library(gt)

# Filtrar el mes anterior
base_mc_filtro <- base_mc %>%
  filter(month(Fecha) == month(Sys.Date()) - 1, year(Fecha) == year(Sys.Date()))

# Calcular promedios semanales
promedio_semanal <- base_mc_filtro %>%
  group_by(Servicio, Semana = isoweek(Fecha)) %>%
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

# Crear tabla gt
tabla_ocupacion <- promedio_semanal %>%
  mutate(
    ocupacion_resumen = paste0(
      round(ifelse(is.na(promedio_pacientes), 0, promedio_pacientes), 0), " pacientes | ",
      round(ifelse(is.na(promedio_camas), 0, promedio_camas), 0), " camas | ",
      round(ifelse(is.na(porcentaje_ocupacion), 0, porcentaje_ocupacion), 0), "%"
    )
  ) %>%
  select(Servicio, Semana, ocupacion_resumen) %>%
  pivot_wider(names_from = Semana, values_from = ocupacion_resumen, names_prefix = "Semana ") %>%
  gt(rowname_col = "Servicio") %>%
  tab_spanner(
    label = "Promedio de ocupacion semanal",
    columns = starts_with("Semana ")
  ) %>%
  cols_align(align = "center", columns = starts_with("Semana ")) %>%
  tab_options(
    table.font.size = "small",
    data_row.padding = px(5),
    heading.align = "center"
  )

# Mostrar la tabla
tabla_ocupacion

