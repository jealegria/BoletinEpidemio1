library(dplyr)
library(lubridate)
library(gt)
library(tidyr)

# Filtrar el mes anterior
base_mc_filtro <- base_mc %>%
  filter(month(Fecha) == month(Sys.Date()) - 1, year(Fecha) == year(Sys.Date()))

# Calcular el % de ocupación y preparar la columna con % y pacientes
ocupacion_semana <- base_mc_filtro %>%
  group_by(Servicio, Semana = isoweek(Fecha)) %>%
  summarise(
    total_pacientes_dia = sum(`Pacientes dia`, na.rm = TRUE),
    total_disponibles = sum(Disponibles, na.rm = TRUE)
  ) %>%
  mutate(
    porcentaje_ocupacion = if_else(
      total_disponibles == 0 & total_pacientes_dia == 0, 
      0, 
      (total_pacientes_dia / total_disponibles) * 100
    ),
    ocupacion_pacientes = paste0(round(porcentaje_ocupacion, 2), "% | ", total_pacientes_dia)
  ) %>%
  select(Servicio, Semana, ocupacion_pacientes) %>%
  pivot_wider(names_from = Semana, values_from = ocupacion_pacientes, names_prefix = "Semana ") %>%
  ungroup()

# Crear la tabla gt
tabla_ocupacion <- ocupacion_semana %>%
  gt(rowname_col = "Servicio") %>%
  tab_spanner(
    label = "Semanas",
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


