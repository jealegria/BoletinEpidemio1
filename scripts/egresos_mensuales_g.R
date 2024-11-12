##### 1. Procesar datos====


# Definir el año actual y el año anterior
año_actual <- year(Sys.Date())
año_anterior <- año_actual - 1

# Filtrar el dataset para incluir solo el año anterior y el año actual, hasta el mes anterior al actual
mes_actual <- month(Sys.Date())
base_filtrada <- base %>%
  filter((year(Fecha) == año_actual & month(Fecha) < mes_actual) |
           year(Fecha) == año_anterior)

# Filtrar para el servicio Pediatría
base_pediatria <- base_filtrada %>%
  filter(Servicio == "Pediatria")

# Filtrar para el servicio Adultos
base_adultos <- base_filtrada %>%
  filter(Servicio == "Adultos")

# Contar la cantidad de egresos por año y mes para Pediatría
tabla_egresos_mensual_p <- base_pediatria %>%
  mutate(Año = year(Fecha), Mes = month(Fecha, label = TRUE, abbr = TRUE)) %>%
  group_by(Año, Mes) %>%
  summarise(Cantidad_egresos = n(), .groups = "drop") %>%
  pivot_wider(names_from = Año, values_from = Cantidad_egresos, values_fill = NA) %>%
  arrange(Mes)

# Contar la cantidad de egresos por año y mes para Adultos
tabla_egresos_mensual_a <- base_adultos %>%
  mutate(Año = year(Fecha), Mes = month(Fecha, label = TRUE, abbr = TRUE)) %>%
  group_by(Año, Mes) %>%
  summarise(Cantidad_egresos = n(), .groups = "drop") %>%
  pivot_wider(names_from = Año, values_from = Cantidad_egresos, values_fill = NA) %>%
  arrange(Mes)

# Calcular la diferencia entre los meses de 2024 y 2023 para Pediatría
tabla_egresos_mensual_p <- tabla_egresos_mensual_p %>%
  mutate(Diferencia = `2024` - `2023`,
         Diferencia_Perc = ifelse(is.na(`2023`), NA, (Diferencia / `2023`) * 100))

# Calcular la diferencia entre los meses de 2024 y 2023 para Adultos
tabla_egresos_mensual_a <- tabla_egresos_mensual_a %>%
  mutate(Diferencia = `2024` - `2023`,
         Diferencia_Perc = ifelse(is.na(`2023`), NA, (Diferencia / `2023`) * 100))

# Redondear las columnas a 0 decimales (sin decimales)
tabla_egresos_mensual_p <- tabla_egresos_mensual_p %>%
  mutate(across(`2023`:`2024`, round, 0),
         across(c(Diferencia, Diferencia_Perc), round, 0))

tabla_egresos_mensual_a <- tabla_egresos_mensual_a %>%
  mutate(across(`2023`:`2024`, round, 0),
         across(c(Diferencia, Diferencia_Perc), round, 0))


##### 2.Creacion de las tablas====

# Crear la tabla de Pediatría con los meses como columnas y los años como filas
tabla_pediatria_gt <- tabla_egresos_mensual_p %>%
  select(Mes, `2023`, `2024`, Diferencia, Diferencia_Perc) %>%
  pivot_longer(cols = c(`2023`, `2024`, Diferencia, Diferencia_Perc), 
               names_to = "Variable", 
               values_to = "Valor") %>%
  pivot_wider(names_from = Mes, values_from = Valor) %>%
  gt() %>%
  tab_spanner(label = "Egresos", columns = starts_with("2023")) %>%
  tab_spanner(label = "Diferencias", columns = starts_with("Diferencia")) %>%
  tab_header(title = "Egresos y Diferencias Mensuales por Año")

# Crear la tabla de Adultos con los meses como columnas y los años como filas
tabla_adultos_gt <- tabla_egresos_mensual_a %>%
  select(Mes, `2023`, `2024`, Diferencia, Diferencia_Perc) %>%
  pivot_longer(cols = c(`2023`, `2024`, Diferencia, Diferencia_Perc), 
               names_to = "Variable", 
               values_to = "Valor") %>%
  pivot_wider(names_from = Mes, values_from = Valor) %>%
  gt() %>%
  tab_spanner(label = "Egresos", columns = starts_with("2023")) %>%
  tab_spanner(label = "Diferencias", columns = starts_with("Diferencia")) %>%
  tab_header(title = "Egresos y Diferencias Mensuales por Año (Adultos)")

# Mostrar las tablas
#tabla_pediatria_gt
#tabla_adultos_gt


