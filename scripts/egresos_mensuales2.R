##### 1. Graficos de linea====


#Proceso de datos para obtener fecha segun año actual, dividir dataframe segun Servicio


# Obtener el año y mes actuales
fecha_actual <- Sys.Date()
anio_actual <- format(fecha_actual, "%Y")
mes_actual <- as.numeric(format(fecha_actual, "%m"))

# Calcular el último día del mes anterior
primer_dia_mes_actual <- as.Date(paste0(anio_actual, "-", mes_actual, "-01"))
ultimo_dia_mes_anterior <- primer_dia_mes_actual - 1

# Determinar el número de semana de ese último día del mes anterior
semana_limite <- as.numeric(format(ultimo_dia_mes_anterior, "%U")) + 1

# Filtrar los datos hasta la última semana de octubre (o del mes anterior)
base_filtrada <- base %>%
  filter(Fecha <= ultimo_dia_mes_anterior) %>%
  filter(Semana <= semana_limite)

# Separar el dataframe según "Servicio"
base_separado <- split(base_filtrada, base_filtrada$Servicio)

# Contar egresos por semana y tipo de egreso para Pediatría
base_pediatria <- base_separado$Pediatria %>%
  filter(`Tipo de egreso` %in% c("Derivación", "Internación")) %>%
  group_by(Semana, `Tipo de egreso`) %>%
  summarise(Cantidad = n(), .groups = 'drop')


# Crear gráfico de línea para Pediatría (con solo 2 líneas)
egresos_mensual_p <- ggplot(base_pediatria, aes(x = Semana, y = Cantidad, color = `Tipo de egreso`, group = `Tipo de egreso`)) +
  geom_line(size = 1) + 
  labs(title = "Egresos por Semana (Pediatría)",
       x = "Semana",
       y = "Cantidad de Egresos") +
  theme_minimal() +
  scale_color_manual(values = c("Derivación" = "blue", "Internación" = "red"))

# Contar egresos por semana y tipo de egreso para Adultos
base_adultos <- base_separado$Adultos %>%
  filter(`Tipo de egreso` %in% c("Derivación", "Internación")) %>%
  group_by(Semana, `Tipo de egreso`) %>%
  summarise(Cantidad = n(), .groups = 'drop')

# Crear gráfico de línea para Adultos (con solo 2 líneas)
egresos_mensual_a <- ggplot(base_adultos, aes(x = Semana, y = Cantidad, color = `Tipo de egreso`, group = `Tipo de egreso`)) +
  geom_line(size = 1) + 
  labs(title = "Egresos por Semana (Adultos)",
       x = "Semana",
       y = "Cantidad de Egresos") +
  theme_minimal() +
  scale_color_manual(values = c("Derivación" = "blue", "Internación" = "red"))


##### 2. Tablas de egresos====

#Proceso de datos

# Obtener la fecha actual y calcular el último día del mes anterior
fecha_actual <- Sys.Date()
anio_actual <- format(fecha_actual, "%Y")
mes_actual <- as.numeric(format(fecha_actual, "%m"))

# Calcular el primer día del mes actual y el último día del mes anterior
primer_dia_mes_actual <- as.Date(paste0(anio_actual, "-", mes_actual, "-01"))
ultimo_dia_mes_anterior <- primer_dia_mes_actual - 1

# Determinar el número de semana de ese último día del mes anterior
semana_limite <- as.numeric(format(ultimo_dia_mes_anterior, "%U")) + 1

# Filtrar los datos hasta el último día del mes anterior
base_filtrada <- base %>%
  filter(Fecha <= ultimo_dia_mes_anterior) %>%
  filter(Semana <= semana_limite)

# Obtener las semanas del mes anterior (mes previo al mes actual)
mes_anterior <- mes_actual - 1
if (mes_anterior == 0) {
  mes_anterior <- 12
  anio_anterior <- anio_actual - 1
} else {
  anio_anterior <- anio_actual
}

# Filtrar para que solo incluya las semanas del mes anterior
base_mes_anterior <- base_filtrada %>%
  filter(format(Fecha, "%Y-%m") == paste(anio_anterior, sprintf("%02d", mes_anterior), sep = "-"))

#Creacion de tablas

# Crear tabla para Pediatría
tabla_pediatria <- base_mes_anterior %>%
  filter(Servicio == "Pediatria", `Tipo de egreso` %in% c("Derivación", "Internación")) %>%
  group_by(Semana, `Tipo de egreso`) %>%
  summarise(Cantidad = n(), .groups = 'drop') %>%
  pivot_wider(names_from = `Tipo de egreso`, values_from = Cantidad, values_fill = list(Cantidad = 0)) %>%
  mutate(Total = `Derivación` + `Internación`) %>%
  select(Semana, `Derivación`, `Internación`, Total) %>%
  gt() %>%
  tab_header(
    title = "Egresos por Semana - Pediatría"
  )

# Crear tabla para Adultos
tabla_adultos <- base_mes_anterior %>%
  filter(Servicio == "Adultos", `Tipo de egreso` %in% c("Derivación", "Internación")) %>%
  group_by(Semana, `Tipo de egreso`) %>%
  summarise(Cantidad = n(), .groups = 'drop') %>%
  pivot_wider(names_from = `Tipo de egreso`, values_from = Cantidad, values_fill = list(Cantidad = 0)) %>%
  mutate(Total = `Derivación` + `Internación`) %>%
  select(Semana, `Derivación`, `Internación`, Total) %>%
  gt() %>%
  tab_header(
    title = "Egresos por Semana - Adultos"
  )

# Mostrar las tablas
#tabla_pediatria
#tabla_adultos


