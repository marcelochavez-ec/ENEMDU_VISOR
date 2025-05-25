

areas <- c("URBANO", "RURAL")
dominios <- c("QUITO", "GUAYAQUIL", "CUENCA", "MACHAL", "AMBATO")
sexos <- c("HOMBRES", "MUJERES")
edades <- c("ENTRE 15 y 24 AÑOS", "ENTRE 25 y 34 AÑOS", "ENTRE 35 y 44 AÑOS", "ENTRE 45 y 64 AÑOS", "65 AÑOS Y MÁS")
etnias <- c("INDÍGENAS", "AFROECUATORIANOS", "MESTIZOS", "BLANCOS", "MONTUBIOS", "OTROS")

# Crear función auxiliar para listas de datos
crear_lista_puntos <- function(nombres, valores) {
  map2(nombres, valores, ~list(name = .x, y = .y))
}

# Datos simulados
set.seed(123)
df_simulado <- expand.grid(
  TRIMESTRE = trimestres,
  INDICADOR = indicadores,
  stringsAsFactors = FALSE
) %>%
  rowwise() %>%
  mutate(
    TOTAL_NACIONAL = sample(15000000:18000000, 1),
    AREA = list(crear_lista_puntos(areas, sample(30:70, 2))),
    DOMINIOS = list(crear_lista_puntos(dominios, sample(10:30, 5))),
    SEXO = list(crear_lista_puntos(sexos, sample(45:55, 2))),
    EDAD = list(crear_lista_puntos(edades, sample(10:30, 5))),
    ETNIA = list(crear_lista_puntos(etnias, sample(5:80, 6)))
  ) %>%
  ungroup()
