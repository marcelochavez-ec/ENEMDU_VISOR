# global.R
library(shiny)
library(shinydashboard)
library(highcharter)
library(tidyverse)
library(openxlsx)
library(shinyWidgets)
library(fontawesome)

setwd("D:/DATA_SCIENCE_ENDI/APPS/ENEMDU2")

# Paleta de colores
paleta_pastel <- c("#9B59B6",  # Violeta fuerte (púrpura)
                   "#F39C12",  # Naranja dorado
                   "#1ABC9C",  # Verde turquesa vibrante
                   "#E74C3C",  # Rojo coral intenso
                   "#3498DB",  # Azul vibrante
                   "#8E44AD")   # Morado oscuro para contraste

# Trimestres
trimestres <- c("IV 2020",
                "I 2021", 
                "II 2021", 
                "III 2021",
                "IV 2021", 
                "I 2022",
                "II 2022", 
                "III 2022", 
                "IV 2022",
                "I 2023",
                "II 2023", 
                "III 2023",
                "IV 2023",
                "I 2024",
                "II 2024", 
                "III 2024",
                "IV 2024",
                "I 2025")

# Indicadores
indicadores <- c("Empleo Bruto (%)",
                 "Empleo Global (%)",
                 "Empleo Adecuado/Pleno (%)",
                 "Subempleo (%)",
                 "Subempleo por insuficiencia de tiempo de trabajo (%)",
                 "Subempleo por  insuficiencia de ingresos (%)",
                 "Empleo no Remunerado (%)",
                 "Otro Empleo no pleno(%)",
                 "Empleo no Clasificado (%)",
                 "Desempleo (%)",
                 "Desempleo Abierto (%)",
                 "Desempleo Oculto (%)",
                 "Participación Global (%)",
                 "Participación Bruta (%)")

# Nombres de columnas deseados
nombres_columnas <- c("TRIMESTRE",
                      "INDICADOR",
                      "TOTAL_NACIONAL",
                      "URBANO", 
                      "RURAL",
                      "QUITO",
                      "GUAYAQUIL", 
                      "CUENCA",
                      "MACHALA",
                      "AMBATO",
                      "HOMBRES",
                      "MUJERES",
                      "EDAD_1", 
                      "EDAD_2",
                      "EDAD_3", 
                      "EDAD_4", 
                      "EDAD_5",
                      "ETNIA_1",
                      "ETNIA_2",
                      "ETNIA_3",
                      "ETNIA_4",
                      "ETNIA_5",
                      "ETNIA_6")

df_enemdu <- read.xlsx("db/2025_I_trimestre_Tabulados_Mercado_Laboral.xlsx",
                       sheet = "2. Tasas", 
                       startRow = 3) %>% 
    setNames(nombres_columnas) %>% 
    mutate(
        across(c(TRIMESTRE, INDICADOR), as.factor),
        across(-c(TRIMESTRE, INDICADOR), as.numeric))


# Categorías
areas  <- c("URBANO", "RURAL")
dominios <- c("QUITO", "GUAYAQUIL", "CUENCA", "MACHALA", "AMBATO")
sexos <- c("HOMBRES", "MUJERES")
edades <- c("ENTRE 15 y 24 AÑOS", "ENTRE 25 y 34 AÑOS", "ENTRE 35 y 44 AÑOS", "ENTRE 45 y 64 AÑOS", "65 AÑOS Y MÁS")
etnias <- c("INDÍGENAS", "AFROECUATORIANOS", "MESTIZOS", "BLANCOS", "MONTUBIOS", "OTROS")

# Nombres de columnas reales (del archivo Excel)
edades_columnas <- c("EDAD_1", "EDAD_2", "EDAD_3", "EDAD_4", "EDAD_5")
etnias_columnas <- c("ETNIA_1", "ETNIA_2", "ETNIA_3", "ETNIA_4", "ETNIA_5", "ETNIA_6")

# Función auxiliar
crear_lista_puntos <- function(nombres, valores) {
    map2(nombres, valores, ~list(name = .x, y = .y))
}

# Construcción de estructura simulada
df_enemdu <- df_enemdu %>%
    rowwise() %>%
    mutate(
        AREA     = list(crear_lista_puntos(areas, c_across(all_of(areas)))),
        DOMINIOS = list(crear_lista_puntos(dominios, c_across(all_of(dominios)))),
        SEXO     = list(crear_lista_puntos(sexos, c_across(all_of(sexos)))),
        EDAD     = list(crear_lista_puntos(edades, c_across(all_of(edades_columnas)))),
        ETNIA    = list(crear_lista_puntos(etnias, c_across(all_of(etnias_columnas))))
    ) %>%
    select(TRIMESTRE, INDICADOR, TOTAL_NACIONAL, AREA, DOMINIOS, SEXO, EDAD, ETNIA) %>%
    ungroup()
