# Cargar librerías
library(shiny)
library(dplyr)
library(purrr)
library(highcharter)

# Crear datos simulados
trimestres <- c("I-2024", "II-2024", "III-2024", "IV-2024")
indicadores <- c("POBLACIÓN OCUPADA", "POBLACIÓN DESOCUPADA")

areas <- c("URBANO", "RURAL")
dominios <- c("QUITO", "GUAYAQUIL", "CUENCA", "MACHALA", "AMBATO")
sexos <- c("HOMBRES", "MUJERES")
edades <- c("ENTRE 15 y 24 AÑOS", "ENTRE 25 y 34 AÑOS", "ENTRE 35 y 44 AÑOS", "ENTRE 45 y 64 AÑOS", "65 AÑOS Y MÁS")
etnias <- c("INDÍGENAS", "AFROECUATORIANOS", "MESTIZOS", "BLANCOS", "MONTUBIOS", "OTROS")

crear_lista_puntos <- function(nombres, valores) {
    map2(nombres, valores, ~list(name = .x, y = .y))
}

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

# UI
ui <- fluidPage(
    titlePanel("Demo ENEMDU - Highcharter"),
    sidebarLayout(
        sidebarPanel(
            selectInput("trimestre", "Selecciona un trimestre", choices = unique(df_simulado$TRIMESTRE)),
            selectInput("indicador", "Selecciona un indicador", choices = unique(df_simulado$INDICADOR)),
            width = 3
        ),
        mainPanel(
            h4("Gráfico por Área"),
            highchartOutput("grafico_area"),
            h4("Gráfico por Dominio"),
            highchartOutput("grafico_dominios"),
            h4("Gráfico por Sexo"),
            highchartOutput("grafico_sexo"),
            h4("Gráfico por Edad"),
            highchartOutput("grafico_edad"),
            h4("Gráfico por Etnia"),
            highchartOutput("grafico_etnia")
        )
    )
)

# SERVER
server <- function(input, output, session) {
    
    # Filtro reactivo
    data_filtrada <- reactive({
        df_simulado %>%
            filter(TRIMESTRE == input$trimestre, INDICADOR == input$indicador)
    })
    
    # Función para renderizar gráficos
    render_grafico <- function(lista_datos, titulo, colores = NULL) {
        hc <- highchart() %>%
            hc_chart(type = "column") %>%
            hc_title(text = titulo) %>%
            hc_xAxis(type = "category") %>%
            hc_add_series(data = lista_datos, name = "Valor") %>%
            hc_plotOptions(column = list(dataLabels = list(enabled = TRUE)))
        
        if (!is.null(colores)) {
            hc <- hc %>% hc_colors(colores)
        }
        
        hc
    }
    
    output$grafico_area <- renderHighchart({
        datos <- data_filtrada()$AREA[[1]]
        render_grafico(datos, "Distribución por Área", c("#004e98", "#1ca9c9"))
    })
    
    output$grafico_dominios <- renderHighchart({
        datos <- data_filtrada()$DOMINIOS[[1]]
        render_grafico(datos, "Distribución por Dominio")
    })
    
    output$grafico_sexo <- renderHighchart({
        datos <- data_filtrada()$SEXO[[1]]
        render_grafico(datos, "Distribución por Sexo", c("#D95F02", "#7570B3"))
    })
    
    output$grafico_edad <- renderHighchart({
        datos <- data_filtrada()$EDAD[[1]]
        render_grafico(datos, "Distribución por Edad")
    })
    
    output$grafico_etnia <- renderHighchart({
        datos <- data_filtrada()$ETNIA[[1]]
        render_grafico(datos, "Distribución por Etnia")
    })
    
}

# Ejecutar app
shinyApp(ui, server)
