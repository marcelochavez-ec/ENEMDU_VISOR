# global.R
library(shiny)
library(shinydashboard)
library(highcharter)
library(tidyverse)
library(openxlsx)
library(shinyWidgets)
library(fontawesome)
# setwd("D:/DATA_SCIENCE_ENDI/APPS/ENEMDU2")
library(rsconnect)
rsconnect::writeManifest(appPrimaryDoc = "app.R")

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


# ******************************************************************************
#                                   UI
# ******************************************************************************

ui <- dashboardPage(skin = "purple",
                    dashboardHeader(title = "ENEMDU Trimestral"),
                    dashboardSidebar(
                      sidebarMenu(
                        id = "menu",
                        menuItem("Población", tabName = "poblacion", icon = icon("users")),
                        menuItem("Tasas", tabName = "tasas", icon = icon("chart-line")),
                        menuItem("Caracterización", tabName = "caracterizacion", icon = icon("id-card")),
                        menuItem("Sectorización del Empleo", tabName = "empleo", icon = icon("briefcase")),
                        menuItem("Glosario", tabName = "glosario", icon = icon("book")),
                        
                        conditionalPanel(
                          condition = "input.menu == 'tasas'",
                          tags$div(
                            style = "padding: 15px; background-color: #e6f0fa; border-radius: 10px; margin: 20px; box-shadow: 0 2px 6px rgba(0,0,0,0.1); overflow: visible;",
                            
                            h4("Filtros", style = "text-align: center; color: #004e98; font-weight: bold; margin-bottom: 15px;"),
                            
                            tags$div(
                              style = "margin-bottom: 2px;",
                              pickerInput(
                                inputId = "trimestre",
                                label = HTML("<span style='color:#004e98;'>Trimestre:</span>"),
                                choices = unique(df_enemdu$TRIMESTRE),
                                options = list(
                                  style = "btn-primary"
                                ),
                                width = "100%"
                              )
                            ),
                            
                            tags$div(
                              style = "margin-bottom: 5px;",
                              pickerInput(
                                inputId = "indicador",
                                label = HTML("<span style='color:#004e98;'>Indicador:</span>"),
                                choices = unique(df_enemdu$INDICADOR),
                                options = list(
                                  style = "btn-primary"
                                ),
                                width = "100%"
                              )
                            ),
                            
                            tags$style(HTML("
    /* Colorea las opciones del dropdown */
    .dropdown-menu.inner li a {
      color: black !important;}")))))),
    
    dashboardBody(
      tags$head(
        tags$style(HTML("
        body, .content-wrapper, .main-header, .main-sidebar {
          font-family: 'Arial Narrow', Arial, sans-serif;
        }
        .custom-box {
          background-color: white;
          padding: 15px;
          border-radius: 8px;
          box-shadow: 0 4px 8px rgba(0,0,0,0.1);
          margin-bottom: 20px;
          height: 320px;
        }
        .custom-box h4 {
          text-align: center;
          margin-bottom: 10px;
        }
        .graph-footnote {
          font-size: 10px;
          color: #666666;
          text-align: center;
          margin-top: 8px;
        }
      "))),
      tabItems(
        tabItem(tabName = "tasas",
                fluidRow(
                  column(width = 6,
                         div(class="custom-box",
                             h2("Visor de Estadísticas de la ENEMDU Trimestral",
                                style = "text-align: center; font-weight: bold; color: #8E44AD;"),
                             hr(),
                             valueBoxOutput("total_nacional", width = 12))),
                  column(width = 6,
                         div(class = "custom-box",
                             h4("Área: Urbano vs Rural"),
                             highchartOutput("grafico_area", height = "240px"),
                             div("Fuente: INEC, ENEMDU TRIMESTRAL 2025", class = "graph-footnote")))),
                fluidRow(
                  column(width = 6,
                         div(class = "custom-box",
                             h4("Ciudades principales: Quito, Guayaquil, Cuenca, Machala, Ambato"),
                             highchartOutput("grafico_dominios", height = "240px"),
                             div("Fuente: INEC, ENEMDU TRIMESTRAL 2025", class = "graph-footnote"))),
                  column(width = 6,
                         div(class = "custom-box",
                             h4("Sexo: Hombres vs Mujeres"),
                             highchartOutput("grafico_sexo", height = "240px"),
                             div("Fuente: INEC, ENEMDU TRIMESTRAL 2025", class = "graph-footnote")))),
                fluidRow(
                  column(width = 6,
                         div(class = "custom-box",
                             h4("Grupos de Edad"),
                             highchartOutput("grafico_edad", height = "240px"),
                             div("Fuente: INEC, ENEMDU TRIMESTRAL 2025", class = "graph-footnote"))),
                  column(width = 6,
                         div(class = "custom-box",
                             h4("Distribución por Etnia"),
                             highchartOutput("grafico_etnia", height = "240px"),
                             div("Fuente: INEC, ENEMDU TRIMESTRAL 2025", class = "graph-footnote"))))),
        tabItem(tabName = "poblacion", h3("⚠️ Esta sección está en construcción.")),
        tabItem(tabName = "caracterizacion", h3("⚠️ Esta sección está en construcción.")),
        tabItem(tabName = "empleo", h3("⚠️ Esta sección está en construcción.")),
        tabItem(tabName = "glosario", h3("⚠️ Esta sección está en construcción."))
      )
    )
)

# ******************************************************************************
#                                   SEVER
# ******************************************************************************

server <- function(input, output, session) {
  
  data_filtrada <- reactive({
    df_enemdu %>%
      filter(TRIMESTRE == input$trimestre, INDICADOR == input$indicador)
  })
  
  output$total_nacional <- renderValueBox({
    total <- data_filtrada()$TOTAL_NACIONAL[1]
    valueBox(
      paste0(formatC(total, format = "f", digits = 2, big.mark = ","), " %"), 
      HTML(paste0(
        "<b>Indicador:</b> ", input$indicador, ", ",
        "<b>Período:</b> ", input$trimestre
      )),
      icon = icon("bar-chart"),
      color =  "light-blue")})
  
  # Gráfico de área urbano-rural:
  
  output$grafico_area <- renderHighchart({
    highchart() %>%
      hc_chart(type = "pie", options3d = list(enabled = TRUE, alpha = 45, beta = 0)) %>%
      hc_plotOptions(
        pie = list(
          innerSize = '60%',
          depth = 45,
          dataLabels = list(
            enabled = TRUE,
            format = '{point.name}: {point.y:.2f}'))) %>%
      hc_tooltip(pointFormat = 'Indicador: <b>{point.y:.2f}%</b>') %>%
      hc_colors(paleta_pastel) %>%
      hc_add_series(data = data_filtrada()$AREA[[1]], name = "Área")})
  
  # Gráfico de las ciudades principales:
  
  output$grafico_dominios <- renderHighchart({
    dominios <- data_filtrada()$DOMINIOS[[1]]
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_colors(paleta_pastel) %>%
      hc_xAxis(categories = map_chr(dominios, "name")) %>%
      hc_yAxis(title = list(text = "Porcentaje")) %>%
      hc_plotOptions(column = list(
        dataLabels = list(
          enabled = TRUE,
          format = '{point.y:.2f}'))) %>%
      hc_tooltip(headerFormat = "",  # Evita mostrar el nombre de la serie en negrita
                 pointFormat = '<b>{point.category}</b><br/>INDICADOR: <b>{point.y:.2f}%</b>') %>%
      hc_legend(enabled = FALSE) %>% 
      hc_add_series(data = map_dbl(dominios, "y"))})
  
  # Gráfico por sexo:
  
  output$grafico_sexo <- renderHighchart({
    highchart() %>%
      hc_chart(type = "pie", options3d = list(enabled = TRUE, alpha = 45, beta = 0)) %>%
      hc_plotOptions(
        pie = list(
          innerSize = '60%',
          depth = 45,
          dataLabels = list(
            enabled = TRUE,
            format = '{point.name}: {point.y:.2f}'))) %>%
      hc_colors(paleta_pastel) %>%
      hc_tooltip(pointFormat = 'Indicador: <b>{point.y:.2f}%</b>') %>%
      hc_add_series(data = data_filtrada()$SEXO[[1]], name = "Sexo")})
  
  # Gráfico por rangos etáreos:  
  
  output$grafico_edad <- renderHighchart({
    edad <- data_filtrada()$EDAD[[1]]
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_colors(paleta_pastel) %>%
      hc_xAxis(categories = map_chr(edad, "name")) %>%
      hc_yAxis(title = list(text = "Porcentaje")) %>%
      hc_plotOptions(column = list(
        dataLabels = list(
          enabled = TRUE,
          format = '{point.y:.2f}'))) %>%
      hc_tooltip(headerFormat = "",  # Evita mostrar el nombre de la serie en negrita
                 pointFormat = '<b>{point.category}</b><br/>INDICADOR: <b>{point.y:.2f}%</b>') %>%
      hc_legend(enabled = FALSE) %>% 
      hc_add_series(data = map_dbl(edad, "y"), name = "Edad")})
  
  # Gráfico por tipo autoidentificación étnica:
  
  output$grafico_etnia <- renderHighchart({
    etnia <- data_filtrada()$ETNIA[[1]]
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_colors(paleta_pastel) %>%
      hc_xAxis(categories = map_chr(etnia, "name")) %>%
      hc_yAxis(title = list(text = "Porcentaje")) %>%
      hc_plotOptions(column = list(
        dataLabels = list(
          enabled = TRUE,
          format = '{point.y:.2f}'))) %>%
      hc_tooltip(headerFormat = "",  # Evita mostrar el nombre de la serie en negrita
                 pointFormat = '<b>{point.category}</b><br/>INDICADOR: <b>{point.y:.2f}%</b>') %>%
      hc_legend(enabled = FALSE) %>% 
      hc_add_series(data = map_dbl(etnia, "y"), name = "Etnia")})
}

# Ejecutar app
shinyApp(ui, server)

