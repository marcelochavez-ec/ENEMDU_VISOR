# modules/mod_poblacion.R

mod_poblacion_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    box(highchartOutput(ns("area")), width = 6, title = "Área: Urbano vs Rural"),
    box(highchartOutput(ns("dominios")), width = 6, title = "Dominios"),
    box(highchartOutput(ns("sexo")), width = 6, title = "Sexo"),
    box(highchartOutput(ns("edad")), width = 6, title = "Grupos de Edad"),
    box(highchartOutput(ns("etnia")), width = 12, title = "Etnia")
  )
}

mod_poblacion_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Simulación de datos de ejemplo para cada gráfico
    output$area <- renderHighchart({
      highchart() %>%
        hc_chart(type = "pie", options3d = list(enabled = TRUE, alpha = 45)) %>%
        hc_title(text = "Distribución por Área") %>%
        hc_plotOptions(pie = list(innerSize = '50%', depth = 45)) %>%
        hc_add_series(
          data = list(
            list(name = "Urbano", y = 65),
            list(name = "Rural", y = 35)
          ),
          name = "Área",
          colors = pastel_cols()
        )
    })
    
    output$dominios <- renderHighchart({
      highchart() %>%
        hc_chart(type = "column", options3d = list(enabled = TRUE, alpha = 15)) %>%
        hc_title(text = "Distribución por Dominio") %>%
        hc_xAxis(categories = c("Quito", "Guayaquil", "Cuenca", "Machala", "Ambato")) %>%
        hc_add_series(
          data = c(30, 25, 15, 10, 20),
          name = "Porcentaje",
          colorByPoint = TRUE,
          colors = pastel_cols()
        )
    })
    
    output$sexo <- renderHighchart({
      highchart() %>%
        hc_chart(type = "pie", options3d = list(enabled = TRUE, alpha = 45)) %>%
        hc_title(text = "Distribución por Sexo") %>%
        hc_plotOptions(pie = list(innerSize = '50%', depth = 45)) %>%
        hc_add_series(
          data = list(
            list(name = "Hombres", y = 48),
            list(name = "Mujeres", y = 52)
          ),
          name = "Sexo",
          colors = pastel_cols()
        )
    })
    
    output$edad <- renderHighchart({
      highchart() %>%
        hc_chart(type = "column", options3d = list(enabled = TRUE, alpha = 10)) %>%
        hc_title(text = "Grupos de Edad") %>%
        hc_xAxis(categories = c("15-24", "25-34", "35-44", "45-64", "65+")) %>%
        hc_add_series(
          data = c(20, 25, 18, 22, 15),
          name = "Personas",
          colorByPoint = TRUE,
          colors = pastel_cols()
        )
    })
    
    output$etnia <- renderHighchart({
      highchart() %>%
        hc_chart(type = "column", options3d = list(enabled = TRUE, alpha = 10)) %>%
        hc_title(text = "Distribución por Etnia") %>%
        hc_xAxis(categories = c("Mestizo", "Indígena", "Afroecuatoriano", "Montubio", "Blanco")) %>%
        hc_add_series(
          data = c(70, 10, 8, 7, 5),
          name = "Personas",
          colorByPoint = TRUE,
          colors = pastel_cols()
        )
    })
    
  })
}

# Función de paleta pastel
pastel_cols <- function() {
    c("#F1948A", "#85C1E9", "#73C6B6", "#F7DC6F", "#BB8FCE", "#F8C471")}

