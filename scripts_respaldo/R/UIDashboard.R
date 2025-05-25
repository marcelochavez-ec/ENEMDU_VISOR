
UIDashboard <- R6::R6Class("UIDashboard",
  inherit = UIBase,
  public = list(
    render = function() {
      dashboardPage(
        dashboardHeader(title = "Análisis Poblacional"),
        dashboardSidebar(
          sidebarMenu(
            menuItem("Población", tabName = "poblacion", icon = icon("users")),
            menuItem("Tasas", tabName = "tasas", icon = icon("chart-line")),
            menuItem("Caracterización Socio-demográfica", tabName = "caracterizacion", icon = icon("address-card")),
            menuItem("Sectorización del Empleo", tabName = "empleo", icon = icon("briefcase")),
            menuItem("Glosario de Términos", tabName = "glosario", icon = icon("book"))
          )
        ),
        dashboardBody(
          tabItems(
            tabItem(tabName = "poblacion",
              fluidRow(
                box(title = "Áreas: Urbano / Rural", width = 6, highchartOutput("grafico_area")),
                box(title = "Dominios: Quito, Guayaquil, etc.", width = 6, highchartOutput("grafico_dominios"))
              ),
              fluidRow(
                box(title = "Sexo: Hombres / Mujeres", width = 6, highchartOutput("grafico_sexo")),
                box(title = "Grupos de Edad", width = 6, highchartOutput("grafico_edad"))
              ),
              fluidRow(
                box(title = "Etnia", width = 12, highchartOutput("grafico_etnia"))
              )
            ),
            tabItem(tabName = "tasas", h3("Dashboard de Tasas")),
            tabItem(tabName = "caracterizacion", h3("Caracterización Socio-demográfica")),
            tabItem(tabName = "empleo", h3("Sectorización del Empleo")),
            tabItem(tabName = "glosario", h3("Glosario de Términos"))
          )
        )
      )
    }
  )
)
