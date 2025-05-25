
# UI
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