# Server
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
