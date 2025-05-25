
ServerPoblacion <- R6::R6Class("ServerPoblacion",
  inherit = ServerBase,
  public = list(
    run = function() {
      library(highcharter)
      pastel_colors <- c("#FFB3BA", "#FFDFBA", "#FFFFBA", "#BAFFC9", "#BAE1FF")

      output$grafico_area <- renderHighchart({
        highchart() %>%
          hc_chart(type = "pie", options3d = list(enabled = TRUE, alpha = 45)) %>%
          hc_title(text = "Áreas") %>%
          hc_plotOptions(pie = list(innerSize = 50, depth = 45)) %>%
          hc_add_series(
            name = "Área",
            data = list(
              list(name = "Urbano", y = 65),
              list(name = "Rural", y = 35)
            )
          ) %>%
          hc_colors(pastel_colors)
      })

      output$grafico_dominios <- renderHighchart({
        highchart() %>%
          hc_chart(type = "column", options3d = list(enabled = TRUE, alpha = 15, beta = 15, depth = 50)) %>%
          hc_title(text = "Dominios") %>%
          hc_xAxis(categories = c("Quito", "Guayaquil", "Cuenca", "Machala", "Ambato")) %>%
          hc_add_series(name = "Población", data = c(23, 18, 15, 10, 9)) %>%
          hc_colors(pastel_colors)
      })

      output$grafico_sexo <- renderHighchart({
        highchart() %>%
          hc_chart(type = "pie", options3d = list(enabled = TRUE, alpha = 45)) %>%
          hc_title(text = "Sexo") %>%
          hc_plotOptions(pie = list(innerSize = 50, depth = 45)) %>%
          hc_add_series(
            name = "Sexo",
            data = list(
              list(name = "Hombres", y = 48),
              list(name = "Mujeres", y = 52)
            )
          ) %>%
          hc_colors(pastel_colors)
      })

      output$grafico_edad <- renderHighchart({
        highchart() %>%
          hc_chart(type = "column", options3d = list(enabled = TRUE, alpha = 15, beta = 15, depth = 50)) %>%
          hc_title(text = "Grupos de Edad") %>%
          hc_xAxis(categories = c("15-24", "25-34", "35-44", "45-64", "65+")) %>%
          hc_add_series(name = "Población", data = c(12, 25, 20, 30, 13)) %>%
          hc_colors(pastel_colors)
      })

      output$grafico_etnia <- renderHighchart({
        highchart() %>%
          hc_chart(type = "column", options3d = list(enabled = TRUE, alpha = 15, beta = 15, depth = 50)) %>%
          hc_title(text = "Etnia") %>%
          hc_xAxis(categories = c("Indígena", "Afroecuatoriano", "Mestizo", "Montubio", "Otro")) %>%
          hc_add_series(name = "Población", data = c(7, 6, 70, 12, 5)) %>%
          hc_colors(pastel_colors)
      })
    }
  )
)
