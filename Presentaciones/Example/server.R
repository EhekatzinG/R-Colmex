#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#Outputs
shinyServer(function(input, output, session) {
    
    output$oaxx <- renderPlotly({
        plot_ly(oax100, type = 'scatter', mode = 'markers')%>%
            add_trace(x = ~Fecha, y =~Brecha, name = 'Brecha', line = list(color = 'purple', width = 4),  marker = list(color = 'purple', size = 10))%>%
            add_trace(x = ~Fecha, y = ~Unexplained, name = 'Unexplained', line = list(color = 'magenta', width = 4),  marker = list(color = 'magenta', size = 10))%>%
            add_trace(x = ~Fecha, y = ~Explained, name = 'Explained', line = list(color = 'pink', width = 4),  marker = list(color = 'pink', size = 10))%>%
            layout(showlegend = T, title='Descompisición de la brecha salarial de género',
                   xaxis = list(title = 'Fecha',rangeslider = list(visible = T)), yaxis = list(title = ''))
        
    })
    
    output$oax2 <- renderPlotly({
        plot_ly(oax2, type = 'scatter', mode = 'markers')%>%
            add_trace(x = ~Fecha, y =~Hombres, name = 'Hombres', line = list(color = 'blue', width = 4),  marker = list(color = 'blue', size = 10))%>%
            add_trace(x = ~Fecha, y = ~Mujeres, name = 'Mujeres', line = list(color = 'magenta', width = 4),  marker = list(color = 'magenta', size = 10))%>%
            layout(showlegend = T, title='Salario por hora estimado',
                   xaxis = list(title = 'Fecha',rangeslider = list(visible = T)), yaxis = list(title = ''))
        
        
    })
    
    output$oaxy2 <- renderDygraph({
        
        dygraph(oax2_y, main = "Salario por hora estimado") %>%
            dySeries("Hombres", drawPoints = TRUE,pointSize = 5, color = "blue")%>%
            dySeries("Mujeres", drawPoints = TRUE, pointSize = 5, color = "magenta")%>%
            dyOptions(stackedGraph = FALSE, strokeWidth = 3) %>%
            dyAxis("y", label = "", valueRange = c(100, 160)) %>%
            dyAxis("x", ) %>%
            dyRangeSelector(height = 30)
        
    })
    
    output$oaxy <- renderDygraph({
        
        dygraph(oax_y, main = "Descompisición de la brecha salarial de género") %>%
            dySeries("Brecha", drawPoints = TRUE,  pointSize = 5, color = 'purple') %>%
            dySeries("Unexplained", drawPoints = TRUE,pointSize = 5, color = 'magenta')%>%
            dySeries("Explained", drawPoints = TRUE, pointSize = 5, color = 'pink')%>%
            dyOptions(stackedGraph = FALSE, strokeWidth = 3) %>%
            dyAxis("y", label = "", valueRange = c(-.1, .16)) %>%
            dyAxis("x", ) %>%
            dyRangeSelector(height = 30)
        
    })
    
    output$oax100 <- renderPlotly({
        
        plot_ly(oax100, type = 'scatter', mode = 'markers')%>%
            add_trace(x = ~Fecha, y =~Brecha, name = 'Brecha', line = list(color = 'purple', width = 4),  marker = list(color = 'purple', size = 10))%>%
            add_trace(x = ~Fecha, y = ~Unexplained, name = 'Unexplained', line = list(color = 'magenta', width = 4),  marker = list(color = 'magenta', size = 10))%>%
            add_trace(x = ~Fecha, y = ~Explained, name = 'Explained', line = list(color = 'pink', width = 4),  marker = list(color = 'pink', size = 10))%>%
            layout(showlegend = T, title='Descompisición de la brecha salarial de género',
                   xaxis = list(title = 'Fecha',rangeslider = list(visible = T)), yaxis = list(title = ''))
        
        
    })
    
    output$oax99 <- renderPlotly({
        
        plot_ly(oax, type = 'scatter', mode = 'markers')%>%
            add_trace(x = ~Fecha, y =~Brecha, name = 'Brecha', line = list(color = 'purple', width = 4),  marker = list(color = 'purple', size = 10))%>%
            add_trace(x = ~Fecha, y = ~Unexplained, name = 'Unexplained', line = list(color = 'magenta', width = 4),  marker = list(color = 'magenta', size = 10))%>%
            add_trace(x = ~Fecha, y = ~Explained, name = 'Explained', line = list(color = 'pink', width = 4),  marker = list(color = 'pink', size = 10))%>%
            layout(showlegend = T, title='Descompisición de la brecha salarial de género',
                   xaxis = list(title = 'Fecha',rangeslider = list(visible = T)), yaxis = list(title = ''))
        
        
    })
    
    
    output$brecha_ent <- renderPlot({
        data <- oax_ent %>%
            filter(year == input$year) %>%
            filter(ent == input$ent)
        print(oax_ent)
        plot_ly(oax_ent, type = 'scatter', mode = 'lines')%>%
            add_trace(x = ~Year, y = ~Brecha, name = 'Brecha')%>%
            add_trace(x = ~Year, y = ~Explained, name = 'Explaied')%>%
            add_trace(x = ~Year, y = ~Unexplained, name = 'Unexplained')%>%
            layout(showlegend = F, title='Time Series with Range Slider and Selectors',
                   xaxis = list(rangeslider = list(visible = T)))
    })
    
    output$plot <- renderPlotly({
        data1 <- oax_ent %>%
                filter(Year == input$Year) %>%
            filter(Estado == input$Estado)
        print(data1)
        plot_ly(data1, type = 'bar')%>%
            add_trace(x = ~Year, y = ~Brecha, name = 'Brecha', marker = list(color = 'purple'))%>%
            add_trace(x = ~Year, y = ~Explained, name = 'Explaied',marker = list(color = 'magenta'))%>%
            add_trace(x = ~Year, y = ~Unexplained, name = 'Unexplained', marker = list(color = 'pink'))%>%
            layout(showlegend = T, title='Descomposición de la frecha salarial de género')
    })
    
    output$plot2 <- renderPlotly({
        data2 <- oax_ent %>%
            filter(Estado == input$Estado)
        print(data2)
        plot_ly(data2, type = 'scatter', mode = "lines")%>%
            add_trace(x = ~Year, y = ~Brecha, name = 'Brecha', line = list(color = 'purple', width = 4))%>%
            add_trace(x = ~Year, y = ~Explained, name = 'Explaied', line = list(color = 'magenta', width = 4))%>%
            add_trace(x = ~Year, y = ~Unexplained, name = 'Unexplained', line = list(color = 'pink', width = 4))%>%
            layout(showlegend = T, title='Descomposición de la brecha salarial de género',
                   xaxis = list(rangeslider = list(visible = T)))
    })
    
    output$plot3 <- renderPlotly({
        data <- oax_ent %>%
            filter(Year == input$Year)
        print(data)
        plot_ly(data, type = 'bar')%>%
            add_trace(x = ~Estado, y = ~Brecha, name = 'Brecha', marker = list(color = 'purple'))%>%
            layout(showlegend = F, title='Brecha salarial por estado',
                   xaxis = list(rangeslider = list(visible = F)))
    })
    
    output$plot4 <- renderPlotly({
        data <- oax_ent %>%
            filter(Year == input$Year) 
        print(data)
        plot_ly(data, type = 'bar')%>%
            add_trace(x = ~Estado, y = ~Unexplained, name = 'Unexplained', marker = list(color = 'pink'))%>%
            layout(showlegend = F, title='Discriminación',
                   xaxis = list(rangeslider = list(visible = F)))
    })
    
    output$plot5 <- renderPlotly({
        data <- oax_ent %>%
            filter(Year == input$Year) 
        print(data)
        plot_ly(data, type = 'bar')%>%
            add_trace(x = ~Estado, y = ~Explained, name = 'Explained', marker = list(color = 'magenta'))%>%
            layout(showlegend = F, title='Habilidades',
                   xaxis = list(rangeslider = list(visible = F)))
    })
    

    output$plot6 <- renderPlotly({
        data6 <- quantil %>%
            filter(y == input$anio) 
        plot_ly(data6, type = 'scatter', mode = 'lines+markers', marker = list(color = 'purple', size = 10))%>%
            add_trace(x = ~cuantil, y = ~b_q*-1, name = 'Brecha',line = list(color = 'purple', width = 4))%>%
            layout(showlegend = F, title='Estimadores por cuantil',
                   xaxis = list(title="Cuantil", rangeslider = list(visible = F)), yaxis = list(title = 'Estimador'))
        
    })
    
    output$plot7 <- renderPlotly({
        data7 <- quantil %>%
            filter(cuantil == input$quant) 
        plot_ly(data7, type = 'scatter', mode = 'lines+markers', marker = list(color = 'purple', size = 10))%>%
            add_trace(x = ~fecha, y = ~b_q*-1, name = '',line = list(color = 'purple', width = 4))%>%
            layout(showlegend = F, title='Estimadores por cuantil',
                   xaxis = list(title = "Fecha",rangeslider = list(visible = F)), yaxis = list(title = 'Estimador'))
        
    })
    
    
    output$plot8 <- renderPlotly({
        plot_ly(ols, type = 'scatter', mode = 'lines+markers', marker = list(color = 'purple', size = 10))%>%
            add_trace(x = ~fecha, y = ~b_ols*-1, name = '',line = list(color = 'purple', width = 4))%>%
            layout(showlegend = F, title='Estimadores OLS',
                   xaxis = list(title = "Fecha",rangeslider = list(visible = T)), yaxis = list(title = 'Estimador'))
        
    })
    
    output$plot9 <- renderPlotly({
            plot_ly(quantil, type = 'scatter', mode = 'markers',color=~cuantil, size = 10)%>%
            add_trace(x = ~y, y = ~b_q100, name = 'Estimador')%>%
            layout(showlegend = F, title='Estimadores por cuantil (2005 = 100)',
                   xaxis = list(title = 'Fecha',rangeslider = list(visible = F)), yaxis = list(title = 'Estimador'))
        
        
        
    })
    
    output$plot10 <- renderPlotly({
        
        plot_ly(part_mujeres, type = 'scatter', mode = 'markers', marker = list(color = 'purple', size = 10))%>%
            add_trace(x = ~fecha, y = ~(exp(par_b)-1)*100, name = 'Mujer con pareja', line = list(color = 'purple', width = 4))%>%
            layout(showlegend = F, title='Mujeres con pareja',
                   xaxis = list(title = 'Fecha',rangeslider = list(visible = F)), yaxis = list(title = 'Prob. respecto a mujeres solteras'))
        
        
        
    })
    
    output$plot11 <- renderPlotly({
        plot_ly(part_mujeres, type = 'scatter', mode = 'markers', marker = list(color = 'magenta', size = 10))%>%
            add_trace(x = ~fecha, y = ~(exp(hijo_b)-1)*100, name = 'Mujer con hijos', line = list(color = 'magenta', width = 4))%>%
            layout(showlegend = F, title='Mujeres con hijos',
                   xaxis = list(title = 'Fecha',rangeslider = list(visible = F)), yaxis = list(title = 'Prob. respecto a mujeres sin hijos'))
        
        
    })
    
    output$plot12 <- renderPlotly({
        data12 <- part_mujeres %>%
            filter(y == input$pls)
        plot_ly(data12, type = 'bar')%>%
            add_trace(x = ~q, y = ~(exp(hijo_b)-1)*100, name = 'Mujer con hijos', marker = list(color = 'magenta'))%>%
            add_trace(x = ~q, y = ~(exp(par_b)-1)*100, name = 'Mujer con pareja', marker = list(color = 'purple'))%>%
            layout(showlegend = T, title='Probabilidad de participación',
                   xaxis = list(title = 'Trimestre'), yaxis = list(title = 'Probabilidad'))
        
    })
    
    
    output$plot13 <- renderPlotly({
        plot_ly(formal_mujeres, type = 'scatter', mode = 'markers', marker = list(color = 'magenta', size = 10))%>%
            add_trace(x = ~fecha, y = ~(exp(par_b)-1)*100, name = 'Mujer con pareja', line = list(color = 'magenta', width = 4))%>%
            layout(showlegend = F, title='Mujer con pareja',
                   xaxis = list(title = 'Fecha',rangeslider = list(visible = F)), yaxis = list(title = 'Prob. respecto a mujeres sin pareja'))
        
        
    })
    
    output$plot14 <- renderPlotly({
        plot_ly(formal_mujeres, type = 'scatter', mode = 'markers', marker = list(color = 'purple', size = 10))%>%
            add_trace(x = ~fecha, y = ~(exp(hijo_b)-1)*100, name = 'Mujer con hijos', line = list(color = 'purple', width = 4))%>%
            layout(showlegend = F, title='Mujeres con hijos',
                   xaxis = list(title = 'Fecha',rangeslider = list(visible = F)), yaxis = list(title = 'Prob. respecto a mujeres sin hijos'))
        
        
    })

    output$plot15 <- renderPlotly({
        data12 <- formal_mujeres %>%
            filter(y == input$formalix)
        plot_ly(data12, type = 'bar')%>%
            add_trace(x = ~q, y = ~(exp(hijo_b)-1)*100, name = 'Mujer con hijos', marker = list(color = 'purple'))%>%
            add_trace(x = ~q, y = ~(exp(par_b)-1)*100, name = 'Mujer con pareja', marker = list(color = 'magenta'))%>%
            layout(showlegend = T, title='Probabilidad de formalidad',
                   xaxis = list(title = 'Trimestre'), yaxis = list(title = 'Probabilidad'))
        
        
    })
    

    output$plot16 <- renderPlotly({
        plot_ly(formal, type = 'scatter', mode = 'markers')%>%
            add_trace(x = ~fecha, y = ~(exp(fem0_b)-1)*100, name = 'Mujer sin pareja', line = list(color = 'purple', width = 4),  marker = list(color = 'purple', size = 10))%>%
            add_trace(x = ~fecha, y = ~(exp(fem1_b)-1)*100, name = 'Mujer con pareja', line = list(color = 'magenta', width = 4),  marker = list(color = 'magenta', size = 10))%>%
            add_trace(x = ~fecha, y = ~(exp(mal1_b)-1)*100, name = 'Hombre con pareja', line = list(color = 'blue', width = 4),  marker = list(color = 'blue', size = 10))%>%
            layout(showlegend = T, title='Probabilidad de formalidad',
                   xaxis = list(title = 'Fecha',rangeslider = list(visible = F)), yaxis = list(title = 'Prob. respecto a hombres sin pareja'))
        
        
    })
    
    output$plot17 <- renderPlotly({
        data12 <- formal %>%
            filter(y == input$formalix2)
        plot_ly(data12, type = 'bar')%>%
            add_trace(x = ~q, y = ~(exp(fem0_b)-1)*100, name = 'Mujer sin pareja', marker = list(color = 'purple'))%>%
            add_trace(x = ~q, y = ~(exp(fem1_b)-1)*100, name = 'Mujer con pareja', marker = list(color = 'magenta'))%>%
            add_trace(x = ~q, y = ~(exp(mal1_b)-1)*100, name = 'Hombre con pareja', marker = list(color = 'blue'))%>%
            layout(showlegend = T, title='Probabilidad de formalidad',
                   xaxis = list(title = 'Trimestre'), yaxis = list(title = 'Prob. respecto a hombres sin pareja'))
        
        
    })
    
    

    output$plot18 <- renderPlotly({
        plot_ly(part, type = 'scatter', mode = 'markers')%>%
            add_trace(x = ~fecha, y = ~(exp(fem0_b)-1)*100, name = 'Mujer sin pareja', line = list(color = 'purple', width = 4),  marker = list(color = 'purple', size = 10))%>%
            add_trace(x = ~fecha, y = ~(exp(fem1_b)-1)*100, name = 'Mujer con pareja', line = list(color = 'magenta', width = 4),  marker = list(color = 'magenta', size = 10))%>%
            add_trace(x = ~fecha, y = ~(exp(mal1_b)-1)*100, name = 'Hombre con pareja', line = list(color = 'blue', width = 4),  marker = list(color = 'blue', size = 10))%>%
            layout(showlegend = T, title='Probabilidad de formalidad',
                   xaxis = list(title = 'Fecha',rangeslider = list(visible = F)), yaxis = list(title = 'Prob. respecto a hombres sin pareja'))
        
        
    })
    
    output$plot19 <- renderPlotly({
        data12 <- part %>%
            filter(y == input$formalix3)
        plot_ly(data12, type = 'bar')%>%
            add_trace(x = ~q, y = ~(exp(fem0_b)-1)*100, name = 'Mujer sin pareja', marker = list(color = 'purple'))%>%
            add_trace(x = ~q, y = ~(exp(fem1_b)-1)*100, name = 'Mujer con pareja', marker = list(color = 'magenta'))%>%
            add_trace(x = ~q, y = ~(exp(mal1_b)-1)*100, name = 'Hombre con pareja', marker = list(color = 'blue'))%>%
            layout(showlegend = T, title='Probabilidad de formalidad',
                   xaxis = list(title = 'Trimestre'), yaxis = list(title = 'Prob. respecto a hombres sin pareja'))
        
        
    })
    

    output$plot20 <- renderPlotly({
        data12 <- formal_ent %>%
            filter(av_ent == input$entid)%>%
            filter(y == input$formalixxx)
        plot_ly(data12, type = 'bar')%>%
            add_trace(x = ~q, y = ~(exp(fem0_b)-1)*100, name = 'Mujer sin pareja', marker = list(color = 'purple'))%>%
            add_trace(x = ~q, y = ~(exp(fem1_b)-1)*100, name = 'Mujer con pareja', marker = list(color = 'magenta'))%>%
            add_trace(x = ~q, y = ~(exp(mal1_b)-1)*100, name = 'Hombre con pareja', marker = list(color = 'blue'))%>%
            layout(showlegend = T, title='Respecto a hombres solteros',
                   xaxis = list(title = 'Trimestre'), yaxis = list(title = 'Prob. respecto a hombres sin pareja'))
        
        
    })
    
    output$plot21 <- renderPlotly({
        data13 <- formal_entmujer %>%
            filter(av_ent == input$entid)%>%
            filter(y == input$formalixxx)
        plot_ly(data13, type = 'bar')%>%
            add_trace(x = ~q, y = ~(exp(par_b)-1)*100, name = 'Mujer con pareja', marker = list(color = 'purple'))%>%
            add_trace(x = ~q, y = ~(exp(hijo_b)-1)*100, name = 'Mujer con hijos', marker = list(color = 'magenta'))%>%
            layout(showlegend = T, title='Respecto a mujeres solteras o sin hijos',
                   xaxis = list(title = 'Trimestre'), yaxis = list(title = 'Probabilidad'))
        
        
    })
    
    
    output$plot22 <- renderPlotly({
        data13 <- formal_ent %>%
            filter(av_ent == input$entid)
        plot_ly(data13, type = 'scatter', mode = 'markers')%>%
            add_trace(x = ~fecha, y = ~(exp(fem0_b)-1)*100, name = 'Mujer sin pareja', line = list(color = 'purple', width = 4),  marker = list(color = 'purple', size = 10))%>%
            add_trace(x = ~fecha, y = ~(exp(fem1_b)-1)*100, name = 'Mujer con pareja', line = list(color = 'magenta', width = 4),  marker = list(color = 'magenta', size = 10))%>%
            add_trace(x = ~fecha, y = ~(exp(mal1_b)-1)*100, name = 'Hombre con pareja', line = list(color = 'blue', width = 4),  marker = list(color = 'blue', size = 10))%>%
            layout(showlegend = T, title='Respecto a hombres solteros',
                   xaxis = list(title = 'Fecha',rangeslider = list(visible = F)), yaxis = list(title = 'Prob. respecto a hombres sin pareja'))
        
        
    })
    
    output$plot23 <- renderPlotly({
        data13 <- formal_entmujer %>%
            filter(av_ent == input$entid)
        plot_ly(data13, type = 'scatter', mode = 'markers')%>%
            add_trace(x = ~fecha, y = ~(exp(par_b)-1)*100, name = 'Mujer con pareja', line = list(color = 'purple', width = 4),  marker = list(color = 'purple', size = 10))%>%
            add_trace(x = ~fecha, y = ~(exp(hijo_b)-1)*100, name = 'Mujer con hijos', line = list(color = 'magenta', width = 4),  marker = list(color = 'magenta', size = 10))%>%
            layout(showlegend = T, title='Respecto a mujeres solteras o sin hijos',
                   xaxis = list(title = 'Fecha',rangeslider = list(visible = F)), yaxis = list(title = 'Probabilidad'))
        
        
    })
    
    output$plot24 <- renderPlotly({
        data12 <- part_ent %>%
            filter(av_ent == input$entidx)%>%
            filter(y == input$formalixxxx)
        plot_ly(data12, type = 'bar')%>%
            add_trace(x = ~q, y = ~(exp(fem0_b)-1)*100, name = 'Mujer sin pareja', marker = list(color = 'purple'))%>%
            add_trace(x = ~q, y = ~(exp(fem1_b)-1)*100, name = 'Mujer con pareja', marker = list(color = 'magenta'))%>%
            add_trace(x = ~q, y = ~(exp(mal1_b)-1)*100, name = 'Hombre con pareja', marker = list(color = 'blue'))%>%
            layout(showlegend = T, title='Respecto a hombres solteros',
                   xaxis = list(title = 'Trimestre'), yaxis = list(title = 'Prob. respecto a hombres sin pareja'))
        
        
    })
    
    
    output$plot25 <- renderPlotly({
        data13 <- part_entmujer %>%
            filter(av_ent == input$entidx)%>%
            filter(y == input$formalixxxx)
        plot_ly(data13, type = 'bar')%>%
            add_trace(x = ~q, y = ~(exp(par_b)-1)*100, name = 'Mujer con pareja', marker = list(color = 'purple'))%>%
            add_trace(x = ~q, y = ~(exp(hijo_b)-1)*100, name = 'Mujer con hijos', marker = list(color = 'magenta'))%>%
            layout(showlegend = T, title='Respecto a mujeres solteras o sin hijos',
                   xaxis = list(title = 'Trimestre'), yaxis = list(title = 'Probabilidad'))
        
        
    })
    
    output$plot26 <- renderPlotly({
        data13 <- part_ent %>%
            filter(av_ent == input$entidx)
        plot_ly(data13, type = 'scatter', mode = 'markers')%>%
            add_trace(x = ~fecha, y = ~(exp(fem0_b)-1)*100, name = 'Mujer sin pareja', line = list(color = 'purple', width = 4),  marker = list(color = 'purple', size = 10))%>%
            add_trace(x = ~fecha, y = ~(exp(fem1_b)-1)*100, name = 'Mujer con pareja', line = list(color = 'magenta', width = 4),  marker = list(color = 'magenta', size = 10))%>%
            add_trace(x = ~fecha, y = ~(exp(mal1_b)-1)*100, name = 'Hombre con pareja', line = list(color = 'blue', width = 4),  marker = list(color = 'blue', size = 10))%>%
            layout(showlegend = T, title='Respecto a hombres solteros',
                   xaxis = list(title = 'Fecha',rangeslider = list(visible = F)), yaxis = list(title = 'Prob. respecto a hombres sin pareja'))
        
        
    })
    
    output$plot27 <- renderPlotly({
        data13 <- part_entmujer %>%
            filter(av_ent == input$entidx)
        plot_ly(data13, type = 'scatter', mode = 'markers')%>%
            add_trace(x = ~fecha, y = ~(exp(par_b)-1)*100, name = 'Mujer con pareja', line = list(color = 'purple', width = 4), marker = list(color = 'purple', size = 10))%>%
            add_trace(x = ~fecha, y = ~(exp(hijo_b)-1)*100, name = 'Mujer con hijos', line = list(color = 'magenta', width = 4),  marker = list(color = 'magenta', size = 10))%>%
            layout(showlegend = T, title='Respecto a mujeres solteras o sin hijos',
                   xaxis = list(title = 'Fecha',rangeslider = list(visible = F)), yaxis = list(title = 'Probabilidad'))
        
        
    })
    
    
    output$plot28 <- renderPlotly({
        data13 <- quant_region %>%
            filter(name_reg == input$region)%>%
            filter(year == input$regiondate)
        plot_ly(data13, type = 'scatter', mode = 'markers')%>%
            add_trace(x = ~cuantil, y = ~b_qfem*-1, name = '', line = list(color = 'purple', width = 4), marker = list(color = 'purple', size = 10))%>%
            layout(showlegend = F, title='Estimador por cuantil',
                   xaxis = list(title = 'Fecha',rangeslider = list(visible = F)), yaxis = list(title = 'Estimador'))
        
    })
    
    output$plot29 <- renderPlotly({
        data13 <- quant_region %>%
            filter(year == input$regiondate)
        plot_ly(data13, type = 'scatter', mode = 'markers', color =~cuantil, size = 1000)%>%
            add_trace(x = ~name_reg, y = ~b_qfem*-1, name = '')%>%
            layout(showlegend = F, title='Estimador por cuantil',
                   xaxis = list(title = '',rangeslider = list(visible = F)), yaxis = list(title = ''))
        
    })
    
    output$plot30 <- renderPlotly({
        data13 <- quant_region %>%
            filter(name_reg == input$region)%>%
            filter(cuantil == input$cuants2)
        plot_ly(data13, type = 'scatter', mode = 'markers')%>%
            add_trace(x = ~fecha, y = ~b_qfem*-1, name = '',line = list(color = 'purple', width = 4), marker = list(color = 'purple', size = 10))%>%
            layout(showlegend = F, title='Estimador por cuantil',
                   xaxis = list(title = '',rangeslider = list(visible = F)), yaxis = list(title = ''))
        
    })
    
    output$plot31 <- renderPlotly({
        data13 <- quant_region %>%
            filter(name_reg == input$region)
        plot_ly(data13, type = 'scatter', mode = 'markers',color=~cuantil, size = 10)%>%
            add_trace(x = ~year, y = ~b_qfem100, name = 'Estimador')%>%
            layout(showlegend = F, title='Estimadores por cuantil (2005 = 100)',
                   xaxis = list(title = 'Fecha',rangeslider = list(visible = F)), yaxis = list(title = 'Estimador'))
        
        
    })
    
    output$map1 <- renderPlotly({
        dfxx <- mapox%>%
            filter(Year== input$maps1)
        ggplotly(mxhexbin_choropleth(dfxx, num_colors = 1,
                                     title = "Discriminación")+
                     paletteer::scale_fill_paletteer_c("grDevices::Dynamic", 
                                                       name='(%)',  limits = c(-25, 25))+
                     theme_void()
        )
        
        
        
    })
    
    output$map2 <- renderPlotly({
        dfxx <- mapox2%>%
            filter(Year== input$maps1)
        ggplotly(mxhexbin_choropleth(dfxx, num_colors = 1,
                                     title = "Habilidades")+
                     paletteer::scale_fill_paletteer_c("grDevices::Dynamic", 
                                                       name='(%)',  limits = c(-25, 20))+
                     theme_void()
        )
        
        
        
    })
    
    output$map3 <- renderPlotly({
        dfxx <- mapox3%>%
            filter(Year== input$maps1)
        ggplotly(mxhexbin_choropleth(dfxx, num_colors = 1,
                                     title = "Brecha salarial")+
                     paletteer::scale_fill_paletteer_c("grDevices::Dynamic", 
                                                       name='(%)',  limits = c(-20, 27))+
                     theme_void()
        )
        
        
        
    })
    
    output$map4 <- renderPlotly({
        dfxx <- mappart%>%
            filter(y== input$partmap)%>%
            filter(q == input$partmapq)
        ggplotly(mxhexbin_choropleth(dfxx, num_colors = 1,
                                     title = "Mujer con pareja")+
                     paletteer::scale_fill_paletteer_c("grDevices::Dynamic", 
                                                       name='(%)')+
                     theme_void()
        )
        
        
        
    })
    
    output$map5 <- renderPlotly({
        dfxx <- mappart2%>%
            filter(y== input$partmap)%>%
            filter(q == input$partmapq)
        ggplotly(mxhexbin_choropleth(dfxx, num_colors = 1,
                                     title = "Mujer sin pareja")+
                     paletteer::scale_fill_paletteer_c("grDevices::Dynamic", 
                                                       name='(%)')+
                     theme_void()
        )
        
        
        
    })
    
    
    output$map6 <- renderPlotly({
        dfxx <- mappart3%>%
            filter(y== input$partmap)%>%
            filter(q == input$partmapq)
        ggplotly(mxhexbin_choropleth(dfxx, num_colors = 1,
                                     title = "Hombre con pareja")+
                     paletteer::scale_fill_paletteer_c("grDevices::Dynamic", 
                                                       name='(%)')+
                     theme_void()
        )
        
        
        
    })
    
    
    output$map7 <- renderPlotly({
        dfxx <- mapfor%>%
            filter(y== input$formap)%>%
            filter(q == input$formapq)
        ggplotly(mxhexbin_choropleth(dfxx, num_colors = 1,
                                     title = "Mujer con pareja")+
                     paletteer::scale_fill_paletteer_c("grDevices::Dynamic", 
                                                       name='(%)')+
                     theme_void()
        )
        
        
        
    })
    
    output$map8 <- renderPlotly({
        dfxx <- mappart22%>%
            filter(y== input$partmap)%>%
            filter(q == input$partmapq)
        ggplotly(mxhexbin_choropleth(dfxx, num_colors = 1,
                                     title = "Mujer con pareja")+
                     paletteer::scale_fill_paletteer_c("grDevices::Dynamic", 
                                                       name='(%)')+
                     theme_void()
        )
        
        
        
    })
    
    output$map9 <- renderPlotly({
        dfxx <- mappart222%>%
            filter(y== input$partmap)%>%
            filter(q == input$partmapq)
        ggplotly(mxhexbin_choropleth(dfxx, num_colors = 1,
                                     title = "Mujer con hijos")+
                     paletteer::scale_fill_paletteer_c("grDevices::Dynamic", 
                                                       name='(%)')+
                     theme_void()
        )
        
        
        
    })
    
    output$map10 <- renderPlotly({
        dfxx <- mapfor2%>%
            filter(y== input$formap)%>%
            filter(q == input$formapq)
        ggplotly(mxhexbin_choropleth(dfxx, num_colors = 1,
                                     title = "Hombre con pareja")+
                     paletteer::scale_fill_paletteer_c("grDevices::Dynamic", 
                                                       name='(%)')+
                     theme_void()
        )
    })
    
        output$map11 <- renderPlotly({
            dfxx <- mapfor3%>%
                filter(y== input$formap)%>%
                filter(q == input$formapq)
            ggplotly(mxhexbin_choropleth(dfxx, num_colors = 1,
                                         title = "Hombre con pareja")+
                         paletteer::scale_fill_paletteer_c("grDevices::Dynamic", 
                                                           name='(%)')+
                         theme_void()
            )
        })
            
            
        
        output$map12 <- renderPlotly({
            dfxx <- mapfor22%>%
                filter(y== input$formap)%>%
                filter(q == input$formapq)
            ggplotly(mxhexbin_choropleth(dfxx, num_colors = 1,
                                         title = "Mujer con pareja")+
                         paletteer::scale_fill_paletteer_c("grDevices::Dynamic", 
                                                           name='(%)')+
                         theme_void()
            )
            
            
            
        })
        
        output$map13 <- renderPlotly({
            dfxx <- mapfor222%>%
                filter(y== input$formap)%>%
                filter(q == input$formapq)
            ggplotly(mxhexbin_choropleth(dfxx, num_colors = 1,
                                         title = "Mujer con hijos")+
                         paletteer::scale_fill_paletteer_c("grDevices::Dynamic", 
                                                           name='(%)')+
                         theme_void()
            )
            
            
            
        })
        
        
        output$table <- DT::renderDT({
            quantil %>%
                filter(y== input$aniot)
        })
        
        output$table1 <- DT::renderDT({
            quant_region %>%
                filter(year== input$aniot2)
        })
        
        output$tabl2 <- DT::renderDT({
            oax 
        })
        
        output$tabl3 <- DT::renderDT({
            oax_ent %>%
            filter(Year == input$maps1)
        })
        
        
        
})

