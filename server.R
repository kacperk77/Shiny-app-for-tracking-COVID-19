shinyServer(function(input, output, session) {
  
   


daneReactive_plot1 <- reactive({dane3 %>% 
      filter(country %in% input$country1 &
               between(dateRep, input$dateRep6[1], input$dateRep6[2])) 
    
  }) 


daneReactive_plot2 <- reactive({dane3 %>% 
    filter(country %in% input$country3 &
             between(dateRep, input$dateRep7[1], input$dateRep7[2])) 
  
}) 



daneReactive_plot5 <- reactive({dane3 %>% filter(between(dateRep, input$dateRep3[1], input$dateRep3[2])) %>% 
    dplyr::group_by(dateRep) %>% summarize(sum = sum(all_cases)/1000000)
  
})

daneReactive_plot6 <- reactive({dane3 %>% filter(between(dateRep, input$dateRep3[1], input$dateRep3[2])) %>% 
    dplyr::group_by(dateRep) %>% summarize(sum = sum(cases_weekly)/1000000)
  
})

daneReactive_plot7 <- reactive({dane3 %>% filter(between(dateRep, input$dateRep4[1], input$dateRep4[2])) %>% 
    dplyr::group_by(dateRep) %>% summarize(sum = sum(all_deaths)/1000)
  
})

daneReactive_plot8 <- reactive({dane3 %>% filter(between(dateRep, input$dateRep4[1], input$dateRep4[2])) %>% 
    dplyr::group_by(dateRep) %>% summarize(sum = sum(deaths_weekly)/1000)
  
})



daneReactive_table <- reactive({dane4 %>% 
    filter(between(dateRep, input$dateRep[1], input$dateRep[2]) & continent %in% input$continent1) 
})

 
 
 output$mytable1 <- DT::renderDataTable({
   DT::datatable(daneReactive_table(), escape = FALSE, caption = 'Zrodlo danych: EU Open Data Portal',  options = list(orderClasses = TRUE,
                                       pageLength = 15, 
                                       order = list(list(0,'desc'))), rownames = FALSE)
 })
 

  


output$wykres1 <- renderPlotly({
  req(nrow(daneReactive_plot1()) > 0)
  ggplotly(ggplot(data = daneReactive_plot1(), mapping = aes(x = dateRep, y = all_cases, color = country))+
    geom_line(size = 1.1)+
    scale_x_date(date_breaks = "months" , date_labels = "%b")+
    ggtitle('Skumulowana liczba zachorowan na COVID-19')+
    xlab('Miesiac')+ylab('Ilosc przypadkow zachorowania')+
      labs(colour='Panstwa')+
      theme(
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "grey"))
      
    )
})  


output$wykres2 <- renderPlotly({
  req(nrow(daneReactive_plot1()) > 0)
  ggplotly(ggplot(data = daneReactive_plot1(), mapping = aes(x = dateRep, y = all_deaths, color = country))+
             geom_line(size = 1.1)+
             scale_x_date(date_breaks = "months" , date_labels = "%b")+
             ggtitle('Skumulowana liczba zgonow na COVID-19')+
             xlab('Miesiac')+ylab('Ilosc zgonow na COVID-19')+
             theme(
               panel.border = element_blank(),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.background = element_blank(),
               axis.line = element_line(colour = "grey"))+ labs(colour='Panstwa')
           )
})  


output$wykres3 <- renderPlotly({
  req(nrow(daneReactive_plot2()) > 0)
  ggplotly(ggplot(data = daneReactive_plot2(), mapping = aes(x = dateRep, y = cases_weekly, fill = country))+
             geom_col(alpha = 0.5, position = 'dodge', col = 'black')+
             scale_x_date(date_breaks = "months" , date_labels = "%b")+
             ggtitle('Tygodniowa liczba nowych przypadkow COVID-19')+
             xlab('Miesiac')+ylab('Nowe przypadki COVID-19')+
    theme(
      panel.border = element_blank(), 
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "grey"))+ guides(fill=guide_legend(title="Panstwa"))
    )
}) 


output$wykres4 <- renderPlotly({
  req(nrow(daneReactive_plot2()) > 0)
  ggplotly(ggplot(data = daneReactive_plot2(), mapping = aes(x = dateRep, y = deaths_weekly, fill = country))+
             geom_col(alpha = 0.5, position = 'dodge', col = 'black')+
             scale_x_date(date_breaks = "months" , date_labels = "%b")+
             ggtitle('Tygodniowa liczba zgonow na COVID-19')+
             xlab('Miesiac')+ylab('Zgony na COVID-19')+
             theme(
               panel.border = element_blank(),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.background = element_blank(),
               axis.line = element_line(colour = "grey"))+ guides(fill=guide_legend(title="Panstwa"))
           )
}) 






output$mapka <- renderGvis({gvisGeoChart(dane3 <- dane3 %>% filter(between(dateRep, input$dateRep3[1], input$dateRep3[2])) %>% 
                                           group_by(country) %>% dplyr::summarize(sum = sum(cases_weekly)),
                                         locationvar =  "country",
                                         colorvar = "sum",
                                         options = list(
                                           width=900,height=650,
                                                        colorAxis="{colors:['#AED6F1', '#2874A6']}"
                                                                                            ))
                                         



})


output$reactive_sumcases <- renderText({
  paste0(prettyNum(dane3 %>% filter(between(dateRep, input$dateRep3[1], input$dateRep3[2])) %>%
                     summarize(sum = sum(cases_weekly)) %>% select(sum),  big.mark=","), ' przypadkow')
  
})


output$reactive_sumcases_weekly <- renderText({
  paste0(prettyNum(sum(dane3 %>% filter(dateRep == input$dateRep3[2]) %>% select(cases_weekly)),  big.mark=","), ' przypadkow w wybranym tygodniu ', 
         prettyNum(unique(dane3 %>% filter(dateRep == input$dateRep3[2]) %>% select(dateRep))))
  
})




output$mapka2 <- renderGvis({gvisGeoChart(dane3 <- dane3 %>% filter(between(dateRep, input$dateRep4[1], input$dateRep4[2])) %>% 
                                            group_by(country) %>% dplyr::summarize(sum = sum(deaths_weekly)),
                                         locationvar =  "country",
                                         colorvar = "sum",
                                         options = list(
                                           width=900,height=650,
                                           colorAxis="{colors:['#D5D8DC', '#17202A']}"
                                         ))
  
  
  
  
})



output$plot1 <- renderPlot({ggplot(daneReactive_plot5(),
                           aes(x = dateRep, y = sum))+ geom_line(col = 'dodgerblue2')+geom_point(col = 'dodgerblue2')+
    scale_x_date(date_breaks = "months" , date_labels = "%b")+xlab('Miesiac')+
    ylab('Liczba przypadkow (w mln)')+
    theme(plot.background = element_rect(fill = "grey97"))
})


output$plot2 <- renderPlot({ggplot(daneReactive_plot6(),
                                   aes(x = dateRep, y = sum))+ geom_line(col = 'dodgerblue2')+geom_point(col = 'dodgerblue2')+
    scale_x_date(date_breaks = "months" , date_labels = "%b")+xlab('Miesiac')+
    ylab('Tygodniowa liczba przypadkow (w mln)')+
    theme(plot.background = element_rect(fill = "grey97"))
})

output$plot3 <- renderPlot({ggplot(daneReactive_plot7(),
                                   aes(x = dateRep, y = sum))+ geom_line(col = '#17202A')+geom_point(col = '#17202A')+
    scale_x_date(date_breaks = "months" , date_labels = "%b")+xlab('Miesiac')+
    ylab('Liczba zgonow (w tys)')+
    theme(plot.background = element_rect(fill = "grey97"))
})

output$plot4 <- renderPlot({ggplot(daneReactive_plot8(),
                                   aes(x = dateRep, y = sum))+
    geom_line(col = '#17202A')+
    geom_point(col = '#17202A')+
    scale_x_date(date_breaks = "months" , date_labels = "%b")+xlab('Miesiac')+
    ylab('Tygodniowa liczba zgonow (w tys)')+
    theme(plot.background = element_rect(fill = "grey97"))
})


output$reactive_sumdeaths <- renderText({
  paste0(prettyNum(dane3 %>% filter(between(dateRep, input$dateRep4[1], input$dateRep4[2])) %>%
                     summarize(sum = sum(deaths_weekly)) %>% select(sum),  big.mark=","), ' zgonow')
  
})


output$reactive_sumdeaths_weekly <- renderText({
  paste0(prettyNum(sum(dane3 %>% filter(dateRep == input$dateRep4[2]) %>% select(deaths_weekly)),  big.mark=","), ' zgonow na COVID-19 w wybranym tygodniu ',
         prettyNum(unique(dane3 %>% filter(dateRep == input$dateRep4[2]) %>% select(dateRep))))
  
})


output$reactive_report_date2 <- renderText({
  paste0(prettyNum(unique(dane3 %>% filter(dateRep == input$dateRep4[2]) %>% select(dateRep))))
  
})


output$reactive_gauge <- renderGvis({gvisGauge(dane3 %>% filter(continent %in% input$continent2  &
                                                                dateRep == input$dateRep5 
                                                                  ) %>% 
                                                 select(country, notification_rate_per_100000_population_14.days, continent) %>% 
                                                 select(country, notification_rate_per_100000_population_14.days) %>% 
                                                 arrange(desc(notification_rate_per_100000_population_14.days)),
                                              labelvar = 'country',
                                              numvar = 'notification_rate_per_100000_population_14.days',
                                              options=list(fontSize=5,
                                                min=0, max=1200,
                                                           redFrom=500,redTo=1200,
                                                         yellowFrom=300, yellowTo=500,
                                                           greenFrom=0, greenTo=300,
                                                           width=950, height=950))
  
  
  
})








output$plot5 <- renderPlotly({
  
  
  
  ggplotly(ggplot(dane3 %>% filter(dateRep == input$dateRep8 &  continent %in% input$continent3  ) %>% 
           arrange(desc(all_cases_per_1million)) %>% head(10), aes(x = reorder(country, desc(all_cases_per_1million)),
                                                                   y = all_cases_per_1million, fill = country))+
    geom_col(col = 'black')+xlab('Kraj')+ylab('liczba przypadkow na milion mieszkancow')+
    ggtitle('Top 10 panstw pod wzgledem liczby przypadkow na milion mieszkancow')+
    theme(
      panel.border = element_blank(), 
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "grey"),
                               legend.position = "none"), tooltip = c("fill", "y")
  )
  
  
  
  })

output$plot6 <- renderPlotly({
  
  
  
  ggplotly(ggplot(dane3 %>% filter(dateRep == input$dateRep8 &  continent %in% input$continent3) %>% 
           arrange(desc(all_deaths_per_1million)) %>% head(10), aes(x = reorder(country, desc(all_deaths_per_1million)),
                                                                   y = all_deaths_per_1million, fill = country))+
    geom_col(col = 'black')+xlab('Kraj')+ylab('liczba zgonow na milion mieszkancow')+
    ggtitle('Top 10 panstw pod wzgledem liczby zgonow na COVID-19 na milion mieszkancow')+
      theme(
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "grey"),
        legend.position = "none"), tooltip = c("fill", "y")
    
    
    
    
    )
  
  
})







})
