  shinyUI(navbarPage(theme = shinytheme('cerulean'), 'Analiza danych COVID-19',
tabPanel('Dane',
         sidebarLayout(
           sidebarPanel(h4('Wybierz filtry do tabeli'),
                           sliderInput('dateRep',
                                       label = h5('Data:'),
                                       min = as.Date('2020-01-06'),
                                       max = as.Date(max(dane3$dateRep)),
                                       step = 7,
                                      value = c(as.Date('2020-01-06'),as.Date(max(dane3$dateRep)))),
                           checkboxGroupInput('continent1',
                                              label = h3('kontynent:'),
                                              choices = unique(dane3$continent),
                                              selected = unique(dane3$continent)),
                        h6('* Dane do analizy pochodza ze 148 krajow',  align = 'right')
                        
                        ),
           mainPanel(
                            DT::dataTableOutput('mytable1')
                    )
                    
                    )
          ),


navbarMenu("Mapy",
           tabPanel('Wszystkie przypadki COVID-19',
                    sidebarLayout(
                      sidebarPanel(
                        h3(textOutput('reactive_sumcases')),
                        h5(textOutput('reactive_sumcases_weekly')),
                        plotOutput('plot1',  height = 225, width = 450),
                        plotOutput('plot2', height = 225, width = 450),
                        sliderInput('dateRep3',
                                    label = h5('Data:'),
                                    min = as.Date('2020-01-06'),
                                    max = as.Date(max(dane3$dateRep)),
                                    step = 7,
                                    value = c(as.Date('2020-01-06'),as.Date(max(dane3$dateRep)))
                                    
                        ),
                        h6('* Liczba przypadkow ze 148 krajow',  align = 'right')
                        
                        
                      ),
                      mainPanel(h3('Liczba wszystkich przypadkow zachorowan na COVID-19'),
                                htmlOutput("mapka")
                      )
                      
                    )
           ),
           
           tabPanel('Zgony na COVID-19',
                    sidebarLayout(
                      sidebarPanel(
                        h3(textOutput('reactive_sumdeaths')),
                        h5(textOutput('reactive_sumdeaths_weekly')),
                        plotOutput('plot3',  height = 225, width = 450),
                        plotOutput('plot4', height = 225, width = 450),
                        sliderInput('dateRep4',
                                    label = h5('Data:'),
                                    min = as.Date('2020-01-06'),
                                    max = as.Date(max(dane3$dateRep)),
                                    step = 7,
                                    value = c(as.Date('2020-01-06'),as.Date(max(dane3$dateRep)))
                                    
                        ),
                        h6('* Liczba zgonow ze 148 krajow',  align = 'right')
                        
                      ),
                      mainPanel(h3('Liczba wszystkich zgonow na COVID-19'),
                                htmlOutput("mapka2")
                      )
                      
                    )
           )
           
           
           
),



navbarMenu("Porownania",
tabPanel("Porownanie skumulowanych wartosci", 
         sidebarLayout(
         sidebarPanel(h4("Wybierz filtry"),
                      pickerInput('country1',
                                  label = h4('Kraje:'),
                                  choices = unique(dane3$country),
                                  choicesOpt = list(content = unique(dane3$xx)),
                                  multiple = TRUE,
                                  options = pickerOptions( 
                                                 liveSearch = TRUE)
                                  ),
                      sliderInput('dateRep6',
                                  label = h4('Data:'),
                                  min = as.Date('2020-01-06'),
                                  max = as.Date(max(dane3$dateRep)),
                                  step = 7,
                                  value = c(as.Date('2020-01-06'),as.Date(max(dane3$dateRep))))
                      ),
                      mainPanel(tags$style(type="text/css",
                                           ".shiny-output-error { visibility: hidden; }",
                                           ".shiny-output-error:before { visibility: hidden; }"),
                        plotlyOutput("wykres1"),
                        plotlyOutput('wykres2'))
                      )
        ),

tabPanel("Porownanie tygodniowych danych", 
         sidebarLayout(
           sidebarPanel(h4("Wybierz kraje do porownania"),
                        pickerInput('country3',
                                    label = h4('Kraje:'),
                                    choices = unique(dane3$country),
                                    choicesOpt = list(content = unique(dane3$xx)),
                                    multiple = TRUE,
                                    options = pickerOptions(
                                      liveSearch = TRUE)
                        ),
                        sliderInput('dateRep7',
                                    label = h4('Data:'),
                                    min = as.Date('2020-01-06'),
                                    max = as.Date(max(dane3$dateRep)),
                                    step = 7,
                                    value = c(as.Date('2020-01-06'),as.Date(max(dane3$dateRep))))
                        ),
           mainPanel(tags$style(type="text/css",
                                ".shiny-output-error { visibility: hidden; }",
                                ".shiny-output-error:before { visibility: hidden; }"),
                     plotlyOutput("wykres3"),
                      plotlyOutput('wykres4'))
         )
),

tabPanel("Porownanie sredniej liczby zachorowan na 100tys mieszkancow z 14 dni ", 
         sidebarLayout(
           sidebarPanel(h4("Wybierz filtry"),
                        selectInput(inputId = 'dateRep5',
                                    label = h3('data:'),
                                    choices = unique(dane3$dateRep),
                                    selected = max(dane3$dateRep),
                                    multiple = FALSE
                          
                        ),
                        radioButtons('continent2',
                                           label = h3('kontynent:'),
                                           choices = unique(dane3$continent),
                                           selected = 'Europe')),
           mainPanel(tags$style(type="text/css",
                                ".shiny-output-error { visibility: hidden; }",
                                ".shiny-output-error:before { visibility: hidden; }"),
                     htmlOutput("reactive_gauge")
         ))
),
tabPanel("Porownanie wskaznikow na milion mieszkancow", 
         sidebarLayout(
           sidebarPanel(h4("Wybierz filtry"),
                        selectInput(inputId = 'dateRep8',
                                    label = h3('data:'),
                                    choices = unique(dane3$dateRep),
                                    selected = max(dane3$dateRep),
                                    multiple = FALSE
                                    
                        ),
                        radioButtons('continent3',
                                     label = h3('kontynent:'),
                                     choices = unique(dane3$continent),
                                     selected = 'Europe')),
           mainPanel(tags$style(type="text/css",
                                ".shiny-output-error { visibility: hidden; }",
                                ".shiny-output-error:before { visibility: hidden; }"),
                     plotlyOutput("plot5"),
                     plotlyOutput('plot6')
           ))
)



)




)
)
                   


    
        