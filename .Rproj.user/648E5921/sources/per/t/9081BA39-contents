

# Define UI for application that draws a histogram
dashboardPage(
    skin = "red",
    dashboardHeader(title = 'Chicago Crimes'),
    
    dashboardSidebar(

        sidebarMenu( 
            menuItem('Introduccion', tabName = 'intro',icon = icon('play-circle')),
            menuItem('Crimes Summary', tabName = 'summary', icon = icon('chart-bar')),
            menuItem('Crimes Table', tabName = 'table', icon = icon('database')),
            menuItem('Crimes Heat Maps', tabName = 'map', icon = icon('globe-americas')),
            menuItem('About us', tabName = 'about', icon = icon('id-card'))
        )
        
    ),
    dashboardBody(
        
        
        tabItems(
            # Tab de introduccion ---
            
            tabItem(
                
                tabName = 'intro',
                # h1('Aqui va nuestra intro')
                includeHTML('www/html/intro.html')
                
            ),
            
            # Tab summary ---
            
            tabItem(
                tabName = 'summary',
                #Row Totales---
                fluidRow(
                    column(3,
                           valueBoxOutput('delitosTotales', width = 12)
                    ),
                    column(3,
                           valueBoxOutput('delitosGraves', width = 12)
                    ),
                    column(3,
                           valueBoxOutput('delitosMedios', width = 12)
                    ),
                    column(3,
                           valueBoxOutput('delitosBajos', width = 12)
                    )
                    
                    ),
                
                # Row selección tipo de grafico ----
                
                fluidRow(

                    column(12,

                           checkboxGroupButtons(
                               inputId = "graficos",
                               label = "Click to Choose a graph:",
                               choices = c(`<i class='fa fa-bar-chart'></i>` = "bar", `<i class='fa fa-line-chart'></i>` = "line"),
                               justified = TRUE,
                               status = "danger"
                   
                           ),
                          

                           column(6,

                                  uiOutput('uiBar')

                                  ),

                           column(6,

                                  uiOutput('uiLine')

                           ),
                           
                           fluidRow(column(6),
                                    column(6, helpText("To update the dates use the button above"))
                               
                               
                           ),
                           fluidRow(
                               column(6,
                                      conditionalPanel("input.graficos == 'bar'",
                                                       plotlyOutput('crimesBar'))
                                      ),
                               column(6,
                                      conditionalPanel("input.graficos== 'line'",
                                                       plotlyOutput('crimesLine'))
                                      )
                               )
                           )
                    )
                ),
            #tab table
            
            tabItem(tabName = 'table',
                    #row
                    
                    fluidRow(
                        column(2,
                               uiOutput('horas_dia')

                        ),
                        
                        column(10,
                               
                               withLoader(plotlyOutput('hist_hours'),
                                          type="html",
                                          loader="pacman") 
                                

                        ),
                    ),
                    
                        
                    fluidRow(
                        column(12,
                               withLoader(DTOutput('TablaCrimen'),
                                          type="html",
                                          loader="pacman"))
                    )
                
                
            ),
            #tab Map---
            
            tabItem(tabName = 'map',
                    
                    #rows 
                    
                    fluidRow(
                        column(3,
                               valueBoxOutput('Totalmap', width = 12)
                        ),
                        column(3,
                               valueBoxOutput('highmap', width = 12)
                        ),
                        column(3,
                               valueBoxOutput('mediummap', width = 12)
                        ),
                        column(3,
                               valueBoxOutput('lowmap', width = 12)
                        )
                        
                    ),
                    
                    #row map
                    
                    fluidRow(
                        
                        column(2,
                               
                               uiOutput('uiMap')
 
                            
                        ),
                        
                        column(10,
                               
                               withLoader(leafletOutput('mapcrimes'),
                                          type="html",
                                          loader="pacman") 
                               
                               )
                        
                        
                    )
                
                
                
                
            ),
            
            tabItem(
                
                tabName = 'about',
                # h1('Aqui va nuestra intro')
                includeHTML('www/html/aboutUs.html')
                
            )
            )
        )
)


