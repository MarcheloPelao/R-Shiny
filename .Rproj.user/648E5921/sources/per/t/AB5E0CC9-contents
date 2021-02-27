shinyServer(function(input, output) {
    
    # DATOS REACTIVOS---
    

    
    TotalCrimes<-reactive({
        casos<-length(df$GRAVITY_OF_CRIME)
        medium<-df%>% dplyr::filter(GRAVITY_OF_CRIME=="MEDIUM") %>% count()
        serious<-df %>% dplyr::filter(GRAVITY_OF_CRIME=="HIGH") %>% count()
        low<-df %>% dplyr::filter(GRAVITY_OF_CRIME=="LOW") %>% count()
        
        list(
            casos = casos,
            medium = medium,
            serious = serious,
            low=low
        )
 
    })
    
    selectores<-reactive({
        
        unique(df$GRAVITY_OF_CRIME)
        
    })
    
    
    selecto2<-reactive(({
        
        unique(df$TIME_FRAME)
    }))
    
    
    datosMap<-reactive({
        
        datamap<-df
        datamap['CANTIDAD']=1
        datamap_High<-datamap %>% dplyr::group_by(LATITUDE,LONGITUDE,PRIMARY.DESCRIPTION,GRAVITY_OF_CRIME) %>% 
            summarise(total_casos=sum(CANTIDAD)) %>% filter(GRAVITY_OF_CRIME=="HIGH"&total_casos>=10)
        datamap_Medium<-datamap %>% dplyr::group_by(LATITUDE,LONGITUDE,PRIMARY.DESCRIPTION,GRAVITY_OF_CRIME) %>% 
            summarise(total_casos=sum(CANTIDAD)) %>% filter(GRAVITY_OF_CRIME=="MEDIUM"&total_casos>=10)
        datamnap_Low<-datamap %>% dplyr::group_by(LATITUDE,LONGITUDE,PRIMARY.DESCRIPTION,GRAVITY_OF_CRIME) %>% 
            summarise(total_casos=sum(CANTIDAD)) %>% filter(GRAVITY_OF_CRIME=="LOW"&total_casos>=10)
        
        list(
            totales=datamap,
            altos=datamap_High,
            medios=datamap_Medium,
            bajos=datamnap_Low
            
        )
        
        
    })
    
    
    data_bar<-reactive({
        
        datos_bar<-df
        datos_bar['CANTIDAD']=1
        datos_bar<-datos_bar%>%
            dplyr::group_by(GRAVITY_OF_CRIME)%>%
            summarise(total_casos=sum(CANTIDAD))
        
        
    })
    
    data_fecha<-reactive({
        
        datos_totales<-df
        datos_totales['CANTIDAD']=1
        datos_totales<-datos_totales%>%
            dplyr::group_by(DATE..OF.OCCURRENCE,GRAVITY_OF_CRIME)%>%
            summarise(total_casos=sum(CANTIDAD))
        
    })

    DataTabla<-reactive(
        
        if (input$horas_dia==input$horas_dia[1] |
            input$horas_dia==input$horas_dia[2] |
            input$horas_dia==input$horas_dia[3] |
            input$horas_dia==input$horas_dia[4] ) {
            df1<- df %>% dplyr::filter(TIME_FRAME==input$horas_dia[1] |
                                           TIME_FRAME==input$horas_dia[2] |
                                           TIME_FRAME==input$horas_dia[3] |
                                           TIME_FRAME==input$horas_dia[4])}
    )
    



    # VALUE BOXES CRIMES ----
    
    output$delitosTotales <- renderValueBox({
        valor <- TotalCrimes()$casos
        valueBox(valor, subtitle = 'Total Crimes', width = 12, color = 'light-blue', icon = icon('battery-full'))
    })
    
    output$delitosGraves <- renderValueBox({
        valor <- TotalCrimes()$serious
        valueBox(valor, subtitle = 'Serious Crimes', width = 12, color = 'red', icon = icon('battery-three-quarters'))
    })
    
    output$delitosMedios <- renderValueBox({
        valor <-TotalCrimes()$medium
        valueBox(valor, subtitle = 'Medium Crimes', width = 12, color = 'yellow', icon = icon('battery-half'))
    })
    
    output$delitosBajos <- renderValueBox({
        valor <- TotalCrimes()$low
        valueBox(valor, subtitle = 'Low Crimes', width = 12, color = 'green', icon = icon('battery-quarter'))
    })
    
    #UI GRAFICOS CRIMES ---
    
    
    output$uiBar <- renderUI({
        
        selector=selectores()
        tagList(
            
            pickerInput('severityBar', label = 'Choose Severity', 
                        choices = c('All',selector),
                        multiple=FALSE)
        )
        
        })
    
    output$uiLine <- renderUI({
        
        fechas<-data_fecha()
        
        selector=selectores()
        tagList(
            pickerInput('severityLine', label ="Choose Severity",  
                        choices = c('All',selector),
                        multiple = FALSE),
            
            dateRangeInput('date',
                           label = 'Dates',
                           start=min(fechas$DATE..OF.OCCURRENCE), 
                           end =max(fechas$DATE..OF.OCCURRENCE)),
            
            actionButton('actualizar', 'Update',
                         style = "bordered",
                         color = "success",
                         icon = icon("calendar-check")
                         )
            
            
        )
        
    })
    

    
    # PLOTS CRIMES ----
    
    output$crimesBar <- renderPlotly({

        dataBar<-data_bar()

        if(input$severityBar=='All'){
 
            grafico=
                ggplot(dataBar, aes(x = GRAVITY_OF_CRIME, weight =total_casos,fill = GRAVITY_OF_CRIME)) + 
                geom_bar()+
                labs(title=paste0("Chicago Gravity of Crime ",input$severityBar),
                     x="Gravity", y = "Count",color='Gravity')+
                guides(fill=FALSE)+
                theme_classic()
            
            ggplotly(grafico)
            
        }
        
        else{
 
            dataBar2<-data_bar()%>%dplyr::filter(GRAVITY_OF_CRIME==input$severityBar)
            colors <- c("HIGH" = "red", "MEDIUM" = "green", "LOW" = "blue")
            grafico=
                ggplot(dataBar2, aes(x = GRAVITY_OF_CRIME, weight =total_casos,fill =GRAVITY_OF_CRIME )) + 
                geom_bar()+
                labs(title=paste0("Chicago Gravity of Crime ",input$severityBar),
                     x="Gravity", y = "Count",color='Gravity')+
                scale_fill_manual(values = colors)+
                guides(fill=FALSE)+
                theme_classic()+
                theme(legend.position="none")
            
            
            ggplotly(grafico)
            
        }

            
        
    })
    
    
    output$crimesLine <- renderPlotly({
        
        input$actualizar

        data<-data_fecha()
        

        if (input$severityLine=='All'){
            
            
            isolate(data<-data %>%dplyr::filter(DATE..OF.OCCURRENCE>=as.Date(input$date[1])&
                                                    DATE..OF.OCCURRENCE<=as.Date(input$date[2])))

            grafico=
            
            ggplot(data) + 
                geom_line(aes(x =DATE..OF.OCCURRENCE , y = total_casos,color =GRAVITY_OF_CRIME))+
                labs(title=paste0("Chicago Gravity of Crime ",input$severityLine),
                     x="Gravity", y = "Count",color = "Gravity")+
                guides(fill=FALSE)+
                theme_classic()
            
            ggplotly(grafico)
            
        }
    
        else{
            
            isolate(data2<-data %>%dplyr::filter(GRAVITY_OF_CRIME==input$severityLine) %>% 
                        filter(DATE..OF.OCCURRENCE>=as.Date(input$date[1])&DATE..OF.OCCURRENCE<=as.Date(input$date[2])))
            
            colors <- c("HIGH" = "red", "MEDIUM" = "green", "LOW" = "blue")

            grafico=
            
                ggplot(data2) + 
                geom_line(aes(x =DATE..OF.OCCURRENCE , y = total_casos, color = GRAVITY_OF_CRIME))+
                labs(title=paste0("Chicago Gravity of Crime ",input$severityLine),
                     x="Gravity", y = "Count",color = "Gravity")+
                scale_color_manual(values = colors)+
                theme_classic()+
                theme(legend.position="none")
            
            ggplotly(grafico)

        }
    })

    
    # MAP CRIMES ----
    
    output$uiMap <- renderUI({
    
        helpText("This map show the gravity of crimes by location and color above 10 crimes commited")
        

        
    })
    
    
    output$mapcrimes <- renderLeaflet({
        
        df_map<-datosMap()
        
        leaflet() %>% 
            addProviderTiles("Stamen.TonerHybrid", group="Map View 1") %>% 
            addTiles(options = providerTileOptions(noWrap = TRUE), group="Map View 2") %>% 
            addCircleMarkers(data=df_map$altos, lng=~LONGITUDE , lat=~LATITUDE, radius=8 ,
                             color="red",  fillColor="red", stroke = TRUE, fillOpacity = 0.2, group="HIGH",
                             label=~as.character(total_casos),
                             popup=~as.character(total_casos)) %>%
            addCircleMarkers(data=df_map$medios, lng=~LONGITUDE , lat=~LATITUDE, radius=8 ,
                             color="orange",  fillColor="orange", stroke = TRUE, fillOpacity = 0.2, group="MEDIUM",
                             label=~as.character(total_casos),
                             popup=~as.character(total_casos)) %>%
            addCircleMarkers(data=df_map$bajos, lng=~LONGITUDE , lat=~LATITUDE, radius=8 ,
                             color="green",  fillColor="green", stroke = TRUE, fillOpacity = 0.2, group="LOW",
                             label=~as.character(total_casos),
                             popup=~as.character(total_casos)) %>% 
            addLayersControl(overlayGroups = c("HIGH","MEDIUM","LOW") , baseGroups = c("Map View 1","Map View 2"),
                             options = layersControlOptions(collapsed = FALSE))
        
        
    })
    

    # VALUE BOXES CRIMES ----
    
    output$Totalmap <- renderValueBox({
        valor <- TotalCrimes()$casos
        valueBox(valor, subtitle = 'Total Crimes', width = 12, color = 'light-blue', icon = icon('battery-full'))
    })
    
    output$highmap <- renderValueBox({
        valor <- TotalCrimes()$serious
        valueBox(valor, subtitle = 'Serious Crimes', width = 12, color = 'red', icon = icon('battery-three-quarters'))
    })
    
    output$mediummap <- renderValueBox({
        valor <-TotalCrimes()$medium
        valueBox(valor, subtitle = 'Medium Crimes', width = 12, color = 'yellow', icon = icon('battery-half'))
    })
    
    output$lowmap <- renderValueBox({
        valor <- TotalCrimes()$low
        valueBox(valor, subtitle = 'Low Crimes', width = 12, color = 'green', icon = icon('battery-quarter'))
    })
    

    
    # TABLE CRIMES ----    
    
    
    output$horas_dia<-renderUI({
        seleccion<-selecto2()
        
        awesomeCheckboxGroup("horas_dia", label = h3("Filter Select"), 
                           choices = list("Morning" = unique(df$TIME_FRAME)[1], 
                                          "Dawn" = unique(df$TIME_FRAME)[2],
                                          "Afternoon" = unique(df$TIME_FRAME)[3],
                                          "Evening" = unique(df$TIME_FRAME)[4]),
                           selected = seleccion,
                           status = "danger"
        )
        
        
        
    })
    

    output$hist_hours <- renderPlotly({

        dataHist<-DataTabla()
        hist1<- ggplot(dataHist) + geom_histogram(aes(x=DATE..OF.OCCURRENCE, fill=TIME_FRAME),
                                                 color='White',show.legend = FALSE,
                                                 position="identity", alpha=0.3, bins=30)+xlab('Date') +
            ylab('Frecuency')+facet_grid(TIME_FRAME ~ .)+
            theme_classic() +
            theme(legend.position='none')
        ggplotly(hist1)
    })

    
    output$TablaCrimen <- DT::renderDT({ DataTabla() })
    

    
    
    
    
    

    

})
