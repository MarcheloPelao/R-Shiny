# tips: las fechas son input dinamicos y las tengo que colocar con el render UI, recupero la data con un reactive
# hago el render ui con esa data del reactive, elijo la fecha segun su minimo y su maximo
# el filtro que se usa en edad y altura es el >=
# la UI son dos paneles condicionales con 2 UI outputs, la Ui se contruye en el server
# para leer los data set se usa la función reactive, será la que lea los data set
# antes de enviar la practica hay que borar el  environment y correr la app y funciona se envia
# si se corre la practica y aparecen warning no enviar.


library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Practica2: Marcelo Contreras"),

    # Sidebar with a slider input for number of bins 

    sidebarLayout(
        
        sidebarPanel(
            
            conditionalPanel("input.tabs =='Grafica'",
                             uiOutput('myUI')),

            
            conditionalPanel("input.tabs =='Tabla'",
                             uiOutput('myUI2'))

            
        ),
        
        # main panel
        mainPanel(
            tabsetPanel(id = 'tabs',
                        tabPanel("Grafica", plotlyOutput("plot")), 
                        tabPanel("Tabla", DT::dataTableOutput("table"))),
     
            
            
        )
    )

)
        

# Define server logic required to draw a histogram
server <- function(input, output,session) {
    
    getData<-reactive({
        data<-read.csv("sunspots.csv",sep=",")
        data[,2:3]
        data$Month<-as.Date(data$Month,format = "%Y-%m-%d")
        data
        
    })
    


    output$myUI<-renderUI({
        
        fechas<-getData()
        fecha_min=min(fechas$Month)
        fecha_max=max(fechas$Month)
        
        tagList(
            
            dateRangeInput('date',
                           label = 'Fechas',
                           start=fecha_min, 
                           end =fecha_max),
            
            actionButton('actualizar', 'Actualizar')
            
        )
        
        })
    

    
    output$plot <- renderPlotly({

        dfplot<-getData()

        input$actualizar
        
        isolate(dfplot<-dfplot %>%dplyr::filter(Month>=as.Date(input$date[1])&
                                                     Month<=as.Date(input$date[2])))
        grafico=
            ggplot(dfplot,aes(x=as.Date(Month),y=Sunspots))+
            geom_line(color='blue')+
            xlab("Mes")
        
        ggplotly(grafico)

        })


 
    getData2<-reactive({
        data2<-read.csv("biostats.csv",sep=",")
        names(data2) = c("Nombre", "Sexo", "Edad", "Altura", "Peso")
        data2
    })
    
    output$myUI2<-renderUI({

        miDataDos<-getData2()
        miDataDos$Sexo<-as.factor(miDataDos$Sexo)
        
        tagList(
            
            selectInput(inputId = 'sexo',
                        label = 'Sexo',
                        choices = levels(miDataDos$Sexo)),
            
            numericInput(inputId = 'edad', 
                         label = 'Edad',
                         min = min(miDataDos$Edad), max =max(miDataDos$Edad), value =round(median(miDataDos$Edad),0), step = 1),
            
            numericInput(inputId = 'altura', 
                         label = 'Altura',
                         min = min(miDataDos$Altura), max =max(miDataDos$Altura), value =round(median(miDataDos$Altura),0), step = 1)
            
            
        )

        
    })
    
    

    
    output$table<-DT::renderDT({

        data_tabla<-getData2()
        data_tabla
        
        data_tabla<- data_tabla %>%
            dplyr::filter(Sexo == input$sexo) %>% 
            dplyr::filter(Edad >= input$edad)%>% 
            dplyr::filter(Altura>= input$altura)


        # if(input$sexo %in% data_tabla$Sexo){
        #     data_tabla<-data_tabla %>%
        #         dplyr::filter(Sexo==input$sexo)
        # }
        # if(input$edad %in% data_tabla$Edad){
        #     data_tabla<-data_tabla %>%
        #         dplyr::filter(Edad>=input$edad)
        # }
        # 
        # if(input$altura %in% data_tabla$Altura){
        #     data_tabla<-data_tabla %>%
        #         dplyr::filter(Altura>=input$altura)
        # }

        DT::datatable(data_tabla)
    })

 
}


# Run the application 
shinyApp(ui = ui, server = server)
