
library(shiny)
library(quantmod)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Practica Stock"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            # sliderInput("bins",
            #             "Number of bins:",
            #             min = 1,
            #             max = 50,
            #             value = 30)
            
            helpText('Selecciona un stock que analizar'),
            
            textInput('symb',label='selecciona el simbolo',value='SPY'),
            
            dateRangeInput('dates',label = 'Fechas',start='2013-01-01', end = Sys.Date()),# vector de longitud 2
            
            checkboxInput('adjust',label = 'Ajustar precios por inflacion',value = FALSE),
            
            checkboxInput('log',label = 'Usar escala logaritmica',value = FALSE)
            
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           
           plotOutput('plot')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # la funcion reactive es la manera de llamar a una función en shiny, hace exactamente lo mismo que una función
    # en R solo que una funcion de R no se puede usar en shiny, se tiene que usar reactive
    
    #vamoas a usar dos sfunciones de quantmod, la primera getSymbols que traerá un df con los valores que 
    #use al tiempo que se le pase en el panel de input
    #y la funcion chartSeries que representa la data que se llama con getSymbols
    #dato: date en un vector de longitud 2 por lo tanto [1] devuelve la fecha from y [2] devuelve la fecha to
    # con la funcion reactive tb podriamos contruir un read_csv y traer los datos, la funcon reactive se encarga de
    #recibir los datos, manipularlos en casa de ser necesario y luego ser llamada para optener los datos
   
    source('helpers/ajuste_inflacion.R') 
    
    getData<-reactive({
        
        simbolo=input$symb
        
        data<-getSymbols(simbolo,
                         from=input$dates[1],
                         to=input$dates[2],auto.assign = F)
 
        
    })
    
    getajuste<-reactive({
        
        if (input$adjust==TRUE){
            
            adjust(getData())
        }
        else{
            
            getData()
        }
    })

    output$plot <- renderPlot({
        
        misDatos<-getajuste()

        chartSeries(misDatos,type='line',log.scale=input$log,TA=NULL)


        
    })
    
    observe({
        print(input$adjust)
        
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
