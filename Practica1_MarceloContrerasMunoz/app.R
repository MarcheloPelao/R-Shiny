
library(shiny)
library(ggplot2)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Practica_1: Marcelo Contreras"),

    # Sidebar with a slider input for number of bins

    sidebarLayout(
        sidebarPanel(
            
            
            helpText("Pulsa para usar el dataset faithful.Por defecto se usara MPG"),
            
            checkboxInput('faithful', label = 'Usar Faithful', value = FALSE),
            
            helpText("La opcion variable solo se usara cuando Estemos usando el dataset MPG"),


            selectInput('variablempg', 
                        label = 'Variable de MPG', 
                        choices = list('displ', 'cyl','cty','hwy')),
            
            helpText("Puedes escoger el numero de bins a tu gusto"),
            
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            h4('Histograma'),
            plotlyOutput('distPlot'),
           # plotOutput("distPlot"),
           textOutput('miOutput')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    observe({
        print(input$variable)
    })
    

    output$distPlot <- renderPlotly({

        plot = ggplot(mpg) + 
            geom_histogram(aes_string(input$variablempg),bins=input$bins,color='white',fill='blue')
        
        if(input$faithful == TRUE){
            plot = ggplot(faithful) + geom_histogram(aes(waiting), bins =input$bins,color='white',fill='darkgreen')
        }
        
        ggplotly(plot)
        
    })
    
    output$miOutput <- renderText({
        
        t=''
        
        if(input$faithful == TRUE){
            t = paste('Estamos usando el dataset Faithful con la variable Waiting')
        }
        
        if(input$faithful == FALSE){
            t = paste('Estamos usando el dataset MPG, representando la variable', input$variablempg)
        }
        
        
        t
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
