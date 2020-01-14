library(shiny)
library(openxlsx)

# okeep("acv")
# head(acv)
# data <- create.gdata(acv)


data <- create.gdata(acv)
data1 <- data[[1]]
data2 <- data[[2]]

# Define UI for application that draws a histogram
ui <- navbarPage(
    "acto individual",
    
    tabPanel("acto",
        # Selecciona un archvio y lo carga
        selectInput("fileList", "Lista de archivos",
                    choices = archivos),
        
        textOutput("lefile"),
            
        # Al cargar lee lo que tenga el sujeto
        plotOutput("plot1")
        
        # plotOutput("plot2"),
        # plotOutput("plot3")
        # 
        # Ideas
        # A) Cuando selecciona un file se tienen que hacer varias cosas
        #     1. Buscar con un grep ese file en el folder de trabajo
        #     2. Si solo tiene el awd indicar que no tiene nada y se debe procesar
        #     3. Si tiene edit file, cargarlo a un objeto aunque este en blanco
        #     4. Printar el objeto para mirarlo
            
        
    )


)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$lefile <- renderText({
        input$fileList
    })
    
    output$plot1 <- renderPlot({
        plot.period(data1)
    })
    
    fname <- reactive({
        asdf <- input$file
        asdf <- str_replace(asdf, ".AWD", "")
    })
    
    
    # grep()
    
    
    
    # output$plot2 <- renderPlot({
    #     plot.period(data2)
    # })
    # 
    # output$plot3 <- renderPlot({
    #     plot.period(data3)
    # })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
