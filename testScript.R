# ---- Define UI for app --------------------------------------------
ui <- fluidPage(
    # Titulo
    titlePanel("Slider range test with plot"),

    # En una row
    fluidRow(
        column(12, align = "center",
            # El grafico primero
            plotOutput("periodPlot", height = 200, width = "90%"),
            
            # Input: Slider for the number of bins
            uiOutput("sliderEdicion")
        )
    ),

    fluidRow(
        # Selector de lw
        column(2,
            h4("Tamaño de linea"),
            numericInput("ldNum", value = 1, min = 1, max = 10, step = 1, label = NULL)
        ),

        # Botón de reset
        column(10, 
            h4("Resetear el rango del gráfico"),
            actionButton("resetBtn", label = "Reset"),
        )
    )
)

# ---- Define server logic ------------------------------------------
server <- function(input, output, session){
    # | Reset Range btn -----------------------------------------------------
    observeEvent(input$resetBtn, {
        # El reset debiera calzar con el del grafico
        xscale <- seq(as.numeric(set$ininoc)/3600, length.out = 25)
        updateSliderInput(session, "rangoX", value = c(min(xscale), max(xscale)))
    })

    # | Render ui del slider -----------------------------------------------
    output$sliderEdicion <- renderUI({
        xscale <- seq(as.numeric(set$ininoc)/3600, length.out = 25)
        minui <- min(xscale)
        maxui <- max(xscale)

        sliderInput("rangoX", label = NA,
                    min = minui, max = maxui, value = c(minui, maxui),
                    width = "95%", step = 1)
    })

    # | El mono -----------------------------------------------------------
    output$periodPlot <- renderPlot({
        create.plotSimple(gdata, limites = input$rangoX, lw = input$ldNum)
    })
    
    output$test <- renderPrint({
        cat("")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)










