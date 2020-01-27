# Cargar un sujeto valido
awdfolder <- "D:/OneDrive/INTA/Actigrafia/testfolder/test_kansas"
setwd(awdfolder)
archivos <- dir()
archivos <- archivos[grep(".[Aa][Ww][Dd]$", archivos)]
awdfile <- archivos[1]
awdfile <- str_replace(awdfile, ".AWD", "")

# Cargar data para un grafico
gdata <- check.acvfilter(awdfile)
gdata <- gdata$semiper
gdata <- gdata$per01


# ---- Define UI for app --------------------------------------------
ui <- fluidPage(
    # Titulo
    titlePanel("Slider range test with plot"),

    # En una row
    fluidRow(
        column(12, align = "center",
        # # El plot
        # plotOutput("periodPlot", height = 200, width = "90%"),
        # 
        # # El Slider
        # sliderInput("sliderEdicion", label = NA,
        #            min = 0, max = 100, value = c(0,100), width = "90%")
        
        # El grafico primero
        plotOutput("periodPlot", height = 200, width = "90%"),
        
        # Input: Slider for the number of bins
        uiOutput("sliderEdicion")
        )
    ),
    
    fluidRow(
        column(12, 
            # Botón de reset
            actionButton("resetBtn", "Resetear"),
            verbatimTextOutput("test")
        )
    )
)

# ---- Define server logic ------------------------------------------
server <- function(input, output, session){
    # Un botón de reset update el eslaider
    observeEvent(input$resetBtn, {
        # El reset debiera calzar con el del grafico
        xscale <- seq(as.numeric(set$ininoc)/3600, length.out = 25)
        updateSliderInput(session, "rangoX", value = c(min(xscale), max(xscale)))
    })

    # Render ui del slider porque necesito que el min y max no sea fijo
    output$sliderEdicion <- renderUI({
        xscale <- seq(as.numeric(set$ininoc)/3600, length.out = 25)
        minui <- min(xscale)
        maxui <- max(xscale)

        sliderInput("rangoX", label = NA,
                    min = minui, max = maxui, value = c(minui, maxui),
                    width = "95%")
    })

    # El mono
    output$distPlot <- renderPlot({
        xscale <- seq(as.numeric(set$ininoc)/3600, length.out = 25)
        xscale <- c(min(xscale), max(xscale))
        
        if (sum(xscale == input$rangoX) == 2){
            lim.x <- NULL
        } else {
            lim.x <- input$rangoX
        }
        

        create.plotSimple(gdata)
    })
    
    output$test <- renderPrint({
        xscale <- seq(as.numeric(set$ininoc)/3600, length.out = 25)
        xscale <- c(min(xscale), max(xscale))
        
        if (sum(xscale == input$rangoX) == 2){
            lim.x <- NULL
        } else {
            lim.x <- input$rangoX
        }
        
        paste(lim.x)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)



# create.plotSimple(gdata, limites = c(32,40))
create.plotSimple(gdata)












