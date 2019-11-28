library(shiny)

# User interface
ui <- fluidPage(
    # El input
    selectInput("dataset", label = "Dataset", choices = ls("package:datasets")),
    
    # El output
    verbatimTextOutput("summary"),
    tableOutput("table")
)

# Server 
server <- function(input, output, session) {
    # El output para verbatimTextOutput("summary")
    output$summary <- renderPrint({
        dataset <- get(input$dataset, "package:datasets")
        summary(dataset)
    })
    
    # Para el <tableOutput("table")>
    output$table <- renderTable({
    dataset <- get(input$dataset, "package:datasets")
    dataset
  })
}

# Hacer funcionar
shinyApp(ui, server)





