server <- function(input, output){
    # Panel 1 - ARCHIVOS ------------------------------------------------------
    # | -- Parametros detección -----------------------------------------------
    output$showSet1 <- renderUI({
        HTML(
            paste("<strong>Hora inicio noche:</strong><br>", set$ininoc, "<br>",
                  "<strong>Duración primer sueño:</strong><br>", set$dursleep, " mins<br><br>",
                  "<strong>Hora inicio día:</strong><br>", set$inidia, "<br>",
                  "<strong>Duración primera vigilia:</strong><br>", set$durawake, " mins<br><br>",
                  "<strong>Sensibilidad deteción</strong><br>", set$sensivar, "<br><br>",
                  "Para cambiar la configuración se debe modificar el archivo <strong>settings.lab</strong> ubicado en el directorio del programa.",
                  "<br><br>", sep = "")
        )
    })


    # | -- Botón Seleccionar directorio ---------------------------------------
    # Capturar y mostrar el directorio  
    volumes <- c("Usuario" = fs::path_home(), getVolumes()())
    shinyDirChoose(input, "BrowsePath", roots = volumes, restrictions = system.file(package = "base"))
    
    # Texto del buscar folder y wd si hubiera
    output$wdFolderTxt <- renderPrint({
        
        # Con datos el input es una lista y en awdfolder siempre habrá un valor aunque esté vacio el file
        if (is.list(input$BrowsePath) == FALSE) {
            cat(awdfolder)
            setwd(awdfolder)
        } else if (is.list(input$BrowsePath) == TRUE){
            cat(as.character(parseDirPath(volumes, input$BrowsePath)))
            awdfolder <<- as.character(parseDirPath(volumes, input$BrowsePath))
            setwd(awdfolder)
        }
    })
    
    # Resuelto lo anterior ya podemos poner el working directory --- de nuevo por siaca
    setwd(awdfolder)

    # | -- Botón de guardar directorio ----------------------------------------
    observeEvent(input$saveDir, {
        setwd(mainfolder)
        writeLines(awdfolder, "savedir.lab")        
        setwd(awdfolder)
    })

    
    # | -- Table de los archivos ----------------------------------------------
    # Depende de los radioButtons para elegir que mostrar
    # choices = c("No procesado", "En edicion", "Terminado", "Con error", "Todos")
    subjectDF <- load.awdfolder(awdfolder) # <<<<<<<<<<< data.frame sujetos >>>>>>>>>>>>>>
    output$dfdir <- renderTable({
        if (input$filterDir != "Todos"){
            filter(subjectDF, Status == input$filterDir)            
        } else {
            subjectDF
        }
    })
    

    # | -- Tabla de los recuentos ---------------------------------------------
    output$tableDir <- renderTable({
        summDF <- otable(rvar = "Status", data = subjectDF)
        summDF <- summDF %>% mutate(pct = pct*100, pct = paste(pct, "%", sep="")) %>%
            rename(N = freq, Porcentaje = pct)
        levels(summDF$Status)[length(levels(summDF$Status))] <- "Total"
        summDF
    })
    
    
    # Panel 2 - EDICION DE ARCHIVOS -------------------------------------------
    
    # | -- UI selectInput de sujetos ------------------------------------------
    output$subjInput <- renderUI({
        lechoices <- c("Seleccionar", as.character(subjectDF$Sujeto))
        selectInput(inputId="awdfile2", label=NULL, choices=lechoices)
    })
    
    # | -- Render la selección del sujeto -------------------------------------
    output$selectedSubj <- renderPrint({
        # Tomar los valores si es que hay -- Bien verboso para que sea facil de leer
        if (input$awdfile1 %in% subjectDF$Sujeto){
            awd1 <- input$awdfile1
        } else {
            awd1 <- NA
        }
        
        if (length(input$awdfile2)==0){
            awd2 <- NA
        } else if (input$awdfile2 %in% subjectDF$Sujeto){
            awd2 <- input$awdfile2
        } else {
            awd2 <- NA
        }
        
        # ----- Decidir el output a mostrar ----- #
        # No hay nada
        if (is.na(awd1)==TRUE & is.na(awd2)==TRUE){
            cat("Debe ingresar un sujeto")
            awdfile <<- NULL
        # Solo hay el pegado
        } else if (is.na(awd1)==FALSE & is.na(awd2)==TRUE){
            cat(input$awdfile1)
            awdfile <<- paste(input$awdfile1, ".AWD", sep ="")
        # Solo hay el seleccionado
        } else if (is.na(awd1)==TRUE & is.na(awd2)==FALSE){
            cat(input$awdfile2)
            awdfile <<- paste(input$awdfile2, ".AWD", sep ="")
        # Hay los 2 pero diferentes
        } else if ((is.na(awd1)==FALSE & is.na(awd2)==FALSE) & (awd1 != awd2)){
            cat("Los sujetos son difentes, borre uno")
            awdfile <<- NULL
        # Hay los 2 y son iguales
        } else if ((is.na(awd1)==FALSE & is.na(awd2)==FALSE) & (awd1 == awd2)){
            cat(input$awdfile2)
            awdfile <<- paste(input$awdfile2, ".AWD", sep ="")
        } else {
            print(paste("Algo pasó con la selección", awd1, awd2, 
                        input$awdfile1 %in% subjectDF$Sujeto, 
                        input$awdfile2 %in% subjectDF$Sujeto,
                        sep = "  -  "))
        }
    })
    
    # | -- Status del sujeto --------------------------------------------------
    output$statsSubj <- renderPrint({
        cat(getwd(), sep = "     ")
    })
    
    
    # | -- Actograma ----------------------------------------------------------
    output$actograma <- renderPlot({

        if (class(awdfile) == "NULL"){
            # "2058-001-368 JRG Baseline.AWD"
            hist(subjectDF$N.Files)
        } else {
            create.actogram(awdfile)
            
        }
    })
    
    
    
}












