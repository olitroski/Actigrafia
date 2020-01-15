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
        if (is.list(input$BrowsePath) == FALSE & length(awdfolder) == 0) {
            cat(awdfolder)
        } else if (is.list(input$BrowsePath) == FALSE & length(awdfolder) == 1){
            cat(awdfolder)
        } else if (is.list(input$BrowsePath) == TRUE){
            cat(as.character(parseDirPath(volumes, input$BrowsePath)))
            awdfolder <<- as.character(parseDirPath(volumes, input$BrowsePath))
        }
    })
    
    # Resuelto lo anterior ya podemos poner el working directory
    setwd(awdfolder)

    # | -- Botón de guardar directorio ----------------------------------------
    observeEvent(input$saveDir, {
        currdir <- getwd()
        setwd(mainfolder)
        writeLines(as.character(parseDirPath(volumes, input$BrowsePath)), "workdir.lab")        
        setwd(currdir)
    })

    
    # | -- Table de los archivos ----------------------------------------------
    output$dfdir <- renderTable({
        df <- load.awdfolder(awdfolder)
        df
    })
    
    
    # | -- radioButton para filtrar -------------------------------------------
    
    
    
    # | -- Table de los recuentos ---------------------------------------------
    # Esta es solo de prueba para que se vea <<tableDir>>
    output$tableDir <- renderTable({
        data.frame(Status = c("No procesado", "En edición", "Terminado", "Con error"),
                        N = c(21,342,54,1))
        
    })
    
    
    
    
    # Panel 2 - EDICION DE ARCHIVOS -------------------------------------------
    
    
    
}














