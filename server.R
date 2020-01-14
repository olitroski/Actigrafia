server <- function(input, output){
    # #### El lado derecho de las settings primero ################################ #    
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
    
    output$wd <- renderText(getwd())


    # #### Elementos tab Archivos ################################################# #
    # ---- Seleccionar directorio ------------------------------------------------- #
    # Capturar y mostrar el directorio  
    volumes <- c("Usuario" = fs::path_home(), getVolumes()())
    shinyDirChoose(input, "awdfolder", roots = volumes, restrictions = system.file(package = "base"))
    
    # Texto del buscar folder
    output$ledir <- renderPrint({
        if (is.integer(input$awdfolder) & length(savedir) == 0) {
            cat("Directorio de archivos AWD")
        } else if (is.integer(input$awdfolder) & length(savedir) == 1){
            cat(savedir)
        } else {
            cat(as.character(parseDirPath(volumes, input$awdfolder)))
        }
    })
    

    # Botón de guardar directorio
    observeEvent(input$saveDir, {
        currdir <- getwd()
        setwd(mainfolder)
        writeLines(as.character(parseDirPath(volumes, input$awdfolder)), "workdir.lab")        
        setwd(currdir)
    })

    
    # Mostrar los archivos
    output$dfdir <- renderTable({
        if (is.integer(input$awdfolder) & length(savedir) == 0) {
            cat("")
        } else if (is.integer(input$awdfolder) & length(savedir) == 1){
            setwd(savedir)
            
            # Un data frame en 2 lineas
            df <- dir()
            cutval <- floor(length(df)/2)
            df1 <- df[1:cutval]
            df2 <- df[(cutval+1):length(df)]
            
            df <- data.frame(Archivos = df1, Archivos = df2, stringsAsFactors = FALSE)
            df
        } else {
            # Al wd
            setwd(parseDirPath(volumes, input$awdfolder))
            # Un data frame en 2 lineas
            df <- dir()
            cutval <- floor(length(df)/2)
            df1 <- df[1:cutval]
            df2 <- df[(cutval+1):length(df)]
            
            df <- data.frame(Archivos = df1, Archivos = df2, stringsAsFactors = FALSE)
            df
        }    
        
        
    })
    
}














