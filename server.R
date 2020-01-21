server <- function(input, output, session){
    # Panel - VISUALIZAR ARCHIVOS ---------------------------------------------
    # | -- Seleccionar directorio ---------------------------------------------
    # Cargar el valor del save dir
    saved.folder <- reactive({
        load.savedir(mainfolder)
    })
    
    # Boton para <input$BrowsePath>
    volumes <- getVolumes()
    shinyDirChoose(input, "BrowsePath", roots = volumes, session = session)
    
    # El reactive que determina el path (saved.folder o input$BrowsePath)
    awdfolder <- reactive({
        if (length(input$BrowsePath) != 1){
            parseDirPath(volumes, input$BrowsePath)
        } else if (length(saved.folder()) == 1){
            saved.folder()
        } else {
            " "
        }
    })
    
    # Y la funcion para setear el wd (ya que tenemos el folder definitivo)
    fn.setwd <- function(){
        if (length(input$dir) != 1 | (length(saved.folder()) == 1)){
            setwd(awdfolder())
        }
    }
    
    # | -- Output del folder -------------------------------------------------
    output$wdFolderTxt <- renderPrint({
        cat(awdfolder())
    })

    # | -- Botón de guardar directorio ----------------------------------------
    observeEvent(input$saveDir, {
        writeLines(awdfolder(), file.path(mainfolder, "savedir.lab"))        
        
        # Verificar que se guardó
        if (awdfolder() == load.savedir(mainfolder)){
            showNotification("Se guardó correctamente el directorio")
        } else {
            showNotification("Apretar de nuevo para que se guarde")
            
        }
    })

    #| -- Cargar directorio en df ---------------------------------------------
    # Cargar el directorio en un data.frame
    subjectDF <- reactive({
        fn.setwd()
        load.awdfolder() 
    })

        # | -- -- Tabla directorio ----------------------------------
        output$dfdir <- renderTable({
            # Los radio buttons para el filtraje
            if (input$filterDir != "Todos"){
                filter(subjectDF(), Status == input$filterDir)            
            } else {
                subjectDF()
            }
        })

        # | -- -- Tabla de los recuentos ----------------------------
        output$tableDir <- renderTable({
            if (subjectDF()[1,1] != "Directorio sin archivos AWD"){
                summDF <- otable(rvar = "Status", data = subjectDF())
                summDF <- summDF %>% mutate(pct = pct*100, pct = paste(pct, "%", sep="")) %>%
                    rename(N = freq, Porcentaje = pct)
                levels(summDF$Status)[length(levels(summDF$Status))] <- "Total"
                summDF
            } else {
                data.frame(Status = "Directorio sin archivos AWD")
            }
        })
    
    # | -- Boton para re-cargar directorio ---------------------------------------
    observeEvent(input$btn_cargar, {
        subjectDF <- reactive({
            fn.setwd()
            load.awdfolder() 
        })
    })

    
    # Este boton debe re iniciar el proceso de cargar el directorio, no lo hace por algún motivo
    # claramente es porque no tiene nada reactivo dentro, la funcion tiene reactivo, pero no la funcion
    # de leer entonces.
    
    
    
    
    
    
    
    
    # | -- Parametros detección -----------------------------------------------
    # Los parametros se leen de la lista de settings.
    output$showSet <- renderUI({
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



    # Panel - ACTOGRAMA -------------------------------------------
    # |------------- UN TEST -------------------| ------
    output$test <- renderPrint({
        awdfile()=="Debe ingresar un sujeto"
    })

    # | -- UI selectInput de sujetos ------------------------------------------  renderUI
    output$subjInput <- renderUI({
        lechoices <- c("Seleccionar", as.character(subjectDF()$Sujeto))
        selectInput(inputId="awd_select", label=NULL, choices=lechoices)
    })
    
        
    # | -- Determina el awdfile ----------------------------------------------
    # | -- -- reactive -------------------------------------------------------
    awdfile <-  reactive({
        # Tomar los valores si es que hay -- Bien verboso para que sea facil de analizar
        
        # En <pegar> checa si está en el vector de suejetos
        if (input$awd_paste %in% subjectDF()$Sujeto){
            apaste <- input$awd_paste
        } else {
            apaste <- NA
        }
        
        # En <seleccionar> checka si está en el vector de sujetos
        if (length(input$awd_select)==0){
            aselect <- NA
        } else if (input$awd_select %in% subjectDF()$Sujeto){
            aselect <- input$awd_select
        } else {
            aselect <- NA
        }
        
        # ----- Decidir el output a mostrar ----- #
        # No hay nada
        if (is.na(apaste)==TRUE & is.na(aselect)==TRUE){
            "Debe ingresar un sujeto"
        # Solo hay el pegado
        } else if (is.na(apaste)==FALSE & is.na(aselect)==TRUE){
            input$awd_paste
        # Solo hay el seleccionado
        } else if (is.na(apaste)==TRUE & is.na(aselect)==FALSE){
            input$awd_select
        # Hay los 2 pero diferentes
        } else if ((is.na(apaste)==FALSE & is.na(aselect)==FALSE) & (apaste != aselect)){
            "Los sujetos son difentes, borre uno"
        # Hay los 2 y son iguales
        } else if ((is.na(apaste)==FALSE & is.na(aselect)==FALSE) & (apaste == aselect)){
            input$awd_select
        # Alguna combinacion rara
        } else {
            paste("Algo pasó con la selección", apaste, aselect, 
                        input$awd_paste %in% subjectDF$Sujeto, 
                        input$awd_select %in% subjectDF$Sujeto,
                        sep = "  -  ")
        }
    })
    
    
    # | -- -- Output awdfile (el sujeto en realidad) --------------------------
    output$selectedSubj <- renderPrint({
        if (length(awdfile()) == 0){
            " "
        } else {
            cat(awdfile())
        }
    })
    

    # | -- Status del sujeto ------------------------------------------------------
    output$statsSubj <- renderPrint({
        nfiles <- filter(subjectDF(), Sujeto == awdfile()) %>% select(N.Files)
        
        # Si no hay nada seleccionado
        if (awdfile()=="Debe ingresar un sujeto"){
            cat(" ")
        # Si el sujeto seleccionado no tiene archivos
        } else if (nrow(nfiles) == 0){
            cat("---ERROR--- El sujeto no tiene archivos...")
        # Si tiene solo 1 archivo
        } else if (nfiles == 1){
            cat("Sujeto sin procesar, se debe hacer el análisis inicial...")
        # Si tiene más de 1 archivo
        } else if (nfiles > 1){
            cat("Sujeto en edición, puede finalizarlo o seguir editando...")
        } else {
            cat("Error, Error en la base de datos... :)")
        }
    })
    
    
    # Cuando aprieto "Proceder" para tomar una accion tiene que suceder esta secuencia.
    # 1. Analizar
    # 2. Ejecuta sobre el awdfile todo el pool de funciones iniciales
    # 3. Se tiene que volver a leer el subjectDF()
    # 4. Por lo tanto recargar la pestaña archivos
    # 5. Modificar el resultado de la seleccion 
    
    
     
    # | -- Actograma ----------------------------------------------------------
    output$actograma <- renderPlot({
        fn.setwd()
        sujeto <- paste(awdfile, ".AWD", sep = "")
        

        
        awdfile <- "2058-001-368 JRG Baseline.AWD"
        newname <- str_replace(string=awdfile,
                               pattern=".[Aa][Ww][Dd]$",
                               replacement = "_acv.edit.RDS")
        acv.edit <- readRDS(file = newname)
        semiper <- create.semiper(awdfile, acv.edit)
        
        # lefiles <- dir()
        # acv.edit <- grep(sub(".[Aa][Ww][Dd]$", "_acv.edit.RDS", awdfile), lefiles)
        # acv.edit <- lefiles[acv.edit]
        # acv.edit <- readRDS(acv.edit)
        # semiper <- create.semiper(awdfile, acv.edit)
        # windows()
        create.actogram(semiper)


    })


    
}

