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
    # Tiene un reactivePoll para ver si cambian los archivos del folder. Solo en nombres OJO.
    new.files <- function() {dir()}
    get.files <- function() {dir()}
    my_files <- reactivePoll(100, session, checkFunc = new.files,valueFunc = get.files)
    
    # Cargar el directorio de acuerdo a lo que diga el reactivePoll
    subjectDF <- reactive({
        fn.setwd()
        load.awdfolder(filelist = my_files())
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


    # ####################################################################################### #
    # Panel - ACTOGRAMA -----------------------------------------------------------------------
    # ####################################################################################### #
    # |------------- UN TEST -------------------| ------
    output$test <- renderPrint({
        actSelection()
    })
    
    
    
    
    # | -- UI selectInput de sujetos ------------------------------------------  renderUI
    output$subjInput <- renderUI({
        lechoices <- c("Seleccionar", as.character(subjectDF()$Sujeto))
        selectInput(inputId="awd_select", label=NULL, choices=lechoices)
    })
    
        
    # | Determina el sujeto a trabajar ----------------------------------------
    # | ---- Reactive awdfile() -----------------------------------------------
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
    
    
    # | ---- Output Sujeto ----------------------------------------------------
    output$selectedSubj <- renderPrint({
        if (length(awdfile()) == 0){
            " "
        } else {
            cat(awdfile())
        }
    })
    

    # | ---- Status Sujeto ----------------------------------------------------
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
    
    
    
    
    # | Acciones a tomar ------------------------------------------------------
    # Crear un reactive cada vez que se apriete el boton
    actSelection <- eventReactive(input$accion_button, {
        
        # | ---- Analisis inicial -------------------------------------------------
        if (input$accion_choice == "Analizar"){
            # Si aprieta y no se ha seleccionado un sujeto
            if (awdfile() == "Debe ingresar un sujeto"){
                showNotification("Error, no hay sujeto")
                FALSE
                                
            # Si hay sujeto proseguir
            } else {
                subj.status <- filter(subjectDF(), Sujeto == awdfile()) %>% select(Status)
                subj.status <- subj.status[1,1]
                
                # Hay sujeto, pero con status Editar
                if (subj.status == "En edicion"){
                    showNotification("Error, Sujeto en edición")
                    FALSE
                
                # Hay sujeto pero está terminado
                } else if (subj.status == "Terminado"){
                    showNotification("Error, Sujeto terminado")
                    FALSE
                    
                # Ahora si procesar el sujeto desde cero
                } else if (subj.status == "No procesado"){
                    showNotification("Procesando...")
                    
                    # La vuelta completa está en el powerpoint
                    sujeto <- paste(awdfile(), ".AWD", sep = "")
                    acv <- create.acv(sujeto, set$sensivar)
                    semiper <- create.semiper(sujeto, acv)
                    filter.stats <- create.firstfilter(sujeto, semiper)
                    acv.edit <- create.acvedit(sujeto, acv, filter.stats)
                    FALSE
                }
            }
            
        # Si es que selecciona el histograma
        } else if (input$accion_choice == "Cargar Actograma"){
            # Si aprieta y no se ha seleccionado un sujeto
            if (awdfile() == "Debe ingresar un sujeto"){
                showNotification("Error, no hay sujeto")
                FALSE
                
            # Si hay sujeto proseguir
            } else {
                subj.status <- filter(subjectDF(), Sujeto == awdfile()) %>% select(Status)
                subj.status <- subj.status[1,1]
                
                # Hay sujeto, pero con status Editar
                if (subj.status == "En edicion"){
                    showNotification("Error, Sujeto en edición")
                    # Devuelve el nombre del sujeto, 
                    # asi si cambia no se hace el actograma
                    awdfile()
                } else {
                    FALSE
                }
            
            }
            
        } else {
            FALSE
        }
        
        
        
    
    })
    

    
    # | -- Actograma ----------------------------------------------------------
    # La lógica depende del botón, cuando se aprieta, el output es o FALSE o el 
    # awdfile() y como es eventReactive no lo cambia de valor al cambiar el sujeto
    output$actograma <- renderPlot({
        if (actSelection() == awdfile()){
            # Lee el archivo primero
            sujeto <- paste(awdfile(), ".AWD", sep = "")
            sujeto <- str_replace(sujeto, ".[Aa][Ww][Dd]$", "_acv.edit.RDS")
            acv.edit <- readRDS(sujeto)
            
            # Y hace el actograma
            semiper <- create.semiper(awdfile(), acv.edit)
            create.actogram(semiper)
        }
    })


    
}

