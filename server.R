server <- function(input, output, session){
    # Panel - VISUALIZAR ARCHIVOS -----------------  -------------------------
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
    my_files <- reactivePoll(100, session, checkFunc = new.files, valueFunc = get.files)
    
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



    # Panel - ACTOGRAMA --------------------------------  ---------------------
    
    # | -- renderUI selectInput de sujetos ------------------------------------
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
            cat("Sujeto sin procesar, se debe hacer el análisis preliminar, seleccionar 'Analizar' en 'Acciones'")
        # Si tiene más de 1 archivo
        } else if (nfiles > 1){
            cat("Sujeto en edición, puede finalizarlo o seguir editando...")
        } else {
            cat("Error, Error en la base de datos... :)")
        }
    })
    
    # | Boton Acciones a tomar ------------------------------------------------------
    # Crear un reactive cada vez que se apriete el boton
    actSelection <- eventReactive(input$accion_button, {
        
        # | ---- Accion 1: Analizar -------------------------------------------
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
                    semiper <- create.semiper(acv)
                    filter.stats <- create.firstfilter(sujeto, semiper)
                    acv.edit <- create.acvedit(sujeto, acv, filter.stats)
                    
                    # Volver a la pestaña de archivos
                    updateNavbarPage(session, inputId = "TablasApp", selected = "Archivos")
                    
                    FALSE
                }
            }
            
        # | ---- Accion 2: Cargar actograma -----------------------------------
        } else if (input$accion_choice == "Actograma"){
            # Si aprieta y no se ha seleccionado un sujeto
            if (awdfile() == "Debe ingresar un sujeto"){
                showNotification("Error, no hay sujeto")
                FALSE

            # Si hay sujeto proseguir
            } else {
                subj.status <- filter(subjectDF(), Sujeto == awdfile()) %>% select(Status)
                subj.status <- subj.status[1,1]
                
                # Hay sujeto y con status Editar guarda el awdfile para graficar.
                if (subj.status == "En edicion"){
                    showNotification("Creando actograma")
                    # Devuelve el nombre del sujeto, asi si cambia en el selectInput 
                    # no se hace el actograma para otro sujeto.
                    awdfile()
                } else {
                    showNotification("El sujeto no ha sido analizado")
                    FALSE
                }
            }
         
        # | ---- Accion 3: Editar -----------------------------------------------
        } else if (input$accion_choice == "Editar"){
            # Si aprieta y no se ha seleccionado un sujeto
            if (awdfile() == "Debe ingresar un sujeto"){
                showNotification("Error, no hay sujeto")
                FALSE            
            
            # Hay sujeto... se sigue
            } else {
                subj.status <- filter(subjectDF(), Sujeto == awdfile()) %>% select(Status)
                subj.status <- subj.status[1,1]
                
                # Hay sujeto, pero con status Sin editar
                if (subj.status == "No procesado"){
                    showNotification("Error, Sujeto no tiene análisis inicial")
                    FALSE
                    
                # Hay sujeto pero está terminado
                } else if (subj.status == "Terminado"){
                    showNotification("Error, Sujeto terminado")
                    FALSE
                
                 # Ahora si procesar el sujeto 
                } else if (subj.status == "En edicion"){
                    showNotification("Procesando...")
                    # La idea es que nos mande para la pestaña siguiente.
                    updateNavbarPage(session, inputId = "TablasApp", selected = "Edición")
                    # Dejo el awdfile() para que se haga actograma y se pueda consultar
                    awdfile()
                }
            }

        # Algo extraño pasó
        } else {
            stop("Error en la selección de la acción")
        }
    })

    
    # | Actograma -------------------------------------------------------------
    # La lógica depende del botón de accion a tomar, cuando se aprieta, el output
    # es: FALSE o el awdfile(), se cargan los datos y se pasan al actograma con el
    # actSelection() al igualarse a awdfile() 

    # El ui render, el height se setea grande para que no de error de margins
    output$actoUI <- renderUI({
        # Darle al toque un tamaño
        if (actSelection() != awdfile()){
            plotOutput("actograma", width = "100%", height = 1800)
        
        # Cambiarlo si es que hay datos cargados
        } else if (actSelection() == awdfile()){
            h <- length(acveditRDS()[["semiper"]]) * 120
            plotOutput("actograma", width = "100%", height = h)
            
        # algo raro paso
        } else {
            stop("Algo paso con el render ui del plot")
        }
    })
    
    # El actograma dependiente del botón y el awdfile
    output$actograma <- renderPlot({
        validate(
            need(acveditRDS(), "Esperando datos!")
        )
        
        if (actSelection() == awdfile()){
            # Si no hay semiper pone algo igual
            if (length(acveditRDS()[["semiper"]]) == 0){
                plot(0,type='n',axes=FALSE,ann=FALSE)
            } else if (length(acveditRDS()[["semiper"]]) > 0){
                create.actogram(acveditRDS()[["semiper"]])
            } else {
                stop("Algo pasó con el grafico")
            }
        }
    })

    
    # | -- POLL: Filtro RDS ---------------------------------------------------
    # Funcion leer el file mtime 
    filter.check <- function(){
        fichero <- str_c(awdfile(), ".edit.RDS")
        if (file.exists(fichero)){
            info <- base::file.info(fichero)
            info <- info$mtime
            return(info)
        } else {
            return(1)
        }
    }
    
    # Si cambió usar esta funcion para un get
    filter.get <- function(){
        fichero <- str_c(awdfile(), ".edit.RDS")
        if (file.exists(fichero)){
            return(readRDS(fichero))
        } else {
            return(1)
        }
    }
    
    # La magia del check
    filterRDS <- reactivePoll(200, session, checkFunc = filter.check, valueFunc = filter.get)
    
    
    # | -- POLL: AcvEdit RDS --------------------------------------------------
    # Función leer el mtime del acvedit
    acvedit.check <- function(){
        fichero <- str_c(awdfile(), "_acv.edit.RDS")
        if (file.exists(fichero)){
            info <- base::file.info(fichero)
            info <- info$mtime
            return(info)
        } else {
            return(1)
        }
    }
    
    # Si cambia cargar el acv.edit
    acvedit.get <- function(){
        validate(need(awdfile(), "Esperando awdfile!"))
        fichero <- str_c(awdfile(), "_acv.edit.RDS")
        if (file.exists(fichero)){
            return(check.acvfilter(awdfile()))
        } else {
            return(1)
        }
    }
    
    # El poll
    acveditRDS <- reactivePoll(200, session, checkFunc = acvedit.check, valueFunc = acvedit.get)
    
    
    
    
    # Panel - EDICION ---------------------------------------- ----------------
    # Le test de este panel
    output$testE <- renderPrint({
    })

    # | -- Sujeto en edición --------------------------------------------------
    # Dijimos que cada vez se carga el awdfile, asi que primero checar si quedó 
    # seleccionado
    output$SubjEdicion <- renderPrint({
        cat(awdfile())
    })
    
    # | -- Seleccionar el período a editar ------------------------------------
    # Los periodos no cambian, asi que se cargan una vez y alimentan al botón 
    # para seleccionar la data adecuada.
    output$perSelection <- renderUI({
        
        # Si aun no se selecciona nada da error, asi que a probar
        if (length(acveditRDS()) == 1){
            periodos <- NULL
        } else {
            periodos <- acveditRDS()$timelist
            periodos <- paste(periodos$period, "-", periodos$tlist)
        }
        
        selectInput(inputId = "perChoose", label = NULL, 
                    choices = periodos, width = "100%")
    })

    # | -- El mono ;) -----------------------------------------------------------
    output$periodPlot <- renderPlot({
        # Espera el choose
        validate(
            need(input$perChoose, "Esperando input!")
        )
        # gdata <- acveditRDS()$semiper
        # periodo <- str_sub(input$perChoose, 1, 5)
        # gdata <- gdata[[periodo]]
        # create.plotSimple(gdata, limites = input$rangoX, lw = input$ldNum)
        create.plotSimple(gdata = acveditRDS()$semiper[[str_sub(input$perChoose, 1, 5)]],
                          limites = input$rangoX, lw = input$ldNum)
    })
    
    
    # | -- Slider del gráfico -------------------------------------------------
    # Reset Range btn
    observeEvent(input$resetBtn, {
        # El reset debiera calzar con el del grafico
        xscale <- seq(as.numeric(set$ininoc)/3600, length.out = 25)
        updateSliderInput(session, "rangoX", value = c(min(xscale), max(xscale)))
        updateNumericInput(session, "ldNum", value = 1)
    })

    # Render ui del slider
    output$sliderEdicion <- renderUI({
        xscale <- seq(as.numeric(set$ininoc)/3600, length.out = 25)
        minui <- min(xscale)
        maxui <- max(xscale)

        sliderInput("rangoX", label = NA,
                    min = minui, max = maxui, value = c(minui, maxui),
                    width = "95%", step = 1)
    })

    # | -- Mostrar el filtro --------------------------------------------------
    output$testF <- renderPrint({
        input$ok
    })
    
    # Esto es reactivo dependiendo de cambios en el filtro asi que no cambia
    # | Mostrar el header 
    output$filtroH <- renderPrint({
        # Si el filtroFinal no tiene length=2 es que no existe y carga el inicial
        if (length(filterRDS()) != 2){
            cat("No se ha seleccionado un sujeto")
        } else {
            cat(paste(filterRDS()$header[1:3], collapse = "\n"))
        }
    })
    
    
    # | Mostrar el filtro 
    output$filtroDF <- renderTable({
        # Si el filtroFinal no tiene length=2 es que no existe y carga el inicial
        if (length(filterRDS()) == 2){
            df <- filterRDS()$filter
            df$ini <- format(df$ini, format = "%d/%m/%Y %H:%M:%S")
            df$fin <- format(df$fin, format = "%d/%m/%Y %H:%M:%S")
            df
        }
    }, digits = 0, align = "c")
    
    
    # | -- Pestañas de ediciones ----------------------------------------------
    # Reactive del perChoose en formato POSIXct, para usar en las ediciones
    selectedPer <- reactive({
        validate(need(input$perChoose, "Esperando input!"))
        
        fec <- str_sub(input$perChoose, 9, str_length(input$perChoose))
        fec <- str_sub(fec, str_locate(fec, " ")[1]+1, str_length(fec))
        fec <- dmy(fec)
        fec <- format(fec, "%A %d/%m/%Y")
        fec
    })
    
    # | ---- 1. Editar periodo ------------------------------------------------
    # Fecha en curso
    output$selectedPer1 <- renderPrint({
        cat(selectedPer())
    })
    
    # | ------ Crear un reactive filterPeriod() -------------------------------
    filterPeriod <- reactive({
        # Esperar esto y ver si el periodo tiene dia, noche o ambos
        validate(need(input$perChoose, "Esperando input!"))
        
        gdata <- acveditRDS()$semiper[[str_sub(input$perChoose, 1, 5)]]
        
        time <- gdata$xscale
        corte <- 24 + as.numeric(set$inidia)/3600
        nnoc <- sum(time < corte)
        ndia <- sum(time >= corte)
        
        if (nnoc == 0 & ndia > 0){
            dn <-"d"
        } else if (nnoc > 0 & ndia == 0){
            dn <-"n"
        } else {
            dn <- "a"
        }

        # Checar que existan periodos a editar
        if (class(input$perChoose) == "NULL"){
            list(msg = "Debe existir un sujeto en edición", action = 0)
        
        # Hay para editar
        } else {
        
            # Si aprieta y no hay selección
            if (class(input$dianoc) == "NULL"){
                list(msg = "Debe seleccionar día, noche o ambos", action = 0)
    
            # Si selecciona Ambos
            } else if (sum(input$dianoc == c("Dia", "Noche")) == 2){
                
                # Si solo tiene 1 avisar y action 0
                if (dn != "a"){
                    list(msg = "El periodo solo tiene dia o noche", action = 0)
                    
                # Tiene ambos días tons action 2 (para el n del vector luego)
                } else {
                    noc1 <- min(gdata$time)
                    noc2 <- gdata$time[which(gdata$xscale == corte)-1]
                    nocF <- data.frame(id = NA, ini = noc1, fin = noc2, tipo = 2)
                    
                    dia1 <- gdata$time[which(gdata$xscale == corte)]
                    dia2 <- max(gdata$time)
                    diaF <- data.frame(id = NA, ini = dia1, fin = dia2, tipo = 2)
                    
                    msg <- c(paste0("Noc: ", noc1, " a ", noc2), 
                             paste0("Dia: ", dia1, " a ", dia2))
                    list(msg = msg, action = 1, filtro = bind_rows(nocF, diaF))
                }
            
            # Si selecciona Noche
            } else if (input$dianoc == "Noche"){
                if (dn == "d"){
                    list(msg = "El periodo solo tiene día", action = 0)
                } else {
                    noc1 <- min(gdata$time)
                    noc2 <- gdata$time[which(gdata$xscale == corte)-1]
                    nocF <- data.frame(id = NA, ini = noc1, fin = noc2, tipo = 2)
                    
                    msg <- paste0("Noc: ", noc1, " a ", noc2)
                    list(msg = msg, action = 1, filtro = nocF)
                }

            # Si selecciona Dia
            } else if (input$dianoc == "Dia"){
                if (dn == "n"){
                    list(msg = "El periodo solo tiene dia", action = 0)
                } else {
                    # Si es el primer dia puede que no comience en el corte
                    if (length(gdata$time[which(gdata$xscale == corte)]) == 0){
                        dia1 <- min(gdata$time)
                    } else {
                        dia1 <- gdata$time[which(gdata$xscale == corte)]
                    }
                    
                    dia2 <- max(gdata$time)
                    diaF <- data.frame(id = NA, ini = dia1, fin = dia2, tipo = 2)

                    msg <- paste0("Dia:   ", dia1, " a ", dia2)
                    list(msg = msg, action = 1, filtro = diaF)
                }
    
            # Si algo pasa rarisimo
            } else {
                stop("Full de error")
            }
            
        }
    })

    # | ------ Mostrar el filtro a aplicar -----------------------------------
    output$toFilter1 <- renderPrint({
        validate(need(input$perChoose, "Esperando input!"))
        cat(paste(filterPeriod()$msg, collapse = "\n"))
    })
    
    
    # | ------ Modal de confirmación ------------------------------------------
    warnModal <- function(){
        # Configurar el mensaje
        if (length(filterPeriod()$msg) == 2){
            show <- filterPeriod()$msg
        } else {
            show <- c(filterPeriod()$msg, " ")
        }
        
        modalDialog(
            title = "Confirmar filtros",
            size = "m",
            easyClose = TRUE,
            
            div(span(code(show[1])), br(), span(code(show[2]))),

            footer = tagList(
                modalButton("Cancelar"),
                actionButton("ok", "Confirmar")
            )
        )
    }
    
    # Mostrar el modal cuando se aprieta el modificiar filtro
    observeEvent(input$cambia_periodo,{
        if (filterPeriod()$action == 1){
            showModal(warnModal())
        }
    })

    # | ------ Decidir que hacer cuando es ok el modal ------
    observeEvent(input$ok,{
        showNotification("Procesando...")
        # update a filtro y acvfilter
        # update.filterPeriod()
        
        
        # Primero checar que tenga action = 1
        if (filterPeriod()$action == 1){
            # Hacer update al filtro
            filt <- bind_rows(filterRDS()$filter, filterPeriod()$filtro)
            filt <- mutate(filt, id = 1:nrow(filt))
            newfiltro <- list(header = filterRDS()$header, filter = filt)
            
            # Hacer el update del acvedit (codigo prestado de create.acvedit)
            acvedit <- readRDS(paste0(awdfile(), "_acv.edit.RDS"))
            acvedit$filter <-NA
            for (f in 1:nrow(filt)){
                ini <- filt$ini[f]
                fin <- filt$fin[f]
                
                range <- which(acvedit$time >= ini & acvedit$time <= fin)
                acvedit$filter[range] <- filt$tipo[f]
            }
            
            # Guarda ahora los dos
            saveRDS(newfiltro, paste0(awdfile(), ".edit.RDS"))
            saveRDS(acvedit,  paste0(awdfile(), "_acv.edit.RDS"))
        }
        
        
        removeModal()
    })
    
    
    # | ---- 2. En editar actividad -------------------------------------------
    output$selectedPer2 <- renderPrint({
        selectedPer()
    })
    
    # | ---- 3. En mover noche ------------------------------------------------
    # output$selectedPer2 <- renderPrint({
    #     cat(selectedPer())
    # })
    
}

