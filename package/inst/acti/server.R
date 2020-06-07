# OlitoSleep App de sueño más App del mercado ---------------------------------
server <- function(input, output, session){    
    
    # _________________________________________ -------------------------------
    # ARCHIVOS ----
    # | -- Seleccionar directorio ---------------------------------------------    
    path <- reactiveValues()
    
    # ---- Checar exista savedir ---- #
    savedir_path <- file.path(Sys.getenv("USERPROFILE"), "savedir")
    
    # Si no existe lo crea
    if (file.exists(savedir_path) == FALSE){
        writeLines(Sys.getenv("USERPROFILE"), savedir_path)
        path$ruta <- Sys.getenv("USERPROFILE")
        setwd(Sys.getenv("USERPROFILE"))
    # Si existe lo carga
    } else {
        path$ruta <- readLines(savedir_path)
        setwd(readLines(savedir_path))
    }
    
    # ---- Boton choose dir ---- #     <<<< path$ruta >>>>
    observeEvent(input$pathBoton, {
        # Elije
        ruta <- choose.dir()
        
        # Por si cancela
        if (is.na(ruta)){
            path$ruta <- readLines(savedir_path)
            setwd(readLines(savedir_path))
        # Si sigue
        } else {
            writeLines(ruta, savedir_path)
            path$ruta <- readLines(savedir_path)
            setwd(readLines(savedir_path))
        }
    })
    
    # Mostrar Path
    output$pathText <- renderPrint({
        setwd(path$ruta)
        cat(getwd())
    })
    
    # Refrito con el awdfolder que es el que controla.
    awdfolder <- reactive({path$ruta})
    
    
    #| -- Cargar subjectDF() --------------------------------------------------
    # reactivePoll para ver si cambian los archivos del folder. Solo en dir().
    new.files <- function() {dir(path$ruta)}
    get.files <- function() {dir(path$ruta)}
    my_files <- reactivePoll(100, session, checkFunc = new.files, valueFunc = get.files)
    
    # Cargar el directorio de acuerdo a lo que diga el reactivePoll
    subjectDF <- reactive({
        setwd(path$ruta)
        load.awdfolder(filelist = my_files())
    })

    # | -- -- renderTable directorio ------------------------------------------------
    output$dfdir <- renderTable({
        # Los radio buttons para el filtraje
        if (input$filterDir != "Todos"){
            filter(subjectDF(), Status == input$filterDir)            
        } else {
            subjectDF()
        }
    })

    # | -- -- renderTable recuentos ------------------------------------------
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
    
    # | -- -- Botón procesar en masa "massProc" -------------------------------
    # Modal de confirmación 
    warnModal.mass <- function(){
        modalDialog(
            title = "Confirmar procesado en masa",
            size = "m",
            easyClose = TRUE,
            
            div(span(h4("Está a punto de procesar todos los ficheros en status"), 
                     h4(strong("No procesado")),
                     p("¿Desea continuar?"))),
            
            footer = tagList(
                modalButton("Cancelar"),
                actionButton("mass", "Confirmar")
            )
        )
    }

    # Mostrar modal al apretar boton y error si no hay pa procesar
    observeEvent(input$massProc,{
        nopro <- filter(subjectDF(), Status == "No procesado") %>% select(Sujeto)
        
        if (nrow(nopro) == 0){
            showNotification("No existen sujetos para procesar", type = "error")
        } else {
            showModal(warnModal.mass())
        }
    })    

    # Si se acepta el mass proc, proceder
    observeEvent(input$mass, {
        # Listado de sujetos
        procAwd <- filter(subjectDF(), Status == "No procesado") %>% select(Sujeto)
        procAwd <- paste0(procAwd$Sujeto, ".AWD")
        # Procesar
        for (sujeto in procAwd){
            showNotification(paste("Cargando", sujeto))
            acv <- create.acv(sujeto, set$sensivar)
            semiper <- create.semiper(acv)
            filter.stats <- create.firstfilter(sujeto, semiper)
            acv.edit <- create.acvedit(sujeto, acv, filter.stats)
            showNotification("Sujeto procesado", type = "message")
        }
        removeModal()
    })
    
    
    
    
    
    # ___________________________________________ -----------------------------
    # | ACTOGRAMA -------------------------------------------------------------
    # | ---- Determina awdfile() ----------------------------------------------
    awdfile <-  reactive({
        input$awd_select
    })
    
    # | renderUI selección de sujetos -----------------------------------------
    output$subjInput <- renderUI({
        lechoices <- filter(subjectDF(), Status == "En edicion")
        lechoices <- as.character(lechoices$Sujeto)
        # Por si queda en cero
        if (length(lechoices) == 0){
            radioButtons(inputId = "awd_select", label = NULL, choices = c("No hay sujetos"))
        } else {
            radioButtons(inputId = "awd_select", label = NULL, choices = lechoices)
        }
    })

    # Valor reactivo para que cambie dentro de los botones
    # showActogram <- reactiveValues()
    # showActogram$val <- "sin seleccion"

    # | Boton Editar ----------------------------------------------------------
    # Apretar y nos vamos a pestaña siguiente
    observeEvent(input$edEdit.btn, {
        # Debiera hacer el actograma e irse a la siguiente pestaña
        showNotification("Procesando...", closeButton = FALSE, type = "message")
        # showActogram$val <- awdfile()
        updateNavbarPage(session, inputId = "TablasApp", selected = "Edición")
    })
    
    # | Boton Actograma -------------------------------------------------------
    # Solo muestra el acto
    # observeEvent(input$edActo.btn, {
    #     showNotification("Graficando...", closeButton = FALSE, type = "message", duration = 2)
    #     showActogram$val <- awdfile()
    # })
    
    # | Boton Finalizar -------------------------------------------------------
    # Modal
    edFin.modal <- function(){
        modalDialog(
            title = "Finalizar edición",
            size = "m",
            easyClose = TRUE,
            
            div(h4("Terminar la edición del sujeto"),
                p(awdfile()),
                p(strong("Con esta acción se finaliza la edición del sujeto."))),
            
            footer = tagList(
                modalButton("Cancelar", icon = icon("window-close")),
                actionButton("finalOK", "Finalizar", icon = icon("save"))  # El valor resultante
            )
        )
    }
    
    # Modal de no rango
    edFin.modalNO <- function(){
        modalDialog(
            title = "Falta acción",
            size = "m",
            easyClose = TRUE,
            div(h4("No se ha determinado el inicio y final del registro")),
            footer = tagList(
                modalButton("Cancelar"),
                # actionButton("finalOK", "Finalizar")  # El valor resultante
            )
        )
    }
    
    # Botón a mostrar
    observeEvent(input$edFin.btn, {
        # Asegurar que esté el inicio y fin
        rango <- filterRDS()$header
        
        if (rango[4] == "Inicia:  -No determinado- " | rango[5] == "Termina: -No determinado- "){
            showModal(edFin.modalNO())
        } else {
            showModal(edFin.modal())
        }
    })

    # Mostrar el modal y ejecutar acciones
    observeEvent(input$finalOK,{
        filename <- paste0(awdfile(), ".finish.RDS")
        txt <- c("Sujeto terminado", awdfile(), "Fecha", now())
        saveRDS(txt, file = file.path(awdfolder(), filename))
        showNotification(paste("Terminando", awdfile()), closeButton = FALSE, type = "message")
        removeModal()
    })
    

    # | Actograma -------------------------------------------------------------
    # La lógica depende del botón de accion, cuando se aprieta, el output es awdfile()
    # eso activa el proceso.

    # Dibuja el actograma a partir del awdfile() y el acvditRDS() y lo pasa al renderUI de abajo
    output$actograma <- renderPlot({
        validate(need(acveditRDS(), "Esperando datos!"))
        # validate(need(showActogram$val, "Esperando datos!"))
        
        # if (showActogram$val == awdfile()){
            
        if (awdfile() != "No hay sujetos"){
            
            # Si no hay semiper pone algo igual
            if (length(acveditRDS()[["semiper"]]) == 0){
                plot(0, type='n', axes=FALSE, ann=FALSE)
                
            } else if (length(acveditRDS()[["semiper"]]) > 0){
                create.actogram(acveditRDS()[["semiper"]], set = set)
                
            } else {
                stop("Algo pasó con el grafico")
            }
            
        }
        
        # }
    })

    # El ui render, el height se setea grande para que no de error de margins
    output$actoUI <- renderUI({
        # validate(need(showActogram$val, "Esperando datos!"))
        validate(need(awdfile(), "Esperando datos!"))
        validate(need(acveditRDS(), "Esperado datos!"))
        
        # # Si sujeto no es correcto dibuja en blanco
        # if (showActogram$val != awdfile()){
        #     plotOutput("actograma", width = "100%", height = 1800)
        #     
        # # Cambiarlo si es que hay datos cargados
        # # } else if (actSelection() == awdfile()){
        # } else if (showActogram$val == awdfile()){
        #     h <- length(acveditRDS()[["semiper"]]) * 120
        #     plotOutput("actograma", width = "100%", height = h)
        #     
        #     # algo raro paso
        # } else {
        #     stop("Algo paso con el render ui del plot")
        # }
        
        
        # Carga al tiro
        if (awdfile() != "No hay sujetos"){
            h <- length(acveditRDS()[["semiper"]]) * 120
            plotOutput("actograma", width = "100%", height = h)
        }
        
        
        
    })
    
    
    
    # _________________________________________ -------------------------------
    # REACTIVOS: filterRDS & acveditRDS ---------------------------------------
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
    
    # La magia del reactivePoll
    filterRDS <- reactivePoll(250, session, checkFunc = filter.check, valueFunc = filter.get)
    
    
    # | -- POLL: AcvEdit RDS --------------------------------------------------
    # Función leer el mtime del acvedit
    acvedit.check <- function(){
        fichero <- str_c(awdfile(), ".acv.edit.RDS")
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
        fichero <- str_c(awdfile(), ".acv.edit.RDS")
        if (file.exists(fichero)){
            return(check.acvfilter(awdfile(), set))
        } else {
            return(1)
        }
    }
    
    # El poll
    acveditRDS <- reactivePoll(250, session, checkFunc = acvedit.check, valueFunc = acvedit.get)
    
    
    
    # _________________________________________ -------------------------------
    # EDICION -----------------------------------------------------------------

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
        
        # Si no hay sujetos
        if (awdfile() != "No hay sujetos"){
            radioButtons("perChoose", label = NULL, choices = periodos)
        } 
        
    })
    
    # | -- Tabla de estados  --------------------------------------------------
    tablaEstados <- reactive({
        stagesTable(acveditRDS(), input$perChoose)
    })
    

    # | -- Gráfico ------------------------------------------------------------
    output$periodPlot <- renderPlot({
        # Espera el choose
        validate(need(input$perChoose, "Esperando input!"))
        
        # Periodos combinado con la selección
        create.plotSimple(gdata = acveditRDS()$semiper[[str_sub(input$perChoose, 1, 5)]],
                          limites = input$rangoX, lw = input$ldNum, set = set)
    })

    # | -- Slider y control del gráfico ----
    # Reset Range btn
    observeEvent(input$resetBtn, {
        # El reset debiera calzar con el del grafico
        xscale <- seq(as.numeric(set$ininoc)/3600, length.out = 25)
        updateSliderInput(session, "rangoX", value = c(min(xscale), max(xscale)))
        updateNumericInput(session, "ldNum", value = 1)
    })
    
    # Boton volver a actograma
    observeEvent(input$volverActo, {
        updateNavbarPage(session, inputId = "TablasApp", selected = "Actograma")
    })

    # Render ui del slider
    output$sliderEdicion <- renderUI({
        xscale <- seq(as.numeric(set$ininoc)/3600, length.out = 25)
        minui <- min(xscale)
        maxui <- max(xscale)
        
        sliderInput("rangoX", label = NULL,
                    min = minui, max = maxui, value = c(minui, maxui),
                    width = "95%", step = 1)
    })
    
    

    # | -- Header Filtro ----
    output$filtroH <- renderPrint({
        # Si el filtroFinal no tiene length=2 es que no existe y carga el inicial
        if (length(filterRDS()) != 2){
            cat("No se ha seleccionado un sujeto")
        } else {
            cat(paste(filterRDS()$header[2:3], collapse = "\n"))
        }
    })
    
    
    # | -- Inicio Fin filtro ----
    output$filtroIniFin <- renderPrint({
        if (length(filterRDS()) != 2){
            cat("No se ha seleccionado un sujeto")
        } else {
            cat(paste(filterRDS()$header[4:5], collapse = "\n"))
        }
    })
    
    
    # | -- Tabla Filtro  ----
    output$filtroDF <- renderTable({
        # Si el filtroFinal no tiene length=2 es que no existe y carga el inicial
        if (length(filterRDS()) == 2){
            df <- filterRDS()$filter
            df$ini <- format(df$ini, format = "%d-%m-%Y  %H:%M")
            df$fin <- format(df$fin, format = "%d-%m-%Y  %H:%M")
            df
        }
    }, digits = 0, align = "c")

    
    
    # | Pestañas de ediciones -------------------------------------------------
    # Reactive del perChoose en formato POSIXct, para usar en las ediciones
    selectedPer <- reactive({
        validate(need(input$perChoose, "Esperando input!"))
        fec <- str_sub(input$perChoose, 9, str_length(input$perChoose))
        fec <- str_sub(fec, str_locate(fec, " ")[1] + 1, str_length(fec))
        fec <- dmy(fec)
        fec <- format(fec, "%A %d-%m-%Y")
        fec
    })
    
    # | -- 1. Inicio y termino registro ---------------------------------------
    # | ------ Inicio del registro ----
    output$inifin.iniUI <- renderUI({
        validate(need(input$perChoose, "Esperando input!"))
        
        # Debe ser el inicio de una vigilia
        valor <- filter(tablaEstados(), estado == "W")
        valor <- valor$inicio
        
        selectInput("inifin.ini", label = NULL, choices = valor)
    })

    # Funcion modal
    warnModal.ini <- function(){
        modalDialog(
            title = "Confirmar Inicio Registro",
            size = "m",
            easyClose = TRUE,
            
            # El mensaje
            div(span(p("Fecha y hora: ", code(input$inifin.ini)))),
            
            footer = tagList(
                modalButton("Cancelar"),
                actionButton("inifin.iniOK", "Confirmar")
            )
        )
    }
    
    # Mostrar modal ini
    observeEvent(input$inifin.iniset, {
        showModal(warnModal.ini())
    })
    
    # Acciones a tomar ini
    observeEvent(input$inifin.iniOK, {
        # "Inicia:  "
        newhead <- filterRDS()$header
        newhead[4] <- ""
        newhead[4] <- paste0("Inicia:  ", input$inifin.ini)
        # Guardar
        newfiltro <- list(header = newhead, filter = filterRDS()$filter)
        saveRDS(newfiltro, paste0(awdfile(), ".edit.RDS"))
        removeModal()
    })
    
    
    
    # | ------ Fin del registro ------   
    output$inifin.finUI <- renderUI({
        validate(need(input$perChoose, "Esperando input!"))
        
        # Debe ser un final de vigilia
        valor <- filter(tablaEstados(), estado == "W")
        valor <- valor$termino
        
        selectInput("inifin.fin", label = NULL, choices = valor)
    })

    # Funcion modal fin
    warnModal.fin <- function(){
        modalDialog(
            title = "Confirmar Término del Registro",
            size = "m",
            easyClose = TRUE,
            
            # El mensaje
            div(span(p("Fecha y hora: ", code(input$inifin.fin)))),
            
            footer = tagList(
                modalButton("Cancelar"),
                actionButton("inifin.finOK", "Confirmar")
            )
        )
    }
    
    # Mostrar modal fin
    observeEvent(input$inifin.finset, {
        showModal(warnModal.fin())
    })
    
    # Acciones a tomar
    observeEvent(input$inifin.finOK, {
        # "Termina: "
        newhead <- filterRDS()$header
        newhead[5] <- ""
        newhead[5] <- paste0("Termina: ", input$inifin.fin)
        # Guardar
        newfiltro <- list(header = newhead, filter = filterRDS()$filter)
        saveRDS(newfiltro, paste0(awdfile(), ".edit.RDS"))
        removeModal()
    })
    
    
    
    # | -- 2. Editar periodo --------------------------------------------------
    # Fecha en curso
    output$selectedPer1 <- renderPrint({
        validate(need(input$perChoose, "Esperando input!"))
        # Procesar las fechas
        periodo <- str_split(input$perChoose, " - ", simplify = TRUE)[1]
        minfec <- min(acveditRDS()[["semiper"]][[periodo]][, "time"])
        maxfec <- max(acveditRDS()[["semiper"]][[periodo]][, "time"])
        fechas <- c(format(as_date(minfec), format = "%A %d-%m-%Y"),
                    format(as_date(maxfec), format = "%A %d-%m-%Y"))
        cat(fechas[1], "\n", fechas[2], sep = "")
    })
    
    # | ------ reactive filterPeriod() -----
    # Esto tiene la info del filtro a aplicar, es una lista con el id, filtro y tipo
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
            } else if (length(input$dianoc) == 2 & sum(input$dianoc == c("Noche", "Dia")) == 2){
                
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
            } else if (length(input$dianoc) == 1 & input$dianoc == "Noche"){
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
            } else if (length(input$dianoc) == 1 & input$dianoc == "Dia"){
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

    # Mostrar el filtro a aplicar 
    output$toFilter1 <- renderPrint({
        validate(need(input$perChoose, "Esperando input!"))
        cat(paste(filterPeriod()$msg, collapse = "\n"))
    })

    # Modal de confirmación 
    warnModal <- function(){
        # Configurar el mensaje (1 o 2 periodos)
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
        # Si se va a superponer un filtro 2 en un 1
        test <- filterPeriod()$filtro
        test <- test$ini
        
        filtro <- filterRDS()$filter
        filtro <- filtro$ini
        
        temp <- test %in%filtro
        
        if (sum(temp) == 1 | sum(temp) == 2){
            showNotification("No se pueden superponer filtros", closeButton = FALSE, type = "error")
        } else if (filterPeriod()$action == 1){
            showModal(warnModal())
        }
    })
    
    # Decidir que hacer cuando el modal input$ok
    observeEvent(input$ok,{
        showNotification("Procesando...")
        # Primero checar que tenga action = 1
        if (filterPeriod()$action == 1){
            # Hacer update al filtro
            filt <- bind_rows(filterRDS()$filter, filterPeriod()$filtro)
            filt <- arrange(filt, ini)
            filt <- mutate(filt, id = NA)
            filt <- distinct(filt, id, ini, fin, tipo)
            filt$id <- 1:nrow(filt)
            newfiltro <- list(header = filterRDS()$header, filter = filt)
            
            # Hacer el update del acvedit (codigo prestado de create.acvedit)
            acvedit <- readRDS(paste0(awdfile(), ".acv.edit.RDS"))
            acvedit$filter <-NA
            for (f in 1:nrow(filt)){
                if (filt$tipo[f] != 4){   # el de mueve la noche
                    ini <- filt$ini[f]
                    fin <- filt$fin[f]
                    range <- which(acvedit$time >= ini & acvedit$time <= fin)
                    acvedit$filter[range] <- filt$tipo[f]
                }
            }
            
            # Guarda ahora los dos
            saveRDS(newfiltro, paste0(awdfile(), ".edit.RDS"))
            saveRDS(acvedit,  paste0(awdfile(), ".acv.edit.RDS"))
        }
        removeModal()
    })
    
    
    # | -- 3. Editar actividad ------------------------------------------------
    # El ui de los periodos de sueño
    output$editActUI <- renderUI({
        data <- filter(tablaEstados(), estado == "S")
        data <- c(paste(data$inicio, " a ", data$termino))
        radioButtons("editAct.data", label = NULL, choices = data)
    })
    
    # Mostrar la duración
    output$editActDur <- renderPrint({
        # Tomar la tabla y filtrar
        data <- str_split(input$editAct.data, " a ", simplify = TRUE)
        data <- dmy_hm(data[1])
        data <- format(data,  format = "%d-%m-%Y %H:%M")
        data <- filter(tablaEstados(), inicio == data)
        cat(data$duracion)
    })
    
    # Modal de confirmación
    warnModal.editAct <- function(){
        modalDialog(
            title = "Modificar actividad",
            size = "m",
            easyClose = TRUE,
            
            div(span(p("Va a modificar la actividad entre: ", code(input$editAct.data)))),
            
            footer = tagList(
                modalButton("Cancelar"),
                actionButton("editAct.mdl", "Confirmar")
            )
        )
    }

    # Mostrar modal al apretar boton
    observeEvent(input$editAct.btn, {
        showModal(warnModal.editAct())
    })
    
    
    # Ejecutar si se confirma
    observeEvent(input$editAct.mdl, {
        # Transformar el input en data.frame test <- "15-07-2014 16:52 a 15-07-2014 16:58"
        data <- str_split(input$editAct.data, " a ", simplify = TRUE)
        ini <- dmy_hm(data[1])
        fin <- dmy_hm(data[2])
        data <- data.frame(id = NA, ini = ini, fin = fin, tipo = 3)
        
        # Update del filterRDS()
        filt <- bind_rows(filterRDS()$filter, data)
        filt <- arrange(filt, fin)
        filt <- distinct(filt, id, ini, fin, tipo)
        filt$id <- 1:nrow(filt)
        newfiltro <- list(header = filterRDS()$header, filter = filt)
        
        # Update al acv.edit 
        acvedit <- readRDS(paste0(awdfile(), ".acv.edit.RDS"))
        acvedit$filter <-NA
        for (f in 1:nrow(filt)){
            # Edita la variable "filter"
            ini <- filt$ini[f]
            fin <- filt$fin[f]
            range <- which(acvedit$time >= ini & acvedit$time <= fin)
            acvedit$filter[range] <- filt$tipo[f]
            
            if (filt$tipo[f] == 3){
                # Edita la actividad en variable "act.edit"
                df <- filter(acvedit, act.edit > 0)
                mu <- mean(df$act.edit, na.rm = TRUE)
                de <- sd(df$act.edit, na.rm = TRUE)
                
                newActiv <- rnorm(n = length(range), mean = floor(mu), sd = floor(de*0.8))
                newActiv <- ifelse(newActiv < 0, de, newActiv)
                acvedit$act.edit[range] <- newActiv
                
                # Edita el estado en la variable "st.edit"
                acvedit$st.edit[range] <- "W"
            }
        }
        
        # Guarda ahora los dos
        saveRDS(newfiltro, paste0(awdfile(), ".edit.RDS"))
        saveRDS(acvedit,  paste0(awdfile(), ".acv.edit.RDS"))
        removeModal()
    })
    
    
    
    # | -- 4. En mover noche --------------------------------------------------
    # el ui que muestra las horas disponibles
    output$moveNightUI <- renderUI({
        validate(need(input$perChoose, "Esperando input!"))
        
        # Validar qué mostrar
        if (input$moveNightEscena == "Inicio Vigilia"){
            if (nrow(filter(tablaEstados(), estado == "W")) == 0){
                radioButtons("moveNight.data", label = NULL, choices = c("No hay episodios"))
            } else {
                valor <- filter(tablaEstados(), estado == "W")
                valor <- valor$inicio
                radioButtons("moveNight.data", label = NULL, choices = valor)
            }
            
        } else if (input$moveNightEscena == "Fin Vigilia"){
            if (nrow(filter(tablaEstados(), estado == "W")) == 0){
                radioButtons("moveNight.data", label = NULL, choices = c("No hay episodios"))
            } else {
                valor <- filter(tablaEstados(), estado == "W")
                valor <- valor$termino
                radioButtons("moveNight.data", label = NULL, choices = valor)
            }
            
        } else if (input$moveNightEscena == "Inicio Sueño"){
            if (nrow(filter(tablaEstados(), estado == "S")) == 0){
                radioButtons("moveNight.data", label = NULL, choices = c("No hay episodios"))
            } else {
                valor <- filter(tablaEstados(), estado == "S")
                valor <- valor$inicio
                radioButtons("moveNight.data", label = NULL, choices = valor)
            }
            
        } else if (input$moveNightEscena == "Fin Sueño"){
            if (nrow(filter(tablaEstados(), estado == "S")) == 0){
                radioButtons("moveNight.data", label = NULL, choices = c("No hay episodios"))
            } else {
                valor <- filter(tablaEstados(), estado == "S")
                valor <- valor$termino
                radioButtons("moveNight.data", label = NULL, choices = valor)
            }
        }
    })
    
    # La duracion del episodio seleccionado
    output$moveNight.Dur <- renderPrint({
        validate(need(input$perChoose, "Esperando input!"))
        validate(need(input$moveNight.data, "Esperando"))
        
        # Tomar la tabla y filtrar
        data <- input$moveNight.data
        
        if (input$moveNight.data == "No hay episodios"){
            cat("No hay actividad")
        } else if (input$moveNightEscena == "Inicio Vigilia" | input$moveNightEscena == "Inicio Sueño"){
            data <- filter(tablaEstados(), inicio == data)
            cat(data$duracion)
        } else if (input$moveNightEscena == "Fin Vigilia" | input$moveNightEscena == "Fin Sueño"){
            data <- filter(tablaEstados(), termino == data)
            cat(data$duracion)
        } else {
            cat("0m 0s")
        }
        
    })

    # Modal de confirmación
    warnModal.moveNight <- function(){
        modalDialog(
            title = "Determinar hora inicio noche",
            size = "m",
            easyClose = TRUE,
            
            div(span(p("El inicio de noche para este período queda establecido en:"), 
                     code(input$moveNight.data))),
            
            footer = tagList(
                modalButton("Cancelar"),
                actionButton("moveNight.OK", "Confirmar")
            )
        )
    }
    
    # Mostrar modal al apretar boton Mover Noche
    observeEvent(input$moveNight.btn, {
        if (input$moveNight.data == "No hay episodios"){
            showNotification("Periodo no es válido", closeButton = FALSE, type = "message")
        } else {
            showModal(warnModal.moveNight())
        }
    })
    
    # Acciones a tomar: Agregar al filtro Mover Noche <"18-07-2014 02:33">
    observeEvent(input$moveNight.OK, {
        # Crear el data.frame primero
        data <- dmy_hm(input$moveNight.data)
        data <- data.frame(id = NA, ini = data, fin = NA, tipo = 4)
        
        # Update del filterRDS()
        filt <- bind_rows(filterRDS()$filter, data)
        filt <- arrange(filt, ini)
        filt <- mutate(filt, id = ifelse(is.na(fin), NA, id))
        filt <- distinct(filt, id, ini, fin, tipo)
        filt$id <- 1:nrow(filt)
        newfiltro <- list(header = filterRDS()$header, filter = filt)
        saveRDS(newfiltro, paste0(awdfile(), ".edit.RDS"))
        removeModal()
    })

    
    # | -- 5. Borrar filtro ---------------------------------------------------
    # Mostrar el filtro a borrar
    output$dropChoose <- renderUI({
        # Sacar ambos datos
        gdata <- acveditRDS()$semiper[[str_sub(input$perChoose, 1, 5)]]["time"]
        gdata <- format(ymd_hms(gdata$time), format = "%d-%m-%Y  %H:%M")
        
        filtro <- filterRDS()$filter[["ini"]]
        filtro <- format(ymd_hms(filtro), format = "%d-%m-%Y  %H:%M")
        
        # Ver qué combina y sacar el id
        indx <- filtro %in% gdata
        id <- filterRDS()$filter[["id"]]
        id <- id[indx]
        
        # Ajuste por si no hay filtros
        if (sum(id) == 0){
            radioButtons("todrop", choices = c("Periodo sin filtros"), label = NULL)
            
        } else {
            # Sección del filtro real
            temp <- filterRDS()$filter[id, ]
            temp$ini <- format(temp$ini, format = "%d-%m-%Y  %H:%M")
            temp$fin <- format(temp$fin, format = "%d-%m-%Y  %H:%M")
            
            # Construir vector de choose
            btnChoose <- NULL
            for (i in 1:length(id)){
                btn <- c(temp[i,])
                btn <- paste(btn, collapse = " | ")
                btnChoose <- c(btnChoose, btn)
            }
            radioButtons("todrop", choices = btnChoose, label = NULL)
        }
    })

    # Modal de confirmación
    warnModal.borrar <- function(){
        # Mensaje
        show <- str_split(input$todrop, " | ", simplify = TRUE)
        show <- show[1]
        
        # Dialogo
        modalDialog(
            title = "Confirmar borrar filtro",
            size = "m",
            easyClose = TRUE,
            div(span("Filtro a borrar ID = ", code(show))),
            footer = tagList(modalButton("Cancelar"), actionButton("borraFiltroOk", "Confirmar"))
        )
    }
    
    # Mostrar modal al apretar boton
    observeEvent(input$borraFiltroBtn, {
        if (length(filterRDS()) != 2){
            cat("No hay nada de filtro")
        } else if (input$todrop == "Periodo sin filtros"){
            showNotification("Período no tiene filtros", closeButton = FALSE, type = "message")
        } else {
            showModal(warnModal.borrar())
        }
    })
    
    # Si confirma se borra filtro y actualiza el acv.edit()
    observeEvent(input$borraFiltroOk, {
        showNotification("Procesando...")
        # El numero del filtro
        borraFiltroNum <- str_split(input$todrop, " | ", simplify = TRUE)
        borraFiltroNum <- as.numeric(borraFiltroNum[1])
        
        # Que el archivo de filtro tenga al menos 1 registro
        if (nrow(filterRDS()$filter) >= 0){
            # ---Update del acvedit---
            acvedit <- readRDS(paste0(awdfile(), ".acv.edit.RDS"))
            filtroDrop <- filterRDS()$filter[borraFiltroNum,]
            ini <- filtroDrop$ini[1]
            fin <- filtroDrop$fin[1]
            range <- which(acvedit$time >= ini & acvedit$time <= fin)
            tipo <- filtroDrop$tipo[1]
            # Aplicar
            if (tipo == 3){
                acvedit$st.edit[range] <- acvedit$st.stable[range]
                acvedit$act.edit[range] <- acvedit$act.smooth[range]
                acvedit$filter[range] <- NA
            } else {
                acvedit$filter[range] <- NA
            }
            saveRDS(acvedit,  paste0(awdfile(), ".acv.edit.RDS"))

            # Update al filtro
            filt <- filterRDS()$filter[-borraFiltroNum,]
            filt <- arrange(filt, ini)
            filt <- mutate(filt, id = NA)
            filt <- distinct(filt, id, ini, fin, tipo)
            if (nrow(filt) > 0){filt$id <- 1:nrow(filt)}
            newfiltro <- list(header = filterRDS()$header, filter = filt)
            saveRDS(newfiltro, paste0(awdfile(), ".edit.RDS"))
        }
        removeModal()
    })

    
    # | -- 6. Mostrar estados ------
    output$estadosTabla <- renderTable({
        validate(need(input$perChoose, "Esperando input!"))
        stagesTable(acveditRDS(), input$perChoose)
    })
    
    
    
    
    # _________________________________________ -------------------------------
    # EPISODIOS ---------------------------------------------------------------
    # | -- Crear el reactive EPI ----
    epi <- reactive({
        data <- select(create.epi(acveditRDS()), 
                       file, ini, fin, estado, duracion, 
                       dianoc, periodo, filtro)
        data$ini <- format(data$ini, format = "%d-%m-%Y %H:%M")
        data$fin <- format(data$fin, format = "%d-%m-%Y %H:%M") 
        names(data) <- c("AWD", "Inicio", "Fin", "Estado", "Duracion", 
                         "DiaNoc", "Periodo", "Filtro")
        data
    })
    
    # | -- Filtro para el periodo ----
    output$periodFilter <- renderUI({
        # Mejor las fechas
        
        
        periodList <- as.data.frame(cbind(str_split_fixed(unique(epi()$Periodo), " ", 2), 
                                  unique(epi()$Periodo)), stringsAsFactors = FALSE)
        periodList <- arrange(periodList, V2) %>% pull(V3)
        periodList <- c("Todo", periodList)

        selectInput(inputId = "epiPeriod", label = "Filtrar por período", 
                    selected = "Todo", choices = periodList, width = "100%", 
                    multiple = TRUE)
    })
    
    # | -- Tabla EPI ----
    output$epi <- renderTable({
        # Condicionales en variables secundarias
        if (is.null(input$estadoFilter)) {ef <- 0} else {ef <- 1}   # estado = ef
        if (is.null(input$dianocFilter)) {df <- 0} else {df <- 1}   # dianoc = df
        if (is.null(input$epiPeriod)) {                             # period = pf
            pf <- 0
        } else if (input$epiPeriod == "Todo"){
            pf <- 0
        } else {pf <- 1}

        # Si no hay selecciones
        if (ef == 0 & df == 0 & pf == 0){
            epi()
        # Solo estado
        } else if (ef == 1 & df == 0 & pf == 0){
            epi()[epi()$Estado %in% input$estadoFilter, ]
        # Solo Dianoc
        } else if (ef == 0 & df == 1 & pf == 0) {
            epi()[epi()$DiaNoc %in% input$dianocFilter, ]
        # Solo Periodo
        } else if (ef == 0 & df == 0 & pf == 1){
            epi()[epi()$Periodo %in% input$epiPeriod, ]
        # Estado y Dianoc
        } else if (ef == 1 & df == 1 & pf == 0){
            epi()[(epi()$Estado %in% input$estadoFilter) &
                  (epi()$DiaNoc %in% input$dianocFilter), ]
        # Estado y Periodo
        } else if (ef == 1 & df == 0 & pf == 1){
            epi()[(epi()$Estado  %in% input$estadoFilter) &
                  (epi()$Periodo %in% input$epiPeriod), ]
        # DiaNoc y Periodo
        } else if (ef == 0 & df == 1 & pf == 1){
            epi()[(epi()$DiaNoc  %in% input$dianocFilter) &
                  (epi()$Periodo %in% input$epiPeriod), ]
        # Los 3
        } else if (ef == 1 & df == 1 & pf == 1){
            epi()[(epi()$DiaNoc  %in% input$dianocFilter) &
                  (epi()$Periodo %in% input$epiPeriod)    &
                  (epi()$Estado  %in% input$estadoFilter), ]
        } else {
            data.frame(Error = "Algo pasó con los filtros... reportar!!!")
        }

    }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "xs",
    width = "100%", digits = 1, align = "c")

    
    # | ----
    # Panel - ESTADISTICAS -------------------------------- -------------------
    output$test1 <- renderPrint({

        filter(tablaEstados(), estado == "W")
        
    })
    
    output$test2 <- renderPrint({
        filterRDS()
    })
    

}

















