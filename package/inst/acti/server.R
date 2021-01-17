# OlitoSleep App de sueño más App del mercado ---------------------------------
server <- function(input, output, session){    
    
    # _________________________________________ -------------------------------
    # ARCHIVOS ----
    # | -- Seleccionar directorio ---------------------------------------------    
    path <- reactiveValues()
    
    # ---- Checar exista savedir ---- #   y cargar el set
    # El savedir se guarda en un archivo en el user folder de windows
    savedir_path <- file.path(Sys.getenv("USERPROFILE"), "savedir")
    
    # Si no existe lo crea
    if (file.exists(savedir_path) == FALSE){
        writeLines(Sys.getenv("USERPROFILE"), savedir_path)
        path$ruta <- Sys.getenv("USERPROFILE")
        setwd(Sys.getenv("USERPROFILE"))
    # Si existe lo carga
    } else {
        path$ruta <- readLines(savedir_path)
        
        # Hay que revisar que exista el dir por si movieron los archivos
        if (dir.exists(readLines(savedir_path)) == TRUE){
            setwd(readLines(savedir_path))
        } else {
            writeLines(Sys.getenv("USERPROFILE"), savedir_path)
            path$ruta <- Sys.getenv("USERPROFILE")
            setwd(Sys.getenv("USERPROFILE"))
        }
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

    # | -- -- Table directorio ------------------------------------------------
    output$dfdir <- renderTable({
        validate(need(subjectDF(), "Esperado datos!"))
        
        # Los radio buttons para el filtraje
        if (input$filterDir != "Todos"){
            filter(subjectDF(), Status == input$filterDir)            
        } else {
            tabladir <- subjectDF()
            
            # Por si no hubieran archivos
            if (tabladir[1,1] != "Directorio sin archivos AWD"){
                names(tabladir) <- c("Id Sujeto", "Archivos", "Terminado", "Status")
                tabladir
            } else {
                tabladir
            }
            
        }
    }, digits = 0, align = "c", striped = TRUE, hover = TRUE, width = "100%", spacing = "s")

    
    # | -- -- Tabla recuentos -------------------------------------------------
    # Aca voy a insertar el procesado del archivo de settings.  <<SETTINGS>>
    output$tableDir <- renderTable({
        validate(need(subjectDF(), "Esperado datos!"))
            
        if (subjectDF()[1,1] != "Directorio sin archivos AWD"){
            # Crear la tabla de recuentos
            summDF <- otable(rvar = "Status", data = subjectDF())
            summDF <- mutate(summDF, pct = pct*100, pct = paste(pct, "%", sep=""))
            summDF <- rename(summDF, N = freq, Porcentaje = pct)
            levels(summDF$Status)[length(levels(summDF$Status))] <- "Total"
            summDF
        } else {
            data.frame(Status = "Directorio sin archivos AWD")
        }
    })


    # | -- Parametros detección -----------------------------------------------
    # | -- -- Settings ----
    set <- reactive({ 
        if (subjectDF()[1,1] != "Directorio sin archivos AWD"){
            # Ver si pasa el settings
            getset(awdfolder())
        } else {
            list(ininoc=NA, inidia=NA, dursleep=NA, durawake=NA, statedur = NA, sensivar=NA)
        }
    }) 
    
    # Los parametros se leen de la lista de settings.
    output$showSet <- renderUI({
        HTML(
            paste("<strong>Hora inicio período nocturno:</strong><br>",       set()$ininoc, "<br><br>",
                  "<strong>Duración mínima primer sueño:</strong><br>",       set()$dursleep, " mins<br><br>",
                  "<strong>Hora inicio período diurno:</strong><br>",         set()$inidia, "<br><br>",
                  "<strong>Duración mínima primera vigilia:</strong><br>",    set()$durawake, " mins<br><br>",
                  "<strong>Tiempo para consolidación de estado</strong><br>", set()$statedur/60, " mins<br><br>",
                  "<strong>Sensibilidad de la deteción</strong><br>",         set()$sensivar, sep = "")
        )
    })

    # | -- -- Botón procesar en masa "massProc" -------------------------------
    # massProc: Modal de confirmación
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

    # massProc: Evento boton y error si no hay para procesar
    observeEvent(input$massProc, {
        nopro <- filter(subjectDF(), Status == "No procesado") %>% select(Sujeto)
        
        if (nrow(nopro) == 0){
            showNotification("No hay sujetos para procesar", type = "error", duration = 1, closeButton = FALSE)
        } else {
            showModal(warnModal.mass())
        }
    })    

    # massProc: Si se acepta el modal
    observeEvent(input$mass, {
        # Listado de sujetos
        procAwd <- filter(subjectDF(), Status == "No procesado")
        procAwd <- select(procAwd, Sujeto)
        procAwd <- paste0(procAwd$Sujeto, ".AWD")
        
        # <<<<< Secuencia de procesado >>>>>>
        for (sujeto in procAwd){
            showNotification(paste("Cargando", sujeto), closeButton = FALSE, duration = 1)
            
            acv <- create.acv(sujeto, set())
            semiper <- create.semiper(acv, set())
            filter.stats <- create.firstfilter(sujeto, semiper)
            acv.edit <- create.acvedit(sujeto, acv, filter.stats)
            
            showNotification("Sujeto procesado", type = "message", closeButton = FALSE, duration = 1)
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
        validate(need(subjectDF(), "Esperando datos!"))
        lechoices <- filter(subjectDF(), Status == "En edicion")
        lechoices <- as.character(lechoices$Sujeto)
        
        # Por si queda en cero
        if (length(lechoices) == 0){
            # print("los botones1")
            radioButtons(inputId = "awd_select", label = NULL, choices = c("No hay sujetos"))
        } else {
            radioButtons(inputId = "awd_select", label = NULL, choices = lechoices)
        }
    })

    # | Boton Editar ----------------------------------------------------------
    # Apretar y nos vamos a pestaña siguiente
    observeEvent(input$edEdit.btn, {
        showNotification("Procesando...", closeButton = FALSE, type = "message", duration = 1)
        updateNavbarPage(session, inputId = "TablasApp", selected = "Edición")
    })

    # | Boton Finalizar -------------------------------------------------------
    # edFin.btn: Modal cuando hay inicio y fin
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
    
    # edFin.btn: Modal de cuando no hay inicio y fin
    edFin.modalNO <- function(){
        modalDialog(
            title = "Falta acción",
            size = "m",
            easyClose = TRUE,
            div(h4("No se ha determinado el inicio y final"), h4("O hay un excluir mal configurado")),
            footer = tagList(
                modalButton("Cancelar"),
            )
        )
    }
    
    # Modales de procesado
    epiModal <- function(){
        cat("Procesando Episodios\n")
        modalDialog(title = "Procesando...", size = "s", easyClose = FALSE, strong("Procesando Episodios"), footer = NULL, fade = FALSE)
    }
    actModal <- function(){
        cat("Procesando actograma\n")
        modalDialog(title = "Procesando...", size = "s", easyClose = FALSE, strong("Procesando Actograma"), footer = NULL, fade = FALSE)
    }
    statModal <- function(){
        cat("Procesando stats\n")
        modalDialog(title = "Procesando...", size = "s", easyClose = FALSE, strong("Procesando Estadísticas"), footer = NULL, fade = FALSE)
    }
    xlsxModal <- function(){
        cat("Procesando excel\n")
        modalDialog(title = "Procesando...", size = "s", easyClose = FALSE, strong("Exportando Excel"), footer = NULL, fade = FALSE)
    }
    
    # edFin.btn: Botón para mostrar el modal
    observeEvent(input$edFin.btn, {
        # Antecedentes
        head <- filterRDS()$header
        
        filt <- filterRDS()$filter
        filt <- filter(filt, tipo == "Excluir")
        test <- TRUE
        if (length(filt) > 0){
            if (nrow(filt) > 0){
                for (i in 1:nrow(filt)){
                    if (is.na(filt$ini[i]) | is.na(filt$fin[i])){
                        test <- FALSE
                    }
                }
            }
        }
            
        if (head[4] == "Inicia:  -No determinado- " | head[5] == "Termina: -No determinado- " | test == FALSE){
            showModal(edFin.modalNO())
        } else {
            showModal(edFin.modal())
        }
    })

    # edFin.btn: Mostrar el modal y ejecutar acciones 
    observeEvent(input$finalOK,{
        removeModal()
        cat("\n----------------------------------------------------------\n")
        cat(paste("Terminando sujeto", awdfile(), "....\n"))
        
        # ------ Archivo de terminado y epi
        showModal(epiModal())
        filename <- paste0(awdfile(), ".finished.RDS")
        txt <- c("Sujeto terminado", awdfile(), "Fecha", now())
        saveRDS(txt, file = file.path(awdfolder(), filename))
        
        # Archivo epi
        epi <- create.epi(acveditRDS(), filterRDS(), set())
        saveRDS(epi, file = paste0(awdfile(), ".epi.RDS"))
        removeModal()
        
        # ------ Guardar actograma 
        showModal(actModal())
        cat("Guardando Actograma...\n")
        w <- 1400
        h <- (length(acveditRDS()[["semiper"]]) * 110 + 220) * 1.5
        filename <- paste0(awdfile(), ".actogram.png")
        png(filename, width = w, height = h, pointsize = 20)
            create.actogram(acveditRDS()[["semiper"]], set = set(), filterRDS = filterRDS())
        dev.off()
        removeModal()
        
        # | ----- Calcular STATS ---------------------------------------------------------
        showModal(statModal())
        epi <- create.epi(acveditRDS(), filterRDS(), set())
        epi <- epi$epiviejo
        
        # Pre-procesar
        cat("Procesando eventos válidos...")
        epi <- function_ValidEvents(epi)                # 1 
        drop <- epi$drop
        epi <- epi$datos
        
        # Validar que se puede analizar
        cat("Revisando EPI...")
        epi <- select(epi, -actividad)
        check.epidata(epi)                              # 2
        
        # Hacer los analisis
        cat("Calculando estadísticas...\n")
        horaini <- function_hi(epi)                     # 3
        conteo <- function_conteo(epi)                  # 4
        duracion <- function_duration(epi)              # 5
        maximos <- function_duracionMax(epi)            # 6
        latencia <- function_latencia(epi)              # 7
        CausaEfecto <- function_combi24h(epi)           # 8
        
        # Y los peridos de 24 horas
        par24horas <- function_24h(epi)                 # 9
        drop <- bind_rows(drop, par24horas$sinpar)
        par24horas <- par24horas$conpar
        
        # Combinar resultado
        stats <- list(epi = epi,
                      drop = drop,
                      horaini = horaini, 
                      conteo = conteo,
                      duracion = duracion,
                      maximos = maximos,
                      latencia = latencia,
                      CausaEfecto = CausaEfecto,
                      par24horas = par24horas)
        saveRDS(file = paste0(awdfile(), ".stats.RDS"), object = stats)
        removeModal()
        
        # Crear el Excel
        cat("\nExportando Excel...\n")
        showModal(xlsxModal())
        excel <- createWorkbook()
        hojas <- names(stats)
        for (xls in hojas){
            addWorksheet(excel, xls)
            eval(parse(text = paste0("writeData(excel, '", xls, "', ", xls, ")")))
            eval(parse(text = paste0("freezePane(excel, '", xls, "', firstRow = TRUE)")))
            eval(parse(text = paste0("c <- ncol(", xls, ")")))
            eval(parse(text = paste0("setColWidths(excel, '", xls, "', cols = 1:", c, ", widths = 'auto')")))
        }
        saveWorkbook(excel, paste0(awdfile(), ".stats.xlsx"), overwrite=TRUE)
        removeModal()
        
        # ----- Listo finalizar 
        showNotification(paste("Listo :)", awdfile()), closeButton = FALSE, type = "message")
        cat(paste("Sujeto:", awdfile(), "terminado\n"))
        cat("----------------------------------------------------------\n")
    })
    

    # | Actograma -------------------------------------------------------------
    # El ui render, el height se setea grande para que no de error de margins
    output$actoUI <- renderUI({
        validate(need(awdfile(), "Esperando datos!"))
        
        # Carga al tiro
        if (awdfile() == "No hay sujetos"){
            plot(0, type='n', axes=FALSE, ann=FALSE)
        } else {
            h <- length(acveditRDS()[["semiper"]]) * 110 + 220
            plotOutput("actograma", width = "100%", height = h)
        }
    })
    
    # Dibuja el actograma a partir del awdfile() y el acvditRDS() 
    output$actograma <- renderPlot({
        validate(need(acveditRDS(), "Esperando datos!"))
        
        
        # Verifica que haya algo para graficar
        if (length(acveditRDS()[["semiper"]]) > 0){
            create.actogram(acveditRDS()[["semiper"]], set = set(), filterRDS = filterRDS())
        } else {
            plot(0, type='n', axes=FALSE, ann=FALSE)
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
        fichero <- str_c(awdfile(), ".acvedit.RDS")
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
        fichero <- str_c(awdfile(), ".acvedit.RDS")
        if (file.exists(fichero)){
            return(check.acvfilter(awdfile(), set()))
        } else {
            return(1)
        }
    }
    
    # El poll
    acveditRDS <- reactivePoll(250, session, checkFunc = acvedit.check, valueFunc = acvedit.get)
    
    
    
    # _________________________________________ -------------------------------
    # EDICION -----------------------------------------------------------------

    # | -- Sujeto en edición --------------------------------------------------
    # Cada vez se carga el awdfile(), lo primero es mostrarlo
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
                          set(), filterRDS(), limites = input$rangoX, lw = input$ldNum)
    })

    # | -- Slider y control del gráfico ----
    # Reset Range btn
    observeEvent(input$resetBtn, {
        # El reset debiera calzar con el del grafico
        xscale <- seq(as.numeric(set()$ininoc)/3600, length.out = 25)
        updateSliderInput(session, "rangoX", value = c(min(xscale), max(xscale)))
        updateNumericInput(session, "ldNum", value = 1)
    })
    
    # Boton volver a actograma
    observeEvent(input$volverActo, {
        updateNavbarPage(session, inputId = "TablasApp", selected = "Actograma")
    })

    # Render ui del slider
    output$sliderEdicion <- renderUI({
        xscale <- seq(as.numeric(set()$ininoc)/3600, length.out = 25)
        minui <- min(xscale)
        maxui <- max(xscale)
        
        sliderInput("rangoX", label = NULL,
                    min = minui, max = maxui, value = c(minui, maxui),
                    width = "95%", step = 1)
    })
    
    


    
    
    # | -- Inicio Fin filtro ----
    output$filtroIniFin <- renderPrint({
        if (length(filterRDS()) != 2){
            cat("No se ha seleccionado un sujeto")
        } else {
            cat(paste(filterRDS()$header[4:5], collapse = "\n"))
        }
    })
    
    
    # | -- Tabla de Filtros  ----
    output$filtroDF <- renderTable({
        # Si el filtroFinal no tiene length=2 es que no existe y carga el inicial
        if (length(filterRDS()) == 2){
            df <- filterRDS()$filter
            df$ini <- format(df$ini, format = "%d-%m-%Y  %H:%M")
            df$fin <- format(df$fin, format = "%d-%m-%Y  %H:%M")
            names(df) <- c("Id", "Inicio", "Final ", "Tipo")
            df
        }
    }, digits = 0, align = "c", striped = TRUE, hover = TRUE, width = "100%")

    
    
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
    # UI para seleccionar el inicio del registro
    output$inifin.iniUI <- renderUI({
        validate(need(input$perChoose, "Esperando input!"))
        # Debe ser el inicio de una vigilia
        iniW <- filter(tablaEstados(), estado == "W")
        selectInput("inifin.ini", label = NULL, choices = iniW$inicio)
    })

    # inifin.iniset: Funcion modal
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
    
    # inifin.iniset: Mostrar modal 
    observeEvent(input$inifin.iniset, {
        showModal(warnModal.ini())
    })
    
    # inifin.iniset: Acciones a tomar 
    observeEvent(input$inifin.iniOK, {
        # browser()
        # ----- Actualizar el header ----- #
        filt <- readRDS(paste0(awdfile(), ".edit.RDS"))
        newhead <- filt$header
        newhead[4] <- paste0("Inicia:  ", input$inifin.ini)
        
        # ----- Actualizar ACVedit ----- #
        acvedit <- readRDS(paste0(awdfile(), ".acvedit.RDS"))
        acvedit$filter[acvedit$filter == "Ini"] <- NA
        
        # Capturar limites del final 
        ini <- min(acvedit$time)
        fin <- dmy_hm(input$inifin.ini)
        range <- which(acvedit$time >= ini & acvedit$time < fin)
        
        if (length(range) == 0){
            acvedit$filter[1] <- "Ini"
            saveRDS(acvedit,  paste0(awdfile(), ".acvedit.RDS"))
        } else {
            acvedit$filter[range] <- "Ini"
            saveRDS(acvedit,  paste0(awdfile(), ".acvedit.RDS"))
        }
        
        # ---- Actualizar filtro ----- # 
        # Sacar la linea de "Ini" del filtro
        filt <- filt$filter
        filt <- filter(filt, tipo != "Ini")
        
        # Agregar la nueva linea
        temp <- data.frame(id = as.numeric(NA), 
                           ini = format(ini,  format = "%d-%m-%Y %H:%M"), 
                           fin = format(fin,  format = "%d-%m-%Y %H:%M"), 
                           tipo = "Ini", stringsAsFactors = FALSE)
        filt <- bind_rows(filt, temp)
        filt <- arrange(filt, ini)
        filt <- mutate(filt, id = NA)
        filt <- distinct(filt, id, ini, fin, tipo)
        filt$id <- 1:nrow(filt)
        
        # Guardar
        newfiltro <- list(header = newhead, filter = filt)
        saveRDS(newfiltro, paste0(awdfile(), ".edit.RDS"))
        
        # ----- Listo ----- #
        removeModal()
    })
    
    
    
    # | ------ Fin del registro ------ 
    # UI para seleccionar el fin del registro
    output$inifin.finUI <- renderUI({
        validate(need(input$perChoose, "Esperando input!"))
        # Debe ser un final de episodio no mas 
        selectInput("inifin.fin", label = NULL, choices = tablaEstados()$termino)
    })

    # inifin.fin: Funcion Modal 
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
    
    # inifin.fin: Mostrar modal 
    observeEvent(input$inifin.finset, {
        showModal(warnModal.fin())
    })
    
    # inifin.fin: Acciones a tomar
    observeEvent(input$inifin.finOK, {
        # ----- Actualizar el header ----- #
        filt <- readRDS(paste0(awdfile(), ".edit.RDS"))
        newhead <- filt$header
        newhead[5] <- paste0("Termina: ", input$inifin.fin)
        
        # ----- Actualizar ACVedit ----- #
        acvedit <- readRDS(paste0(awdfile(), ".acvedit.RDS"))
        
        # Borrarle los filtros "Fin" que contenga
        acvedit$filter[acvedit$filter == "Fin"] <- NA
        
        # Capturar limites del final (sirven para el lo siguiente igual)
        ini <- dmy_hm(input$inifin.fin)
        fin <- max(acvedit$time)
        
        # Escribir el acvedit
        range <- which(acvedit$time > ini & acvedit$time <= fin)
        
        if (length(range) == 0){
            acvedit$filter[nrow(acvedit)] <- "Fin"
            saveRDS(acvedit,  paste0(awdfile(), ".acvedit.RDS"))
        } else {
            acvedit$filter[range] <- "Fin"
            saveRDS(acvedit,  paste0(awdfile(), ".acvedit.RDS"))
        }
        
        # ---- Actualizar filtro ----- # 
        # Sacar la linea de "Fin" del filtro
        filt <- filt$filter
        filt <- filter(filt, tipo != "Fin")
        
        # Agregar la nueva linea
        temp <- data.frame(id = as.numeric(NA), 
                           ini = format(ini,  format = "%d-%m-%Y %H:%M"), 
                           fin = format(fin,  format = "%d-%m-%Y %H:%M"), 
                           tipo = "Fin", stringsAsFactors = FALSE)
        filt <- bind_rows(filt, temp)
        filt <- arrange(filt, ini)
        filt <- mutate(filt, id = NA)
        filt <- distinct(filt, id, ini, fin, tipo)
        filt$id <- 1:nrow(filt)
        
        # Guardar
        newfiltro <- list(header = newhead, filter = filt)
        saveRDS(newfiltro, paste0(awdfile(), ".edit.RDS"))
        
        # ----- Listo ----- #
        removeModal()
    })

    
    # | -- 2. Editar actividad ------------------------------------------------
    # El ui de los periodos de sueño
    output$editActUI <- renderUI({
        data <- filter(tablaEstados(), estado == "S")
        data <- c(paste(data$inicio, " - ", data$termino))
        radioButtons("editAct.data", label = NULL, choices = data)
    })
    
    # Mostrar la duración
    output$editActDur <- renderPrint({
        # tomar el input, parsearlo y buscar duracion en la tabla de estados
        data <- str_split(input$editAct.data, " - ", simplify = TRUE)
        data <- dmy_hm(data[1])
        data <- format(data,  format = "%d-%m-%Y %H:%M")
        data <- filter(tablaEstados(), inicio == data)
        cat(data$duracion)
    })

    # Boton editar actividad
    observeEvent(input$editAct.btn, {
        showModal(warnModal.editAct())
    })
    
    # Modal editar actividad
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

    # Ejecutar editar actividad
    observeEvent(input$editAct.mdl, {
        # Transformar el input en data.frame test <- "15-07-2014 16:52 a 15-07-2014 16:58"
        data <- str_split(input$editAct.data, " - ", simplify = TRUE)
        # ini <- dmy_hm(data[1])
        # fin <- dmy_hm(data[2])
        ini <- data[1]
        fin <- data[2]
        data <- data.frame(id = NA, ini = ini, fin = fin, tipo = "Actividad", stringsAsFactors = FALSE)
        
        # Update del filterRDS()
        filt <- bind_rows(filterRDS()$filter, data)
        filt <- arrange(filt, ini)
        filt <- distinct(filt, id, ini, fin, tipo)
        filt$id <- 1:nrow(filt)
        newfiltro <- list(header = filterRDS()$header, filter = filt)
        
        # Update al acv.edit 
        acvedit <- readRDS(paste0(awdfile(), ".acvedit.RDS"))
        acvedit$filter <-NA
        
        # Quedo en NA la variable "filter" para re calcular
        for (f in 1:nrow(filt)){
            # Edita la variable "filter"
            ini <- dmy_hm(filt$ini[f])
            fin <- dmy_hm(filt$fin[f])
            range <- which(acvedit$time >= ini & acvedit$time <= fin)
            acvedit$filter[range] <- filt$tipo[f]
            
            if (filt$tipo[f] == "Actividad"){
                # Calcula media y sd para simular actividad
                df <- filter(acvedit, act.edit > 0)
                mu <- mean(df$act.edit, na.rm = TRUE)
                de <- sd(df$act.edit, na.rm = TRUE)
                
                # Agrega la actividad en variable "act.edit"
                newActiv <- rnorm(n = length(range), mean = floor(mu), sd = floor(de*0.8))
                newActiv <- ifelse(newActiv < 0, de, newActiv)
                acvedit$act.edit[range] <- newActiv
                
                # Cambia el estado en la variable "st.edit"
                acvedit$st.edit[range] <- "W"
            }
        }
        
        # Guarda ahora los dos
        saveRDS(newfiltro, paste0(awdfile(), ".edit.RDS"))
        saveRDS(acvedit,  paste0(awdfile(), ".acvedit.RDS"))
        removeModal()
    })
    
    
    
    # | -- 3. En mover noche --------------------------------------------------
    # el ui que muestra las horas disponibles
    output$moveNightUI <- renderUI({
        validate(need(input$perChoose, "Esperando input!"))
        
        # Validar que mostrar
        if (input$moveNightEscena == "Mover Dia"){
            if (nrow(filter(tablaEstados(), estado == "W")) == 0){
                radioButtons("moveNight.data", label = NULL, choices = c("No hay episodios"))
            } else {
                valor <- filter(tablaEstados(), estado == "W")
                valor <- valor$inicio
                radioButtons("moveNight.data", label = NULL, choices = valor)
            }
            
        } else if (input$moveNightEscena == "Mover Noche"){
            if (nrow(filter(tablaEstados(), estado == "S")) == 0){
                radioButtons("moveNight.data", label = NULL, choices = c("No hay episodios"))
            } else {
                valor <- filter(tablaEstados(), estado == "S")
                valor <- valor$inicio
                radioButtons("moveNight.data", label = NULL, choices = valor)
            }
        
        } else {
            stop("error en seleccion de episodios disponibles")
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
        } else if (input$moveNightEscena == "Mover Dia" | input$moveNightEscena == "Mover Noche"){
            data <- filter(tablaEstados(), inicio == data)
            cat(data$duracion)
        } else {
            cat("0m 0s")
        }
        
    })

    # Mover noche: Boton confirmar en la UI
    observeEvent(input$moveNight.btn, {
        if (input$moveNight.data == "No hay episodios"){
            showNotification("Periodo no es válido", closeButton = FALSE, type = "message")
        } else {
            showModal(warnModal.moveNight())
        }
    })
    
    # Mover noche: Modal de confirmacion
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

    # Acciones a tomar: Agregar al filtro Mover Noche <"18-07-2014 02:33">
    observeEvent(input$moveNight.OK, {
        # Crear el data.frame primero
        data <- data.frame(id = NA, ini = input$moveNight.data, fin = NA, tipo = "Mover", stringsAsFactors = FALSE)
        
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
    
    
    
    # | -- 4. Excluir seccion --------------------------------------------------
    # | ---- Horas, episodio y duracion -----
    # Excluir: UI radio buttons horas disponibles
    output$ExcludeUI <- renderUI({
        validate(need(input$perChoose, "Esperando input!"))
        
        # Validar que mostrar
        if (nrow(tablaEstados()) == 0){
            radioButtons("Exclude.data", label = NULL, choices = c("No hay episodios"))
        } else {
            radioButtons("Exclude.data", label = NULL, choices = tablaEstados()[["termino"]])
        }
    })
    
    # Excluir: Mostrar tipo de episodio
    output$Exclude.stInfo <- renderPrint({
        validate(need(input$perChoose, "Esperando input!"))
        
        # Determinar la linea en tabla estados
        linea <- which(tablaEstados()[["termino"]] == input$Exclude.data)
        linea <- tablaEstados()[["estado"]][linea]
        
        # Mostrar segun corresponda
        if (length(linea) == 0){
            cat("No Disp.")
        } else if (linea == "W"){
            cat("Vigilia")
        } else if (linea == "S"){
            cat("Sueño")
        } else {
            cat("Error")
        }
    })
    
    # Excluir: Mostrar duracion
    output$Exclude.durInfo <- renderPrint({
        validate(need(input$perChoose, "Esperando input!"))
        
        # Determinar la linea en tabla estados
        linea <- which(tablaEstados()[["termino"]] == input$Exclude.data)
        linea <- tablaEstados()[["duracion"]][linea]
        linea <- as.character(as.period(linea) + minutes(1))
        
        # Mostrar segun corresponda
        if (length(linea) == 0){
            cat("No Disp.")
        } else {
            cat(linea)
        }
    })
    
    # ----------------------------------------------------------------------- #
    # | ---- Id Filtro, inicio y fin -----
    # Excluir: UI de Ids filtros disponibles
    output$ExcludeFiltros <- renderUI({
        validate(need(input$perChoose, "Esperando input!"))
        
        dataFiltro <- filter(filterRDS()$filter, tipo == "Excluir")
        dataFiltro <- as.character(dataFiltro$id)
        
        # Evaluar si no hay nada
        if (length(dataFiltro) == 0){
            radioButtons("Exclude.filterdata", label = NULL, choices = "Nuevo")
        } else {
            radioButtons("Exclude.filterdata", label = NULL, choices = c("Nuevo", dataFiltro))
        }
        
    })
    
    # Excluir: Mostrar inicio filtro
    output$Exclude.IniText <- renderPrint({
        validate(need(input$perChoose, "Esperando input!"))
        validate(need(input$Exclude.filterdata, "Wait!"))
        
        if (input$Exclude.filterdata == "Nuevo"){
            cat("Sin datos")
        } else {
            # Filtros "Excluir" en el RDS
            dataFiltro <- filter(filterRDS()$filter, tipo == "Excluir")
            dataFiltro <- mutate(dataFiltro, id = as.character(id))
            dataFiltro <- filter(dataFiltro, id == input$Exclude.filterdata)
            dataFiltro <- dataFiltro$ini    # Inicio OJO
            cat(dataFiltro)
        }
    })

    # Excluir: Mostrar final del filtro
    output$Exclude.FinText <- renderPrint({
        validate(need(input$perChoose, "Esperando input!"))
        validate(need(input$Exclude.filterdata, "Wait!"))
        
        if (input$Exclude.filterdata == "Nuevo"){
            cat("Sin datos")
        } else {
            # Filtros "Excluir" en el RDS
            dataFiltro <- filter(filterRDS()$filter, tipo == "Excluir")
            dataFiltro <- mutate(dataFiltro, id = as.character(id))
            dataFiltro <- filter(dataFiltro, id == input$Exclude.filterdata)
            dataFiltro <- dataFiltro$fin    # Inicio OJO
            cat(dataFiltro)
        }
    })

    # ----------------------------------------------------------------------- #
    # | ---- Set Inicio -----
    # Excluir: Boton Setear el inicio 
    observeEvent(input$Exclude.IniSet, {
        validate(need(input$Exclude.data, "wait"))
        validate(need(input$Exclude.filterdata, "Wait!"))
        
        if (input$Exclude.data == "No hay episodios"){
            showNotification("Periodo no es válido", closeButton = FALSE, type = "message")
        
        # Filtro nuevo
        } else if (input$Exclude.filterdata == "Nuevo") {
            showModal(warnModal.ExcludeIni())
        
            # Hay filtro seleccionado    
        } else {
            # Inicio
            dataFiltro <- filter(filterRDS()$filter, tipo == "Excluir")
            dataFiltro <- filter(dataFiltro, id == as.numeric(input$Exclude.filterdata))
            ini <- dataFiltro$ini 
            fin <- dataFiltro$fin
            
            # Si solo hay un valor
            if (is.na(ini) == TRUE & is.na(fin) == FALSE){
                showModal(warnModal.ExcludeIni())
            } else {
                ini <- dmy_hm(ini)
                fin <- dmy_hm(fin)
                if (ini >= fin){
                    showNotification("Error: Inicio menor a Fin", closeButton = FALSE, type = "error")
                } else {
                    showModal(warnModal.ExcludeIni())
                }
            }
        }
    })
    
    # Modal de setear el inicio
    warnModal.ExcludeIni <- function(){
        modalDialog(
            title = "Determinar inicio de Exclusión",
            size = "m",
            easyClose = TRUE,
            div(span(p("La exclusión de datos comienza a partir de:"), code(input$Exclude.data))),
            footer = tagList(
                modalButton(label = "Cancelar"),
                actionButton("ExcludeIniOK", label = "Confirmar")
            )
        )
    }
    
    # Acciones de setear el inicio
    observeEvent(input$ExcludeIniOK, {
        # ----- Update el filterRDS ------------------------------------------------ #
        # Si no hay filtros crear un registro
        if (input$Exclude.filterdata == "Nuevo"){
            filt <- data.frame(id = NA, ini = input$Exclude.data, fin = NA, tipo = "Excluir", stringsAsFactors = FALSE)
            filt <- bind_rows(filterRDS()$filter, filt)
            filt <- arrange(filt, ini)
            filt <- mutate(filt, id = NA)
            filt <- distinct(filt, id, ini, fin, tipo)
            filt$id <- 1:nrow(filt)
            
            newfiltro <- list(header = filterRDS()$header, filter = filt)
            saveRDS(newfiltro, paste0(awdfile(), ".edit.RDS"))
            
        # Si es numero, solo reemplazar el trozo que corresponde, con precision
        } else {
            # Exclude.filterdata = id del filtro               # Exclude.data = fecha seleccionada
            filt <- filterRDS()$filter
            filt$ini[filt$id == input$Exclude.filterdata] <- input$Exclude.data
            
            newfiltro <- list(header = filterRDS()$header, filter = filt)
            saveRDS(newfiltro, paste0(awdfile(), ".edit.RDS"))
        }
        
        
        # ----- Update el acv.edit ------------------------------------------------- #
        acvedit <- readRDS(paste0(awdfile(), ".acvedit.RDS"))
        
        # Borrarle los filtros "Excluir" que contenga
        acvedit$filter[acvedit$filter == "Excluir"] <- NA
        
        # Tomar el newfiltro y filtrar los "Excluir"
        filt <- newfiltro$filter
        filt <- filter(filt, tipo == "Excluir")
        
        # Escribir cada linea de filtroen el acvedit
        for (exc in 1:nrow(filt)){
            # Primero revisar que esten las dos fechas
            tempIni <- filt$ini[exc]
            tempFin <- filt$fin[exc]
            
            # Revisar que tengan los 2, si no pasa
            if (is.na(tempIni) == FALSE & is.na(tempFin) == FALSE){
                # Editar la variable "filter" en "acvedit"
                ini <- dmy_hm(filt$ini[exc])
                fin <- dmy_hm(filt$fin[exc])
                range <- which(acvedit$time > ini & acvedit$time <= fin)
                acvedit$filter[range] <- filt$tipo[exc]
            }
        }
        
        # Listo y guarda
        saveRDS(acvedit,  paste0(awdfile(), ".acvedit.RDS"))
        
        
        removeModal()
    })
    

    # ----------------------------------------------------------------------- #
    # | ---- Set Final -----
    # Excluir: Boton Setear el final 
    observeEvent(input$Exclude.FinSet, {
        validate(need(input$Exclude.data, "wait"))
        validate(need(input$Exclude.filterdata, "Wait!"))
        
        if (input$Exclude.data == "No hay episodios"){
            showNotification("Periodo no es válido", closeButton = FALSE, type = "message")
            
        # Filtro nuevo
        } else if (input$Exclude.filterdata == "Nuevo") {
            showModal(warnModal.ExcludeFin())
            
        # Hay filtro seleccionado    
        } else {
            # data de Inicio
            dataFiltro <- filter(filterRDS()$filter, tipo == "Excluir")
            dataFiltro <- filter(dataFiltro, id == as.numeric(input$Exclude.filterdata))
            ini <- dataFiltro$ini 
            fin <- dataFiltro$fin
            
            # Con un NA
            if (is.na(fin) == TRUE & is.na(ini) == FALSE){
                showModal(warnModal.ExcludeFin())
            } else {
                ini <- dmy_hm(ini)
                fin <- dmy_hm(fin)
                if (ini >= fin){
                    showNotification("Error: Inicio menor a Fin", closeButton = FALSE, type = "error")
                } else {
                    showModal(warnModal.ExcludeFin())
                }
            }
        }
        
        
    })
    
    # Modal de setear el final
    warnModal.ExcludeFin <- function(){
        modalDialog(
            title = "Determinar final de Exclusión",
            size = "m",
            easyClose = TRUE,
            div(span(p("La exclusión de datos termina en :"), code(input$Exclude.data))),
            footer = tagList(
                modalButton(label = "Cancelar"),
                actionButton("ExcludeFinOK", label = "Confirmar")
            )
        )
    }
    
    # Acciones de setear el inicio
    observeEvent(input$ExcludeFinOK, {
        
        # Si no hay filtros crear un registro
        if (input$Exclude.filterdata == "Nuevo"){
            filt <- data.frame(id = NA, ini = NA, fin = input$Exclude.data, tipo = "Excluir", stringsAsFactors = FALSE)
            filt <- bind_rows(filterRDS()$filter, filt)
            filt <- arrange(filt, ini)
            filt <- mutate(filt, id = NA)
            filt <- distinct(filt, id, ini, fin, tipo)
            filt$id <- 1:nrow(filt)
            
            newfiltro <- list(header = filterRDS()$header, filter = filt)
            saveRDS(newfiltro, paste0(awdfile(), ".edit.RDS"))
            
        # Si es numero, solo reemplazar el trozo que corresponde, con precision
        } else {
            # Exclude.filterdata = id del filtro   # Exclude.data = fecha seleccionada
            filt <- filterRDS()$filter
            filt$fin[filt$id == as.numeric(input$Exclude.filterdata)] <- input$Exclude.data
            
            newfiltro <- list(header = filterRDS()$header, filter = filt)
            saveRDS(newfiltro, paste0(awdfile(), ".edit.RDS"))
        }
        
        # ----------------------------------------------------------# 
        # Update el acv.edit
        acvedit <- readRDS(paste0(awdfile(), ".acvedit.RDS"))
        
        # Borrarle los filtros "Excluir" que contenga
        acvedit$filter[acvedit$filter == "Excluir"] <- NA
        
        # Tomar el newfiltro y filtrar los "Excluir"
        filt <- newfiltro$filter
        filt <- filter(filt, tipo == "Excluir")
        
        # Escribir cada linea de filtroen el acvedit
        for (exc in 1:nrow(filt)){
            # Primero revisar que esten las dos fechas
            tempIni <- filt$ini[exc]
            tempFin <- filt$fin[exc]
            
            # Revisar que tengan los 2, si no pasa
            if (is.na(tempIni) == FALSE & is.na(tempFin) == FALSE){
                # Editar la variable "filter" en "acvedit"
                ini <- dmy_hm(filt$ini[exc])
                fin <- dmy_hm(filt$fin[exc])
                range <- which(acvedit$time > ini & acvedit$time <= fin)
                acvedit$filter[range] <- filt$tipo[exc]
            }
        }
        
        # Listo y guarda
        saveRDS(acvedit,  paste0(awdfile(), ".acvedit.RDS"))
        
        
        removeModal()
    })
    
    
    
    
    # | -- 5. Borrar filtro ---------------------------------------------------
    # Mostrar id de filtro a borrar
    output$dropChoose <- renderUI({
        validate(need(input$perChoose, "Esperando input!"))
        # Capturar ids de filtro
        filt <- filterRDS()$filter
        filt <- filt$id
        if (length(filt) == 0){
            radioButtons("todrop", choices = c("Periodo sin filtros"), label = NULL)
        } else {
            radioButtons("todrop", choices = as.numeric(filt), label = NULL)
        }
    })

    # Mostrar info del filtro seleccionado
    output$todrop.info <- renderTable({
        validate(need(input$perChoose, "Esperando input!"))
        validate(need(input$todrop, "wait"))
        
        # Capturar linea
        filt <- filterRDS()$filter
        filt <- filter(filt, id == input$todrop)
        names(filt) <- c("Id", "Inicio", "Final ", "Tipo")
        filt
        
    }, digits = 0, align = "c", striped = TRUE, hover = TRUE, width = "100%")

    # Mostrar modal borrar filtro
    observeEvent(input$borraFiltroBtn, {
        # Para verificar que haiga filtros
        filt <- filterRDS()$filter
        filt <- filt$id
        if (length(filt) == 0){
            showNotification("Período no tiene filtros", closeButton = FALSE, type = "message")
        } else {
            showModal(warnModal.borrar())
        }
    })

    # Modal de borrar filtro
    warnModal.borrar <- function(){
        modalDialog(
            title = "Confirmar borrar filtro",
            size = "m",
            easyClose = TRUE,
            div(span("Filtro a borrar ID = ", code(input$todrop))),
            footer = tagList(
                modalButton("Cancelar"), 
                actionButton("borraFiltroOk", "Confirmar")
            )
        )
    }

    # Si confirma se borra filtro
    observeEvent(input$borraFiltroOk, {
        showNotification("Borrando...", closeButton = FALSE, duration = 1)
        
        # Captura la linea a borrar
        linea <- filterRDS()$filter
        linea <- filter(linea, id == as.numeric(input$todrop))
        
        # ------- Si es actividad --------------------------------------- # 
        if (linea$tipo == "Actividad"){
            # Los limites
            ini <- dmy_hm(linea$ini)
            fin <- dmy_hm(linea$fin)
            
            # Cargar acvedit y estimar el rango
            acvedit <- readRDS(paste0(awdfile(), ".acvedit.RDS"))
            range <- which(acvedit$time >= ini & acvedit$time <= fin)
            
            # Modificar datos originales
            acvedit$filter[range] <- NA
            acvedit$st.edit[range] <- "S"
            acvedit$act.edit[range] <- acvedit$act.smooth[range]
            
            # Guarda el acvedit restaurado
            saveRDS(acvedit,  paste0(awdfile(), ".acvedit.RDS"))
            
            # Ahora replica la instruccion de borrar la linea en el filtro
            # Update al filtro
            filt <- filterRDS()$filter
            filt <- filter(filt, id != as.numeric(input$todrop))
            filt <- arrange(filt, ini)
            filt <- mutate(filt, id = NA)
            filt <- distinct(filt, id, ini, fin, tipo)
            
            # Si queda filtro numerar y guardar
            if (nrow(filt) > 0){
                filt$id <- 1:nrow(filt)
            }
            newfiltro <- list(header = filterRDS()$header, filter = filt)
            saveRDS(newfiltro, paste0(awdfile(), ".edit.RDS"))
        
        # --------- Si es Excluir -------------------------------- #
        } else if (linea$tipo == "Excluir") {    
            # Los limites
            ini <- dmy_hm(linea$ini)
            fin <- dmy_hm(linea$fin)
            
            # Cargar acvedit y borrar el trozo
            acvedit <- readRDS(paste0(awdfile(), ".acvedit.RDS"))
            range <- which(acvedit$time >= ini & acvedit$time <= fin)
            acvedit$filter[range] <- NA
            saveRDS(acvedit,  paste0(awdfile(), ".acvedit.RDS"))
            
            # Ahora con el filtro
            filt <- filterRDS()$filter
            filt <- filter(filt, id != as.numeric(input$todrop))
            filt <- arrange(filt, ini)
            filt <- mutate(filt, id = NA)
            filt <- distinct(filt, id, ini, fin, tipo)
            
            # Si queda filtro numerar y guardar
            if (nrow(filt) > 0){filt$id <- 1:nrow(filt)}
            newfiltro <- list(header = filterRDS()$header, filter = filt)
            saveRDS(newfiltro, paste0(awdfile(), ".edit.RDS"))
            
        # --------- Si es inicio --------------------------------- #
        } else if (linea$tipo == "Ini") { 
            # Cargar datos y borrarle el filtro
            acvedit <- readRDS(paste0(awdfile(), ".acvedit.RDS")) 
            acvedit <- mutate(acvedit, filter = ifelse(filter == "Ini", NA, filter))
            saveRDS(acvedit,  paste0(awdfile(), ".acvedit.RDS"))
            
            # Borrar del data.frame del filtro
            filt <- filterRDS()$filter
            filt <- filter(filt, id != as.numeric(input$todrop))
            filt <- arrange(filt, ini)
            filt <- mutate(filt, id = NA)
            filt <- distinct(filt, id, ini, fin, tipo)
            if (nrow(filt) > 0){filt$id <- 1:nrow(filt)}
            
            # Borrar del encabezado
            encabe <- filterRDS()$header
            encabe[4] <- "Inicia:  -No determinado- "
            
            # Guardar el filtro
            newfiltro <- list(header = encabe, filter = filt)
            saveRDS(newfiltro, paste0(awdfile(), ".edit.RDS"))
            
        # --------Si es final ------------------------------------- #
        } else if (linea$tipo == "Fin") {    
            # Cargar datos y borrarle el filtro
            acvedit <- readRDS(paste0(awdfile(), ".acvedit.RDS")) 
            acvedit <- mutate(acvedit, filter = ifelse(filter == "Fin", NA, filter))
            saveRDS(acvedit,  paste0(awdfile(), ".acvedit.RDS"))
            
            # Borrar del data.frame del filtro
            filt <- filterRDS()$filter
            filt <- filter(filt, id != as.numeric(input$todrop))
            filt <- arrange(filt, ini)
            filt <- mutate(filt, id = NA)
            filt <- distinct(filt, id, ini, fin, tipo)
            if (nrow(filt) > 0){filt$id <- 1:nrow(filt)}
            
            # Borrar del encabezado
            encabe <- filterRDS()$header
            encabe[5] <- "Termina: -No determinado- "
            
            # Guardar el filtro
            newfiltro <- list(header = encabe, filter = filt)
            saveRDS(newfiltro, paste0(awdfile(), ".edit.RDS"))  
            
        # ----- Queda el mover noche ----------------------------------- #        
        } else {
            # Update al filtro
            filt <- filterRDS()$filter
            filt <- filter(filt, id != as.numeric(input$todrop))
            filt <- arrange(filt, ini)
            filt <- mutate(filt, id = NA)
            filt <- distinct(filt, id, ini, fin, tipo)
            
            # Si queda filtro numerar y guardar
            if (nrow(filt) > 0){ filt$id <- 1:nrow(filt) }
            newfiltro <- list(header = filterRDS()$header, filter = filt)
            saveRDS(newfiltro, paste0(awdfile(), ".edit.RDS"))
        }
        
        removeModal()
    })

    
    # | -- 6. Estados ------
    # | ---- Header Filtro ----
    output$filtroH <- renderPrint({
        # Si el filtroFinal no tiene length=2 es que no existe y carga el inicial
        if (length(filterRDS()) != 2){
            cat("No se ha seleccionado un sujeto")
        } else {
            cat(paste(filterRDS()$header[2:3], collapse = "\n"))
        }
    })
    
    # | ---- Tabla de estados ----
    output$estadosTabla <- renderTable({
        validate(need(input$perChoose, "Esperando input!"))
        
        # Calcular tabla
        tabla <- stagesTable(acveditRDS(), input$perChoose)
        tabla <- select(tabla, -filtro)
        tabla <- mutate(tabla, duracion = as.character(as.period(duracion) + minutes(1)))
        names(tabla) <- c("Inicio", "Final", "Estado", "Duracion")
        
        # Duracion todo
        if (input$tablaLength == "Todo"){
            # Todo estado
            if (input$tablaStage == "Todo"){
                tabla
            # S
            } else if (input$tablaStage == "S"){
                filter(tabla, Estado == "S")
            # W
            } else if (input$tablaStage == "W"){
                filter(tabla, Estado == "W") 
            }
            
        # Duracion mayor a    
        } else {
            # Calcular nueva tabla
            mindur <- (set()$dursleep + set()$durawake)/2
            mindur <- as.period(mindur, unit = "minute")
            mindur <- as.period(tabla$Duracion) > mindur
            tabla2 <- tabla[mindur,]
            
            # Todo estado
            if (input$tablaStage == "Todo"){
                tabla2
                # S
            } else if (input$tablaStage == "S"){
                filter(tabla2, Estado == "S")
                # W
            } else if (input$tablaStage == "W"){
                filter(tabla2, Estado == "W") 
            }
        }

    }, digits = 0, align = "c", striped = TRUE, hover = TRUE, width = "100%", spacing = "s")

    
    
    
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

    
    # _________________________________________ -------------------------------
    # Panel - ESTADISTICAS ----------------------------------------------------
    # | ---- Determina awdfin() ----------------------------------------------
    awdfin <-  reactive({
        input$awd_fin
    })
    
    # | renderUI selección de terminados -----------------------------------------
    output$subjInput2 <- renderUI({
        validate(need(subjectDF(), "Esperando datos!"))
        lechoices <- filter(subjectDF(), Status == "Terminado")
        lechoices <- as.character(lechoices$Sujeto)
        
        # Por si queda en cero
        if (length(lechoices) == 0){
            # print("los botones1")
            radioButtons(inputId = "awd_fin", label = NULL, choices = c("No hay sujetos"))
        } else {
            radioButtons(inputId = "awd_fin", label = NULL, choices = lechoices)
        }
    })
    
    # | ---- Boton re-Editar ------------------------------------------------------
    # Pasar a edicion
    reEditar.modal <- function(){
        modalDialog(
            title = "Pasar el sujeto a Edición",
            size = "m",
            easyClose = TRUE,
            div(h4(input$awd_fin),
                p(awdfile()),
                p(strong("Con esta acción se borran las estadísticas calculadas y se pasa el sujeto a condición de edición"))),
            footer = tagList(
                modalButton("Cancelar", icon = icon("window-close")),
                actionButton("reEditarOK", "re-Editar", icon = icon("edit"))  # El valor resultante
            )
        )
    }
    # Boton del UI para mostrar modal
    observeEvent(input$reEditar, {
        showModal(reEditar.modal())
    })
    # Confirmar modal
    observeEvent(input$reEditarOK,{
        showNotification("Borrando archivos...", duration = 2, closeButton = FALSE, type = "message")
        # Borrar asi no mas
        file.remove(file.path(getwd(), paste0(awdfin(), ".epi.RDS")))
        file.remove(file.path(getwd(), paste0(awdfin(), ".finished.RDS")))
        file.remove(file.path(getwd(), paste0(awdfin(), ".stats.RDS")))
        file.remove(file.path(getwd(), paste0(awdfin(), ".stats.xlsx")))
        file.remove(file.path(getwd(), paste0(awdfin(), ".actogram.png")))
        removeModal()
    })
    
    
    # | ---- Boton Compilar -------------------------------------------------------
    # Mensaje de compilar
    compi.modal <- function(){
        modalDialog(
            title = "Compilar estadisticas",
            size = "m",
            easyClose = TRUE,
            div(p(strong("Con esta acción se combinan las estadisticas de todos los sujetos en un solo archivo."))),
            footer = tagList(
                modalButton("Cancelar", icon = icon("window-close")),
                actionButton("compiOK", "Compilar", icon = icon("save")) 
            )
        )
    }
    # Boton del UI para mostrar modal
    observeEvent(input$compilar, {
        # Si no hay sujetos
        if (awdfin() == "No hay sujetos"){
            showNotification("No hay sujetos...", duration = 3, closeButton = FALSE, type = "error")
        } else {
            showModal(compi.modal())
        }
    })
    # Confirmacion del modal
    observeEvent(input$compiOK,{
        showNotification("Compilando archivos...", duration = 3, closeButton = FALSE, type = "message")
        validate(need(subjectDF(), "Esperando datos!"))
        
        # browser()
        # Crear nulos
        epi <- drop <- horaini <- conteo <- duracion <- maximos <- latencia <- CausaEfecto <- par24horas <- NULL
        
        # Lupeo y combinacion
        lechoices <- filter(subjectDF(), Status == "Terminado")
        lechoices <- as.character(lechoices$Sujeto)
        
        for (f in lechoices){
            temp <- readRDS(paste0(f, ".stats.RDS"))
            epi <- bind_rows(epi, temp$epi)
            drop <- bind_rows(drop, temp$drop)
            horaini <- bind_rows(horaini, temp$horaini)
            conteo <- bind_rows(conteo, temp$conteo)
            duracion <- bind_rows(duracion, temp$duracion)
            maximos <- bind_rows(maximos, temp$maximos)
            latencia <- bind_rows(latencia, temp$latencia)
            CausaEfecto <- bind_rows(CausaEfecto, temp$CausaEfecto)
            par24horas <- bind_rows(par24horas, temp$par24horas)
        }

        # Crear el Excel
        cat("\nExportando Excel...\n")
        excel <- createWorkbook()
        hojas <- c("epi", "drop", "horaini", "conteo", "duracion", "maximos", "latencia", "CausaEfecto", "par24horas")
        for (xls in hojas){
            addWorksheet(excel, xls)
            eval(parse(text = paste0("writeData(excel, '", xls, "', ", xls, ")")))
            eval(parse(text = paste0("freezePane(excel, '", xls, "', firstRow = TRUE)")))
            eval(parse(text = paste0("c <- ncol(", xls, ")")))
            eval(parse(text = paste0("setColWidths(excel, '", xls, "', cols = 1:", c, ", widths = 'auto')")))
        }
        saveWorkbook(excel, "EstadisticasGlobales.xlsx", overwrite=TRUE)
        
        removeModal()
    })
    
    
    
    
    # | ---- Actograma ------------------------------------------------------------
    output$finActograma <- renderImage({
        list(src=paste0(awdfin(), ".actogram.png"))
    }, deleteFile = FALSE)
    
    
    # | ---- resto de tablas ------------------------------------------------------
    options(width = 150)
    
    # Tabla de actividad
    output$actTab <- renderPrint({
        readRDS(paste0(awdfin(), ".acvedit.RDS"))
    })
    
    # Tabla epi
    output$epiTab <- renderPrint({
        readRDS(paste0(awdfin(), ".stats.RDS"))$epi
    })
    
    # stats
    output$statTab <- renderPrint({
        temp <- readRDS(paste0(awdfin(), ".stats.RDS"))
        temp$epi <- NULL
        temp
    })
    
    
}















