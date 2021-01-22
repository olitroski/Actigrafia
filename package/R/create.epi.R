#' @title Crea archivo de episodios EPI
#' @description Este es el archivo master que usa todo para crear los episodios con los cuales se calculan las estadisticas.
#' @param acvedit el objeto que sale de check.acvfilter
#' @param filter es el archivo de filtro guardado RDS
#' @param set es la lista de settings del archivo sleep.config
#' @param dia0 logico que indica si se conserva el dia 0 o no en el epi final
#' @return data frame
#' @export
#' @examples
#' # setwd("D:/OneDrive/INTA/Actigrafia/testfolder")
#' # set <- getset(getwd())
#' # awdfile <- "2058-001-368 JRG Baseline"
#' # acvedit <- check.acvfilter(awdfile, set)
#' # filter <- readRDS(paste0(awdfile, ".edit.RDS"))
#' @importFrom dplyr rename
#' @importFrom lubridate hours
#' @importFrom lubridate hour
#' @importFrom lubridate minutes
#' @importFrom lubridate hm
#' @importFrom lubridate hms
#' @importFrom dplyr summarize
#' @importFrom lubridate as_date
#' @importFrom lubridate as.period
#' @importFrom stringr str_extract
#' @importFrom lubridate wday
#' @importFrom lubridate second


# |---------------------------------------------------------------------------------| #
# |---- Create EPI - Deteccion los episodios de sueno y vigilia + noche o dia ------| #
# |---- codigo actualizado de la version 12.19. Oliver Rojas. 03.2020 --------------| #
# |---------------------------------------------------------------------------------| #
#
# La funcion de alimenta de acveditRDS() y filterRDS() que son los reactivos cargados
# en todo momento.
#
# acveditRDS se carga desde <<return(check.acvfilter(awdfile()))>>
# y el filtro se carga directo con <<return(readRDS(fichero))>>

create.epi <- function(acvedit = NULL, filter = NULL, set = NULL, dia0 = TRUE){
    # browser()
    # Nulos para libreria
    data <- dinofinal <- indx <- name <- estado <- periodo <- hrdec <- nper <- NULL
    nseq <- duracion <- dianoc <- ini <- stage <- st.edit <- act.edit <- ini.time <- NULL
    
    actividad <- dianoc2 <- fin.time <- horaAbs <- horaDec <- NULL
    max.index <- meanActividad <- min.index <- perDN <- perND <- NULL
    periodoDN <- periodoND <- periodoSeq <- seqDianoc <- seqPer  <- NULL
    seqStage <- temp <- time <- tipo <- weekday <- NULL
    
    periodo2 <- dur_min <- mean_act_min <- num_epi <- epi_estado <- NULL
    seqND <- seqDN <- NULL
    

    # | - 1. Crear dia noche y juntar periodos ----------------------------------
    # Los filtros
    filtro <- filter$filter
    
    # Crear el tag Dia|Noche en los semiperiodos, para asegurar que existan y
    # no secundarios al estado de sueno como en actividorm
    i <- as.numeric(set$inidia)/3600
    f <- as.numeric(set$ininoc)/3600
    acvdata <- lapply(acvedit$semiper, mutate,
                      dianoc = ifelse(hrdec >= i & hrdec < f, "Dia", "Noche"))
    rm(i, f)

    # Los periodos en secuencia son los mismos nombres de la lista por lo cual basta
    # pasar los nombres al combinar.
    acvdata <- bind_rows(acvdata, .id = "nper")
    acvdata <- mutate(acvdata, nper = str_replace(nper, "per", ""))

    
    # | - 2. Data mangement ---------------------------------------------------
    # Hacer los episodios S|W correlativos 
    wsegm <- find.segment(acvdata, "st.edit", filtro = "W")
    wsegm <- mutate(wsegm, stage = "W", nseq = seq_along(ini),
                    nseq = ifelse(nseq < 10, paste0("0", nseq), nseq),
                    nstage = paste(stage, nseq, sep = "-")) %>% select(-nseq, -stage)

    ssegm <- find.segment(acvdata, "st.edit", filtro = "S")
    ssegm <- mutate(ssegm, stage = "S", nseq = seq_along(ini),
                    nseq = ifelse(nseq < 10, paste0("0", nseq), nseq),
                    nstage = paste(stage, nseq, sep = "-")) %>% select(-nseq, -stage)

    # Los indices de los episodios
    swIndex <- bind_rows(wsegm, ssegm)
    swIndex <- arrange(swIndex, ini)
    
    # Agregar la duracion de los episodios
    acvdata$seqStage <- NA
    acvdata$duracion <- NA
    for (i in 1:nrow(swIndex)){
        # El sw secuencial
        acvdata[["seqStage"]][swIndex$ini[i]:swIndex$fin[i]] <- swIndex$nstage[i]
        
        # La duracion (depende de las horas y no de las filas del swIndex)
        dur <- as.period(acvdata$time[swIndex$fin[i]] - acvdata$time[swIndex$ini[i]])
        dur <- dur + minutes(1)   # porque el fin debiera ser el ini del periodo siguiente
        dur <- hour(dur)*60 + minute(dur) + second(dur)/60
        acvdata[["duracion"]][swIndex$ini[i]:swIndex$fin[i]] <- dur
    }
    rm(swIndex, i, dur, wsegm, ssegm)


    # | - 3. Tabla de episodios -----------------------------------------------
    # Hacer una tabla de episodios para no operar sobre la base de detecciones
    # debiera ser mas rapido asi

    # ---- Tabla de episodios
    tabepi <- dplyr::group_by(acvdata, seqStage)
    tabepi <- dplyr::summarize(tabepi, 
                        duracion = unique(duracion),
                        min.index = min(indx), max.index = max(indx),
                        ini.time = min(time), 
                        filtro = paste(unique(filter), collapse = ","),
                        estado = paste(unique(st.edit), collapse = ","),
                        actividad = sum(act.edit, na.rm = TRUE))
    tabepi <- arrange(tabepi, ini.time)
    tabepi <- mutate(tabepi, fin.time = lead(ini.time))
    
    # Variable tiempo final
    tabepi[["fin.time"]][nrow(tabepi)] <- tabepi[["ini.time"]][nrow(tabepi)] + minutes(tabepi[["duracion"]][nrow(tabepi)])
    tabepi <- as.data.frame(tabepi)     # muahaha... ahora no salen errores fucking tibbles
    
    # Dia o noche traido desde acvdata
    tabepi[["dianoc"]] <- NA
    for (i in 1:nrow(tabepi)){
        index <- tabepi[["min.index"]][i]
        tabepi[["dianoc"]][i] <- acvdata[["dianoc"]][index]
    }
    
    # Limpiar y ordenar
    tabepi <- select(tabepi, 
                     min.index, max.index, ini.time, fin.time,
                     seqStage, estado, duracion, dianoc, filtro, actividad)
    rm(i, index)
    

    # | - 4. Dia|Noche segun settings -----------------------------------------
    # Loopear cada linea para decidir noche o dia segun los settings 
    tabepi$dianoc2 <- NA
    stage.i <- tabepi$dianoc[1]
    ininoc <- hour(set$ininoc) + minute(set$ininoc)/60
    inidia <- hour(set$inidia) + minute(set$inidia)/60

    for (i in 1:nrow(tabepi)){      # i <- 1
        # Evaluar si cambia de "Dia a Noche"
        if (stage.i == "Dia"){
            # Para que cambie a noche la hora debe estar entre "ininoc" y "inidia"
            hr <- tabepi$ini.time[i]
            hr <- hour(hr) + minute(hr)/60
            
            if (hr >= ininoc & hr < 23.9999){
                hora <- TRUE
            } else if (hr >= 0 & hr < inidia) {
                hora <- TRUE
            } else {
                hora <- FALSE
            }
            
            # Pruebas logicas
            if (tabepi[["duracion"]][i] >= set$dursleep & hora == TRUE & tabepi$estado[i] == "S"){
                tabepi$dianoc2[i] <- "Noche"
                stage.i <- "Noche"
            } else {
                tabepi$dianoc2[i] <- "Dia"
            }
        
        # Evaluar cambio de "Noche a Dia"
        } else if (stage.i == "Noche"){
            # Para que cambie a "Dia" la hora debe estar entre "inidia" y "ininoc"
            hr <- tabepi$ini.time[i]
            hr <- hour(hr) + minute(hr)/60
            
            if (hr >= inidia & hr < ininoc){
                hora <- TRUE
            } else {
                hora <- FALSE
            }
            
            # Buscar sueno hasta que cambie
            if (tabepi[["duracion"]][i] >= set$durawake & hora == TRUE & tabepi$estado[i] == "W"){
                tabepi$dianoc2[i] <- "Dia"
                stage.i <- "Dia"
            } else {
                tabepi$dianoc2[i] <- "Noche"
            }
            
        # Por si hay algun error
        } else {
            stop("algo paso en dianoc2")
        }
    }

    # Limpiar
    rm(stage.i, hr, hora, i)    ###### inidia, ininoc
    tabepi <- select(tabepi, -dianoc) %>% rename(dianoc = dianoc2)
    
    
    # | - 5. Mover noche ------------------------------------------------------
    # Capturar el filtro
    moveData <- filter(filtro, tipo == "Mover") 
    moveData <- pull(moveData, ini)
    moveData <- dmy_hm(moveData)
    
    # Procesar si hubiera     i <- 1
    if (length(moveData) > 0){
        tabepi$mover <- NA
        
        for (i in length(moveData)){
            # Marcar ocurrencia y capturar stage
            i.move <- which(tabepi$ini.time == moveData[i])
            tabepi$mover[i.move] <- TRUE
            st.epi <- tabepi$estado[i.move]
            
            # ---- Inicio Episodio, Sueno ---- #
            if (st.epi == "S") {
                # Se mueve el inicio de la noche
                # 
                # | Dia
                # | Dia
                # | Noche
                # |---------+
                # | * Noche |
                # | Noche   |
                # |         v
                # 
                # Hacia atras si hay algun "Noche" lo pasa a "Dia", por si se movio
                i.move <- which(tabepi$ini.time == moveData[i])
                i.move <- i.move - 1
                change <- TRUE
                while (change == TRUE){
                    if (tabepi[["dianoc"]][i.move] == "Noche"){
                        tabepi[["dianoc"]][i.move] <- "Dia"
                        i.move <- i.move - 1
                    } else {
                        change <- FALSE
                    } 
                } # Hacia delante debiera ser todo "Noche"

            # ----Inicio Episodio, Vigilia ---- #
            } else if (st.epi == "W"){
                # Se mueve inicio del dia.
                # 
                # | Noche
                # | Noche
                # | Dia
                # |---------+
                # | * Dia   |
                # | Dia     |
                # |         v
                # 
                # Hacia atras si hay algun "Dia" lo pasa a "Noche"
                i.move <- which(tabepi$ini.time == moveData[i])
                i.move <- i.move - 1
                change <- TRUE
                while (change == TRUE){
                    if (tabepi[["dianoc"]][i.move] == "Dia"){
                        tabepi[["dianoc"]][i.move] <- "Noche"
                        i.move <- i.move - 1
                    } else {
                        change <- FALSE
                    } 
                } # Hacia adelante debiera estar todo ok "Dia"
            }
        }
        
        rm(i, i.move, st.epi)
    }
    
    # Limpiar
    rm(moveData)
    
    
    # | - 6. Secuenciar Dia|noche + drop --------------------------------------
    # Marcar los extremos
    tabepi <- mutate(tabepi, seqDianoc = ifelse(filtro == "Ini", "Drop",
                                                ifelse(filtro == "Fin", "Drop",
                                                       ifelse(filtro == "Excluir", "Drop", NA))))
    
    # Determinar desde donde partir
    start <- which(is.na(tabepi$seqDianoc))
    start <- min(start)
    
    # Numero secuencial inicial
    if (tabepi$dianoc[start] == "Dia"){
        seqnum <- 0
        tabepi$seqDianoc[start] <- seqnum
        start <- start + 1
    } else {
        seqnum <- 1
        tabepi$seqDianoc[start] <- seqnum
        start <- start + 1
    }

    # Loopear cada linea - Solo cambia "seqnum"
    for (i in start:nrow(tabepi)){
        # Valores de varaible
        valLine <- tabepi$seqDianoc[i]
        valPrev <- tabepi$seqDianoc[i - 1]
        
        # Valores de dia|noche
        valLineDN <- tabepi$dianoc[i]
        valPrevDN <- tabepi$dianoc[i - 1]
        
        # Si el actual es "drop" o sea notNA aumenta el i
        if (is.na(valLine) == FALSE){
            tabepi$seqDianoc[i] <- "Drop"
            i <- i + 1
            
        # Si hay valor
        } else if (is.na(valLine) == TRUE){
            # Cuando se conserva porque el anterior es igual
            if (valLineDN == valPrevDN & is.na(valLine) == TRUE & valPrev == as.character(seqnum)){
                tabepi$seqDianoc[i] <- as.character(seqnum)
                i <- i + 1
                
            # Se viene de un drop, se aumenta el "seqnum"
            } else if (valPrev == "Drop"){
                seqnum <- seqnum + 1
                tabepi$seqDianoc[i] <- as.character(seqnum)
                i <- i + 1
                
            # Cambio de dianoc y no hay "drops"
            } else if (valLineDN != valPrevDN & is.na(valLine) == TRUE & valPrev == as.character(seqnum)){
                seqnum <- seqnum + 1
                tabepi$seqDianoc[i] <- as.character(seqnum)
                i <- i + 1
                
            # Algun error    
            } else {
                stop("algo pasa #1 al asignar el secuencial")
            }
            
        # Algo pasa
        } else {
            stop("algo pasa #2 al asignar el secuencial")
        }
    }
    
    # la variable secuencial
    options(warn = -1)
    tabepi <- mutate(tabepi, temp = ifelse(seqDianoc == "Drop", "Drop",
                                           ifelse(as.numeric(seqDianoc) < 10, paste0("0", seqDianoc),
                                                  seqDianoc)),
                             temp = ifelse(seqDianoc == "Drop", "Drop",
                                           paste(dianoc, temp)))
    options(warn = 0)

    # Limpiar
    rm(i, seqnum, start, valLine, valLineDN, valPrev, valPrevDN)    
    
    
    # | - 7. Crar periodos Dia-Noche (DN) y Noche-Dia (ND) --------------------
    # Con una tabla auxiliar mejor
    periodo <- dplyr::group_by(filter(tabepi, temp != "Drop"), temp)
    periodo <- dplyr::summarize(periodo, ini = min(ini.time), fin = max(fin.time))
    periodo <- arrange(periodo, ini)
    periodo <- mutate(periodo, dianoc = str_split(periodo$temp, " ", simplify = TRUE)[,1])
    periodo <- mutate(periodo, num = str_split(periodo$temp, " ", simplify = TRUE)[,2])
    periodo <- as.data.frame(periodo)
    
    # Establecer el inicio (pa partir del 2)
    #       Son hartas combinaciones posibles        #
    # ---------------------------------------------- #
    # ---- Para el segundo periodo ----
    # 1 Anterior Dia 0 y Actual Noche, X+1
    # 2 Anterior Dia 0 y Actual Dia, X+1          Estos 2 son 1, pero queda muy largo 
    
    # 3 Anterior Noche 1 y Actual Dia y Consecutivo, X     >>>>
    # 4 Anterior Noche 1 y Actual Noche, X+1
    
    # ---- En adelante ----
    # 5 Anterior Dia y Actual Noche, X+1
    # 6 Anterior Dia y Actual Dia, X+1
    # 7 Anterior Noche y Actual Dia y Consecutivo, X       >>>>>
    # 8 Anterior Noche y Actual Dia y No-Consecutivo, X+1
    # 9 Anterior Noche y Actual Noche, X+1
    
    periodo$seqND <- NA
    if (periodo$dianoc[1] == "Dia"){
        periodo$seqND[1] <- 0
        X <- 0
    } else {
        periodo$seqND[1] <- 1
        X <- 1
    }
    
    # No voy a hacer todas las combis, solo las X
    for (i in 2:nrow(periodo)){
        anterior  <- periodo$dianoc[i - 1]
        seqval <- periodo$seqND[i -1]
        actual    <- periodo$dianoc[i]
        consecutivo <- periodo$fin[i-1] == periodo$ini[i]
        
        # 2
        if (anterior == "Noche" & seqval == 1 & actual == "Dia" & consecutivo == TRUE){   # no es necesario este
            periodo$seqND[i] <- X
            
        # 7
        } else if (anterior == "Noche" & actual == "Dia" & consecutivo == TRUE){
            periodo$seqND[i] <- X
            
        # Resto
        } else {
            X <- X + 1
            periodo$seqND[i] <- X
        }
    }
    
    
    # Las otras combis de DIA -> NOCHE
    # ---- Para el segundo periodo ----
    # 1 Anterior Dia 0 y Actual Noche y Consecutivo, X     >>>>
    # 2 Anterior Dia 0 y Actual Dia, X+1          
    
    # 3 Anterior Noche 1 y Actual Dia, X+1
    # 4 Anterior Noche 1 y Actual Noche, X+1
    
    # ---- En adelante ----
    # 5 Anterior Dia y Actual Noche y consecutivo, X       >>>>
    # 6 Anterior Dia y Actual Dia, X+1
    # 7 Anterior Noche y Actual Dia y Consecutivo, X+1
    # 8 Anterior Noche y Actual Dia y No-Consecutivo, X+1
    # 9 Anterior Noche y Actual Noche, X+1
    
    periodo$seqDN <- NA
    if (periodo$dianoc[1] == "Dia"){
        periodo$seqDN[1] <- 0
        X <- 0
    } else {
        periodo$seqDN[1] <- 1
        X <- 1
    }
    
    for (i in 2:nrow(periodo)){
        anterior  <- periodo$dianoc[i - 1]
        seqval <- periodo$seqDN[i -1]
        actual    <- periodo$dianoc[i]
        consecutivo <- periodo$fin[i-1] == periodo$ini[i]
        
        # 1
        if (anterior == "Dia" & seqval == 0 & actual == "Noche" & consecutivo == TRUE){
            periodo$seqDN[i] <- X
            
        # 5
        } else if (anterior == "Dia" & actual == "Noche" & consecutivo == TRUE){
            periodo$seqDN[i] <- X
            
        # Resto    
        } else {
            X <- X + 1
            periodo$seqDN[i] <- X
        }
    }

    # Hermosear
    periodo <- mutate(periodo,
                      perND = ifelse(seqND < 10, paste0("0", seqND), seqND),
                      perND = paste(dianoc, perND),
                      perDN = ifelse(seqDN < 10, paste0("0", seqDN), seqDN),
                      perDN = paste(dianoc, perDN))
    periodo <- select(periodo, temp, perND, perDN)

    
    # | - 8. Juntar todo ------------------------------------------------------
    tabepi <- base::merge(tabepi, periodo, by = "temp", all = TRUE)
    tabepi <- arrange(tabepi, ini.time)
    
    # Mover los drop a las variables nuevas
    tabepi <- rename(tabepi, periodoDN = perDN, periodoND = perND, periodoSeq = temp)
    tabepi <- mutate(tabepi, periodoDN = ifelse(seqDianoc == "Drop", "Drop", periodoDN),
                             periodoND = ifelse(seqDianoc == "Drop", "Drop", periodoND))
    tabepi <- select(tabepi, -seqDianoc, -min.index, -max.index)

    # Pegarle al "acvedata" algunas cosas
    acvdata <- merge(acvdata, select(tabepi, seqStage, periodoSeq, periodoND, periodoDN), by = "seqStage")
    acvdata <- select(acvdata, -nper)
    acvdata <- arrange(acvdata, time)

    
    # ----- EPI Viejo -------------------------------------------------------------------------------------
    # Crear data.frame para pasar a las stats que asemeja el epi viejo
    epiviejo <- tabepi
    epiviejo <- filter(epiviejo, periodoSeq != "Drop")
    
    # Dejar solo el periodo Noche -> Dia
    epiviejo <- select(epiviejo, -periodoSeq, -periodoDN)
    epiviejo <- rename(epiviejo, periodo = periodoND)
    
    # Recomponer version vieja del periodo que comienza en "Dia 01"
    temp <- str_split(epiviejo$periodo, " ", simplify = TRUE)
    temp <- as.data.frame(temp, stringsAsFactors = FALSE)
    names(temp) <- c("dianoc", "nper")
    
    # temp <- mutate(temp, 
    #                nper = as.numeric(nper) + 1,
    #                nper = ifelse(nper < 10, paste0("0", nper), as.character(nper)),
    #                periodo = paste(dianoc, nper))
    # epiviejo$periodo2 <- temp$periodo
    # epiviejo <- select(epiviejo, -periodo) %>% rename(periodo = periodo2)
    
    # Recomponer el epi_estado (solo nombre, ya se vera si es nacesario)
    epiviejo <- rename(epiviejo, epi_estado = seqStage, hora = ini.time, dur_min = duracion)
    epiviejo <- arrange(epiviejo, hora) %>% mutate(num_epi = 1:nrow(epiviejo))
    
    # Crear ultimas variables
    epiviejo <- mutate(epiviejo, 
                       mean_act_min = round(actividad/dur_min, 1), 
                       id = acvdata$filename[1], 
                       actividad = round(actividad, 0))
    epiviejo <- select(epiviejo, id, periodo, hora, estado, actividad, dur_min, mean_act_min, num_epi, epi_estado)
    
    
    # Botar el dia cero, igual se deja la opcion por si las moscas
    if (dia0 == FALSE){
        epiviejo = filter(epiviejo, periodo != "Dia 00")
    }
    
    # Combinar todo en una lista
    epi <- list(acvdata = acvdata, epitab = tabepi, epiviejo = epiviejo)
    return(epi)
}
# final -----



