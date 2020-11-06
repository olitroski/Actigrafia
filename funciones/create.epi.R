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
    # Nulos para libreria
    data <- dinofinal <- indx <- name <- estado <- periodo <- hrdec <- nper <- NULL
    nseq <- duracion <- dianoc <- ini <- stage <- st.edit <- act.edit <- ini.time <- NULL
    
    actividad <- dianoc2 <- fin.time <- horaAbs <- horaDec <- NULL
    max.index <- meanActividad <- min.index <- perDN <- perND <- NULL
    periodoDN <- periodoND <- periodoSeq <- seqDianoc <- seqPer  <- NULL
    seqStage <- temp <- time <- tipo <- weekday <- NULL
    
    periodo2 <- dur_min <- mean_act_min <- num_epi <- epi_estado <- NULL
    
    # Los filtros
    filtro <- filter$filter

    # | - 1. Crear dia noche y juntar periodos ----------------------------------
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
    rm(wsegm, ssegm)

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
    rm(swIndex, i, dur)


    # | - 3. Tabla de episodios -----------------------------------------------
    # Hacer una tabla de episodios para no operar sobre la base de detecciones
    # debiera ser mas rapido asi
    # browser()
    # rehacer el index
    acvdata <- mutate(acvdata, indx = indx - 1)

    # ---- Tabla de episodios
    tabepi <- group_by(acvdata, seqStage)
    tabepi <- summarize(tabepi, 
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
    
    
    # Variables adicionales a la tabla de episodios
    # dia o noche del inicio del episodio
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
        
        # Si el actual es "drop" y aumenta el i
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
    tabepi <- mutate(tabepi, 
                     temp = ifelse(seqDianoc == "Drop", as.character(666), seqDianoc), 
                     temp = as.numeric(temp),
                     temp = ifelse(temp == 666, "Drop", ifelse(temp < 10, paste0("0", temp), as.character("temp"))),
                     temp = ifelse(temp == "Drop", "Drop", paste(dianoc, temp)))

    # Limpiar
    rm(i, seqnum, start, valLine, valLineDN, valPrev, valPrevDN)    
    
    
    # | - 7. Crar periodos Dia-Noche (DN) y Noche-Dia (ND) --------------------
    # Con una tabla auxiliar mejor
    periodo <- group_by(filter(tabepi, temp != "Drop"), temp)
    periodo <- summarize(periodo, ini = min(ini.time), fin = max(fin.time))
    periodo <- arrange(periodo, ini)
    periodo <- mutate(periodo, dianoc = str_split(periodo$temp, " ", simplify = TRUE)[,1])
    
    # Marcar consecutivos "Dia-Noche"
    if (periodo$dianoc[1] == "Dia"){
        seqnum <- 0
    } else {
        seqnum <- 1
    }

    periodo$perDN <- NA
    change <- TRUE
    i <- 1
    while (change == TRUE){
        if (is.na(periodo$dianoc[i + 1] == "Noche") == TRUE){     # Para la ultima linea
            periodo$perDN[i] = seqnum
            i <- i + 1
        } else if (periodo$dianoc[i] == "Noche" & periodo$dianoc[i + 1] == "Dia" & periodo$fin[i] == periodo$ini[i + 1]){
            periodo$perDN[i] = seqnum
            periodo$perDN[i + 1] = seqnum
            seqnum <- seqnum + 1
            i <- i + 2
        } else {
            periodo$perDN[i] = seqnum
            seqnum <- seqnum + 1
            i <- i + 1
        }
        # Detiene el loop
        if (i > nrow(periodo)){
            change <- FALSE
        }
    }

    # Crar periodos Noche-Dia (ND), sipis, lo hago separado del DN... 
    if (periodo$dianoc[1] == "Dia"){
        seqnum <- 0
    } else {
        seqnum <- 1
    }
    
    periodo$perND <- NA
    change <- TRUE
    i <- 1
    while (change == TRUE){
        if (is.na(periodo$dianoc[i + 1] == "Noche") == TRUE){     # Para la ultima linea
            periodo$perND[i] = seqnum
            i <- i + 1
        } else if (periodo$dianoc[i] == "Dia" & periodo$dianoc[i + 1] == "Noche" & periodo$fin[i] == periodo$ini[i + 1]){
            periodo$perND[i] = seqnum
            periodo$perND[i + 1] = seqnum
            seqnum <- seqnum + 1
            i <- i + 2
        } else {
            periodo$perND[i] = seqnum
            seqnum <- seqnum + 1
            i <- i + 1
        }
        # Detiene el loop
        if (i > nrow(periodo)){
            change <- FALSE
        }
    }
    

    # Hermosear
    periodo <- as.data.frame(periodo)
    periodo <- mutate(periodo,
                      perDN = ifelse(perDN < 10, paste0("0", perDN), perDN),
                      perDN = paste(dianoc, perDN),
                      perND = ifelse(perND < 10, paste0("0", perND), perND),
                      perND = paste(dianoc, perND))
    periodo <- rename(periodo, periodoSeq = temp, periodoDN = perDN, periodoND = perND)
    periodo <- select(periodo, periodoSeq, periodoDN, periodoND)
    
    
    # Limpiar
    rm(i, change, seqnum)
    
    
    
    # | - 8. Juntar todo ------------------------------------------------------
    tabepi <- rename(tabepi, periodoSeq = temp)
    tabepi <- base::merge(tabepi, periodo, by = "periodoSeq", all = TRUE)
    
    # Mover los drop
    tabepi <- mutate(tabepi, periodoDN = ifelse(seqDianoc == "Drop", "Drop", periodoDN),
                             periodoND = ifelse(seqDianoc == "Drop", "Drop", periodoND))
    tabepi <- arrange(tabepi, ini.time)
    tabepi <- select(tabepi, -seqDianoc, -min.index, -max.index)
    
    
    # #### IMPORTANTE  ####################################################
    # <<< --- OJO --- >>> Me quedo al reves el DN y ND <<< --- OJO ---- >>>
    tabepi <- rename(tabepi, periodoND = periodoDN, periodoDN = periodoND)
    # #### IMPORTANTE  ####################################################
    
    
    # Pegarle al "acvedata" algunas cosas
    acvdata <- merge(acvdata, select(tabepi, seqStage, periodoSeq, periodoND, periodoDN), by = "seqStage")
    acvdata <- select(acvdata, -nper)

    
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
    temp <- mutate(temp, 
                   nper = as.numeric(nper) + 1,
                   nper = ifelse(nper < 10, paste0("0", nper), as.character(nper)),
                   periodo = paste(dianoc, nper))
    epiviejo$periodo2 <- temp$periodo
    epiviejo <- select(epiviejo, -periodo) %>% rename(periodo = periodo2)
    
    # Recomponer el epi_estado (solo nombre, ya ser vera si es nacesario)
    epiviejo <- rename(epiviejo, epi_estado = seqStage, hora = ini.time, dur_min = duracion)
    epiviejo <- arrange(epiviejo, hora) %>% mutate(num_epi = 1:nrow(epiviejo))
    
    # Crear ultimas variables
    epiviejo <- mutate(epiviejo, mean_act_min = round(actividad/dur_min, 1), id = acvdata$filename[1], actividad = round(actividad, 0))
    epiviejo <- select(epiviejo, id, periodo, hora, estado, actividad, dur_min, mean_act_min, num_epi, epi_estado)
    
    
    # ### Botar el dia cero, igual se deja la opcion por si las moscas ####
    if (dia0 == FALSE){epiviejo = filter(epiviejo, periodoND != "Dia 00") }
    # ################################################################### #

    # Combinar todo en una lista
    epi <- list(acvdata = acvdata, epitab = tabepi, epiviejo = epiviejo)
    return(epi)
}
# final -----



