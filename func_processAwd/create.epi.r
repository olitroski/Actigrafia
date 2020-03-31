# |---------------------------------------------------------------------------------| #
# |---- Create EPI - Detección los episodios de sueño y vigilia + noche o dia ------| #
# |---- codigo actualizado de la versión 12.19. Oliver Rojas. 03.2020 --------------| #
# |---------------------------------------------------------------------------------| #
# @semiper: Lista con periodos que proviene de la función "check.acvfilter(awdfile)"
#  
# test <- create.epi(semiper)
create.epi <- function(AcvFilterRds){
    # Data y verctorización función para secuencias
    vectSeq <- Vectorize(seq.default, vectorize.args = c("from", "to"))
    semiper <- AcvFilterRds$semiper
    
    # | - 1. dianoc previo ----
    # Antes que todo, crear el dianoc en los semiperiodos, para asegurar que existan y
    # no como secundarios al estado de sueño
    i <- as.numeric(set$inidia)/3600
    f <- as.numeric(set$ininoc)/3600
    semiper <- lapply(semiper, mutate, dianoc = ifelse(hrdec >= i & hrdec < f, 
                                                       "Dia", "Noche"))
    
    
    # | - 2. Numerar periodos ----
    # Depende del inicio del registro, usualmente parte en el dia antes de "ininoc" 
    # asi el primer dia no sirve y queda "Dia 00", pero si parte antes de "inidia" y 
    # después de "ininoc quedaría el 1er periodo = "Noche 00". Y sus correlativos.
    perIndx <- 1:length(semiper) - 1
    perIndx <- ifelse(perIndx < 10, paste0("0", perIndx), as.character(perIndx))
    for (i in 1:length(semiper)){
        semiper[[i]]["perIndx"] <- perIndx[i]
    }
        
    head(semiper[[1]])
        
    data <- bind_rows(semiper) 
    rm(i, f, semiper, perIndx)
    
    
    # | ---- Data Vigilia ----
    # Segementos e indices
    segm <- find.segment(data, "st.edit", filtro = "W")
    ranges <- vectSeq(from = segm$ini, to = segm$fin, by = 1)
    # Stats
    ini     <- lapply(ranges, function(x) min(data$time[x]))
    fin     <- lapply(ranges, function(x) max(data$time[x]))
    meanAct <- lapply(ranges, function(x) mean(data$act.edit[x]))
    filtro  <- lapply(ranges, function(x) unique(data$filter[x]))
    # Reordenar la data
    dataWake <- data.frame(i = 1:length(ranges), 
                           ini = paste(ini, sep = ","), 
                           fin = paste(fin, sep = ","),
                           meanAct = paste(meanAct, sep = ";"), 
                           filtro = paste(filtro, sep = ","), stringsAsFactors = FALSE)
    dataWake$i <- paste(ifelse(dataWake$i < 10, 
                               paste0("W-0", dataWake$i), 
                               paste0("W-", dataWake$i)))
    rm(segm, ranges, ini, fin, meanAct, filtro)
    
    
    # | ---- Data Sueño ----
    # Indices y segmentos
    segm <- find.segment(data, "st.edit", filtro = "S")
    ranges <- vectSeq(from = segm$ini, to = segm$fin, by = 1)
    # Stats
    ini     <- lapply(ranges, function(x) min(data$time[x]))
    fin     <- lapply(ranges, function(x) max(data$time[x]))
    meanAct <- lapply(ranges, function(x) mean(data$act.edit[x]))
    filtro  <- lapply(ranges, function(x) unique(data$filter[x]))
    # Reordenar la data
    dataSleep <- data.frame(i = 1:length(ranges), 
                            ini = paste(ini, sep = ","), 
                            fin = paste(fin, sep = ","),
                            meanAct = paste(meanAct, sep = ";"), 
                            filtro = paste(filtro, sep = ","), stringsAsFactors = FALSE)
    dataSleep$i <-paste(ifelse(dataSleep$i < 10, 
                               paste0("S-0", dataSleep$i), 
                               paste0("S-", dataSleep$i)))
    rm(segm, ranges, ini, fin, meanAct, filtro)

    # | ---- Data Combinada S|W----
    epi <- bind_rows(dataWake, dataSleep)
    rm(dataWake, dataSleep)
    
    # Data management 
    epi <- mutate(epi, 
                  ini = as_datetime(as.numeric(ini)),
                  fin = as_datetime(as.numeric(fin)),
                  dur = as.numeric(fin - ini),
                  name = data[1, "nombre"],
                  file = data[1, "filename"],
                  meanAct = round(as.numeric(meanAct), 2),
                  estado = str_sub(i, 1, 1)) 
    epi <- rename(epi, duracion = dur) %>% arrange(ini)
    epi <- mutate(epi, indx = 1:nrow(epi))
    rm(data)

    
    # Funcion pa sacar el tiempo en formato intervalos de lubridate
    hr <- function(time) {hours(hour(time)) + minutes(minute(time))}

    # Trozo 1: DINO by setting ----
    # Determinar dia o noche según las horas de setting (ininoc e inidia)
    # y marca 1 si duracion <> durawake
    epi <- mutate(epi, 
                  # Variable dino = dia|noche segun los valores del setting
                  dino = ifelse(hr(ini) >= hm("00:00") & hr(ini) < set$inidia, 
                                "Noche",
                                ifelse(hr(ini) >= set$inidia & hr(ini) < set$ininoc, 
                                       "Dia",
                                       ifelse(hr(ini) >= set$ininoc & hr(ini) < hms("23:59:59"), 
                                              "Noche", "Error"))),
                  # Variable dur = 1 si el episodio dura más que durawake o sleep segun settings
                  dur = ifelse(dino == "Dia" & duracion >= set$durawake, 
                               1,
                               ifelse(dino == "Noche" & duracion >= set$dursleep, 
                                      1, 0)))

    
    # Trozo 2: DINO Correlativo ----    
    # Index para c/ dia|noche para contruir dia01 dia02 etc.
    dia <- find.segment(epi, "dino", "Dia")
    dia <- mutate(dia, n = 1:nrow(dia), dino = "Dia", fin = fin + 1)
    noc <- find.segment(epi, "dino", "Noche")
    noc <- mutate(noc, n = 1:nrow(noc), dino = "Noche", fin = fin + 1)
    dnIndx <- bind_rows(dia, noc) %>% arrange(ini)
    dnIndx$fin[nrow(dnIndx)] <- nrow(epi)
    rm(dia, noc)

    # Correlativo de dianoc construido onda "Dia 01"
    for (i in 1:nrow(dnIndx)){
        epi[dnIndx$ini[i]:dnIndx$fin[i], "dnIndx"] <- dnIndx$n[i]
    }
    epi <- mutate(epi, dnIndx = ifelse(dnIndx < 10, 
                                      paste("0", dnIndx, sep = ""),
                                      dnIndx), 
                       dianoc = paste(dino, dnIndx))
    rm(i, dnIndx)
    
    
    # Trozo 3: Existe dursleep|durawake ----
    # Evaluar si existe (dia|noche) > (dursleep|durwake) si no hay pasa de largo la detección
    maxdur <- group_by(epi, dianoc)
    maxdur <- summarize(maxdur, maxdur = max(dur), indx = min(dnIndx), dino = unique(dino)) 
    maxdur <- filter(maxdur, maxdur == 0)     # Estos no tienen la dur minima para determinar
    
    # Esto es en caso de encontrar un dianoc sin dur sleep|wake suficiente.
    # Crea variable="dur2": la idea es que no queden "Dia" etiquetados como "Noche" porque
    # no se encontró un episodio que durara más de sleep|wake dur.
    epi$dur2 <- 0
    if (nrow(maxdur) > 0){
        for (i in 1:nrow(maxdur)){
            # Para el dia
            if (maxdur$dino[i] == "Dia"){
                ifix <- min(epi[epi$dianoc == maxdur$dianoc[i] & epi$estado == "W", "indx"])
                epi$dur2[ifix] <- 1    # Acá se pone el inicio falso after inidia
            
            # para la noche
            } else if (maxdur$dino[i] == "Noche"){
                ifix <- min(epi[epi$dianoc == maxdur$dianoc[i] & epi$estado == "S", "indx"])
                epi$dur2[ifix] <- 1    # Acá se pone el inicio falso after inidia
                
            } else {
                stop("algo paso, dianoc sin stage > dursleep|durwake")
            }
        }
    }
    rm(maxdur, ifix)
    
    ######### temporal ############# ------
    epi <- mutate(epi, fin = as_date(ini))
    View(epi)
    
    # | --- set -----------
    # Ya tenemos todos los dia noc con stage marcado con comienzo no importa si no tiene stage > 30    
    # Ahora detectar el 1° stage mayor a durawake y dursleep
    epi$dur3 <- 0
    st <- epi$dino[1]
    i <- 1
    while (i < nrow(epi)){
        dino  <- epi$dino[i]
        dur  <- epi$dur[i]
        dur2 <- epi$dur2[i]
        stage <- epi$estado[i]
    
        
        # Si la linea dura más de 30 mins
        if ((dino == st & dur == 1) | (dino == st & dur2 == 1)){
            # El primer dia queda mal puesto porque siempre parte despues de del "inidia", se arregla después.
    
            # Como ya encontramos i > durWake|durSleep, cambia valor "st" para que en la siguiente iteración
            # solo avance, en algun momento cambiara el dino original y volverá a hacer match
            # y seguirá buscando hasta que encuentre una linea que tenga la duración adecuada, volverá 
            # a cambiar el st y repita el proceso
            
            # Si la linea es "Dia" el > que "durDia" debe ser "W"
            if (dino == "Dia" & stage == "W"){
                epi$dur3[i] <- 1
                if (st == "Dia"){
                    st <- "Noche"
                } else if (st == "Noche"){
                    st <- "Dia"
                }
                i <- i + 1
                
            # Si la linea es "noche" el > que "durSleep debe ser "S"
            } else if (dino == "Noche" & stage == "S"){
                epi$dur3[i] <- 1
                if (st == "Dia"){
                    st <- "Noche"
                } else if (st == "Noche"){
                    st <- "Dia"
                }
                i <- i + 1
                
            # Si no... avanzamos 
            } else {
                i <- i + 1
            }
        
        # Si encontramos que la linea tiene duración < durSW pasa y anota cero
        } else {
            # Como "dino!=st" o "dur1|dur2 != 1" asigna valor 0 y avanza
            epi$dur3[i] <- 0
            i <- i + 1
        }
    }
    
    
    
    
    # Borrar lo que ya no sirve
    epi <- select(epi, -dur, -dur2, -dianoc)
    
    
    
    
    
    
    
    
    
    
    # Fabricar el dianoc final, ya se hicieron las correcciones y todo -----
    # Indices el correlativo de dia noc
    dia <- find.segment(epi, "dino", "Dia")
    dia <- mutate(dia, n = 1:nrow(dia), dino = "Dia", fin = fin + 1)
    noc <- find.segment(epi, "dino", "Noche")
    noc <- mutate(noc, n = 1:nrow(noc), dino = "Noche", fin = fin + 1)
    dnIndx <- bind_rows(dia, noc); rm(dia, noc)
    dnIndx <- select(dnIndx, dino, n, ini, fin) %>% arrange(ini)
    dnIndx$fin[nrow(dnIndx)] <- nrow(epi)
    
    # Correlativo de dianoc construido onda "Dia 01"
    for (i in 1:nrow(dnIndx)){
        epi[dnIndx$ini[i]:dnIndx$fin[i], "dnIndx"] <- dnIndx$n[i]
    }
    rm(dnIndx)
    
    
    
    
    # Corregir el epi$dino porque puede estar descuadrado -----
    epi$dinofinal <- NA
    i <- min(which(epi$dur3 == 1))
    epi$dinofinal[i] <- epi$dino[i]     # El primer episodio
    
    i <- i + 1
    st <- epi$dino[i]
    while (i <= nrow(epi)){
        dino  <- epi$dino[i]
        dur3  <- epi$dur3[i]
        
        # Hace el cambio si es diferente al encontrar dur3=1
        if (dino != st & dur3 == 1){
            if (st == "Dia"){
                st <- "Noche"
            } else if (st == "Noche"){
                st <- "Dia"
            }
            # Asigna el valor
            epi$dinofinal[i] <- st
            i <- i + 1
        } else {
            # Continua hasta que encuentre dur3 == 1 con el mismo st
            epi$dinofinal[i] <- st
            i <- i + 1
        }
    }
    
    
    
    # Resta 1 a al index del periodo (dia 1) de dia para que empiece del 0 cuando parte de dia
    # solo se le hace al día
    epi <- mutate(epi, dnIndx = as.numeric(dnIndx),
                  dnIndx = ifelse(dinofinal == "Dia", dnIndx - 1, dnIndx))
    
    # Hace el dianocfinal
    epi <- mutate(epi, dnIndx = ifelse(dnIndx < 10, 
                                           paste0("0", as.character(dnIndx)),
                                           as.character(dnIndx)))
    epi <- mutate(epi, periodo = paste(dinofinal, dnIndx)) %>% rename(dianoc = dinofinal)
    
    # Limpiar y entregar
    epi <- filter(epi, is.na(dianoc) == FALSE) 
    epi <- select(epi, indx, i, name, ini, fin, meanAct, estado, duracion, dianoc, periodo, filtro, file)
    return(epi)
}

# library(tictoc)
# tic(); test <- create.epi(semiper); toc()