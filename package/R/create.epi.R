#' @title Crea archivo de episodios EPI
#' @description Este es el archivo master que usa todo para crear los episodios con los cuales se calculan las estadisticas.
#' @param acvedit el objeto que sale de check.acvfilter
#' @param filter es el archivo de filtro guardado RDS
#' @return data frame
#' @export
#' @examples
#' # setwd("D:/OneDrive/INTA/Actigrafia/testfolder")
#' # awdfile <- "2058-001-368 JRG Baseline"
#' # acvedit <- check.acvfilter(awdfile)
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

create.epi <- function(acvedit = NULL, filter = NULL){
    # Nulos de paquete
    data <- dinofinal <- indx <- name <- estado <- periodo <- set <- hrdec <- nper <- NULL
    nseq <- duracion <- dianoc <- NULL


    # Vectorizacion de secuencia
    vectSeq <- Vectorize(seq.default, vectorize.args = c("from", "to"))

    # Separar los datos de filtro
    rango <- filter$header[4:5]
    rango <- str_split(rango, "a:", simplify = TRUE)[, 2]
    rango <- dmy_hm(rango)
    filtro <- filter$filter   # filtro <- filtro[-12,]

    # | - 1. dianoc previo ----
    # Crear el tag Dia|Noche en los semiperiodos, para asegurar que existan y
    # no secundarios al estado de sueno como en actividorm
    i <- as.numeric(set$inidia)/3600
    f <- as.numeric(set$ininoc)/3600
    acvdata <- lapply(acvedit$semiper, mutate,
                      dianoc = ifelse(hrdec >= i & hrdec < f, "Dia", "Noche"))
    rm(i, f)


    # | - 2. Numerar periodos ----
    # Los periodos en secuencia son los mismos nombres de la lista por lo cual basta
    # pasar los nombres al combinar.
    acvdata <- bind_rows(acvdata, .id = "nper")
    acvdata <- mutate(acvdata, nper = str_replace(nper, "per", ""))


    # --------------------------------------------------------------------------------- #
    # --- 3. Determinar dia y noche ---------------------------------------------------
    # --------------------------------------------------------------------------------- #
    # Lo mejor sera determinar bien antes que todo cuando es dia y noche asi luego
    # solo ser haria un group_by, por noche y por estado de sueno. Por capas de variables.

    # | --- duracion de cada episodio -------------------------------------------------
    # Numerar los episodios primero y su index
    wsegm <- find.segment(acvdata, "st.edit", filtro = "W")
    wsegm <- mutate(wsegm, stage = "W", nseq = seq_along(ini),
                    nseq = ifelse(nseq < 10, paste0("0", nseq), nseq),
                    nstage = paste(stage, nseq, sep = "-")) %>% select(-nseq, -stage)

    ssegm <- find.segment(acvdata, "st.edit", filtro = "S")
    ssegm <- mutate(ssegm, stage = "S", nseq = seq_along(ini),
                    nseq = ifelse(nseq < 10, paste0("0", nseq), nseq),
                    nstage = paste(stage, nseq, sep = "-")) %>% select(-nseq, -stage)

    swIndex <- bind_rows(wsegm, ssegm)
    swIndex <- arrange(swIndex, ini)
    rm(wsegm, ssegm)

    # Para cada index
    acvdata$seqStage <- NA
    acvdata$duracion <- NA
    for (i in 1:nrow(swIndex)){
        # El sw secuencial
        acvdata$seqStage[swIndex$ini[i]:swIndex$fin[i]] <- swIndex$nstage[i]

        # La duracion
        dur <- acvdata$time[swIndex$fin[i]] - acvdata$time[swIndex$ini[i]]
        dur <- as.numeric(as.period(dur))/60
        acvdata[["duracion"]][swIndex$ini[i]:swIndex$fin[i]] <- dur
    }





    # Determinar el 1er episodio de mas de x minutos
    acvdata <- mutate(acvdata, )

















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


    # | ---- Data Sueno ----
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
    # Determinar dia o noche segun las horas de setting (ininoc e inidia)
    # y marca 1 si duracion <> durawake
    epi <- mutate(epi,
                  # Variable dino = dia|noche segun los valores del setting
                  dino = ifelse(hr(ini) >= hm("00:00") & hr(ini) < set$inidia,
                                "Noche",
                                ifelse(hr(ini) >= set$inidia & hr(ini) < set$ininoc,
                                       "Dia",
                                       ifelse(hr(ini) >= set$ininoc & hr(ini) < hms("23:59:59"),
                                              "Noche", "Error"))),
                  # Variable dur = 1 si el episodio dura mas que durawake o sleep segun settings
                  dur = ifelse(dino == "Dia" & duracion >= set$durawake,
                               1,
                               ifelse(dino == "Noche" & duracion >= set$dursleep,
                                      1, 0)))


    # Trozo 2: DINO Correlativo ----
    # Index para c/ dia-noche para contruir dia01 dia02 etc.
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
    # Evaluar si existe (dia|noche) > (dursleep|durwake) si no hay pasa de largo la deteccion
    maxdur <- group_by(epi, dianoc)
    maxdur <- summarize(maxdur, maxdur = max(dur), indx = min(dnIndx), dino = unique(dino))
    maxdur <- filter(maxdur, maxdur == 0)     # Estos no tienen la dur minima para determinar

    # Esto es en caso de encontrar un dianoc sin dur sleep|wake suficiente.
    # Crea variable="dur2": la idea es que no queden "Dia" etiquetados como "Noche" porque
    # no se encontro un episodio que durara mas de sleep|wake dur.
    epi$dur2 <- 0
    if (nrow(maxdur) > 0){
        for (i in 1:nrow(maxdur)){
            # Para el dia
            if (maxdur$dino[i] == "Dia"){
                ifix <- min(epi[epi$dianoc == maxdur$dianoc[i] & epi$estado == "W", "indx"])
                epi$dur2[ifix] <- 1    # Aca se pone el inicio falso after inidia

            # para la noche
            } else if (maxdur$dino[i] == "Noche"){
                ifix <- min(epi[epi$dianoc == maxdur$dianoc[i] & epi$estado == "S", "indx"])
                epi$dur2[ifix] <- 1    # Aca se pone el inicio falso after inidia

            } else {
                stop("algo paso, dianoc sin stage > dursleep|durwake")
            }
        }
    }
    rm(maxdur, ifix)

    ######### temporal ############# ------
    epi <- mutate(epi, fin = as_date(ini))
    # View(epi)

    # | --- set -----------
    # Ya tenemos todos los dia noc con stage marcado con comienzo no importa si no tiene stage > 30
    # Ahora detectar el 1er stage mayor a durawake y dursleep
    epi$dur3 <- 0
    st <- epi$dino[1]
    i <- 1
    while (i < nrow(epi)){
        dino  <- epi$dino[i]
        dur  <- epi$dur[i]
        dur2 <- epi$dur2[i]
        stage <- epi$estado[i]


        # Si la linea dura mas de 30 mins
        if ((dino == st & dur == 1) | (dino == st & dur2 == 1)){
            # El primer dia queda mal puesto porque siempre parte despues de del "inidia", se arregla despues.

            # Como ya encontramos i > durWake|durSleep, cambia valor "st" para que en la siguiente iteracion
            # solo avance, en algun momento cambiara el dino original y volvera a hacer match
            # y seguira buscando hasta que encuentre una linea que tenga la duracion adecuada, volvera
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

        # Si encontramos que la linea tiene duracion < durSW pasa y anota cero
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
    # solo se le hace al dia
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
