## ------------------------------------------------------------------------------------ #
## --- Create EPI - Detección los episodios de sueño y vigilia + noche o dia ---------- #
## ------------------------------------------------------------------------------------ #
# La logica es como ahora tengo el stage actigrafico alisado, puedo contruir periodos y 
# hacerlos consecutivos y hacerles un summarize y buscar la noche y el día.
create.epi <- function(awd = NULL){
    # ----- Crear el pseudo epi ------------------------------------------------------------ #
    # Como ya tenemos los estados para cada epoch numerados se puede hacer la detección
    # de noche y dia

    # --- Crear un epi FTW ------- #
    head(awd)
    awd <- filter(awd, stage != is.na(stage))
    epi <- group_by(awd, stage)
    epi <- summarize(epi, ini = min(fec), fin = max(fec), 
        duracion = difftime(fin, ini, units = "mins"), estado = unique(acti2))
    epi <- as.data.frame(epi)
    epi <- arrange(epi, ini) %>% select(-fin)
    epi <- mutate(epi, duracion = as.numeric(duracion) + 1)   # Corrige la duracion

    # dia o noche según las horas de ininoc e inidia y marca duracion <> durawake
    hr <- function(x){hm(paste(hour(x),":",minute(x), sep=""))}
    epi <- mutate(epi, dino   = ifelse(hr(ini) >= hm("00:00") & hr(ini) < inidia, "Noche",
                                ifelse(hr(ini) >= inidia & hr(ini) < ininoc, "Dia",
                                ifelse(hr(ini) >= ininoc & hr(ini) < hms("23:59:59"), "Noche", "Error"))),
                       dur    = ifelse(dino == "Dia" & duracion >= durawake, 1,
                                ifelse(dino == "Noche" & duracion >= dursleep, 1, 0)))
    
    
# Asigna 1 al primer episodio cuando no se encuentra > dursleep|durawake
    # Logicos para inicio dia noc
    epi <- epi %>% arrange(ini) %>% mutate(dur2 = dino == lag(dino), 
              dur2 = ifelse(is.na(dur2) | dur2==FALSE, FALSE, TRUE), indx = 1:nrow(epi))
    
    # Indices el correlativo de dia noc
    ndino <- bind_rows(
                filter(epi, dino=="Dia", dur2==FALSE) %>% select(dino, indx) %>% mutate(n = 1:n()),
                filter(epi, dino=="Noche", dur2==FALSE) %>% select(dino, indx) %>% mutate(n = 1:n())) %>%
                arrange(indx) %>% mutate(indx2 = lead(indx))
    ndino$indx2[nrow(ndino)]  <- nrow(epi)
 
    # Correlativo de dianoc
    for (i in 1:nrow(ndino)){
        epi[ndino$indx[i]:ndino$indx2[i], "ndino"] <- ndino$n[i]
    }
    epi <- mutate(epi, ndino = ifelse(ndino<10, paste("0",ndino,sep=""),ndino), dianoc = paste(dino, ndino))
    
    # Evaluar si hay dia noc sin stages de mas de 30 mins (util para las wawas)
    maxdur <- group_by(epi, dianoc) %>% summarize(maxdur = max(dur), indx = min(indx), 
            dino = unique(dino)) %>% filter(maxdur == 0)
    
    # Modifica el "dur" según esto, la idea es que no queden dias etiquetados como noche porque
    # no se encontró un episodio que durara más de sleep dur.
    epi$dur2 <- 0
    if (nrow(maxdur) > 0){
        for (i in 1:nrow(maxdur)){
            if (maxdur$dino[i] == "Dia"){
                ifix <- filter(epi, dianoc == maxdur$dianoc[i]) %>% group_by(estado) %>% 
                            summarize(indx = min(indx)) %>% filter(estado == "W") %>% pull(indx)
                epi$dur2[ifix] <- 1
            } else if (maxdur$dino[i] == "Noche"){
                ifix <- filter(epi, dianoc == maxdur$dianoc[i]) %>% group_by(estado) %>% 
                            summarize(indx = min(indx)) %>% filter(estado == "S") %>% pull(indx)
                epi$dur2[ifix] <- 1
            } else {
                stop("algo paso, dianoc sin stage > dursleep|durwake")
            }
        }
    }

# Ya tenemos todos los dia noc con stage marcado con comienzo no importa si no tiene stage > 30    
    # Ahora detectar el 1° stage mayora a durawake y dursleep
    epi$dur3 <- 0
    st <- epi$dino[1]
    i <- 1
    while (i < nrow(epi)){
        dino  <- epi$dino[i]
        dur  <- epi$dur[i]
        dur2 <- epi$dur2[i]
        stage <- epi$estado[i]
        # dino; dur; dur2; stage
        
        # Si es dia que ponga el 1° cuando W y viseversa
        if ((dino == st & dur == 1) | (dino == st & dur2 == 1)){
            if (dino == "Dia" & stage == "W"){
                epi$dur3[i] <- 1
                if (st == "Dia"){st <- "Noche"} else if (st == "Noche"){st <- "Dia"}
                i <- i + 1
            } else if (dino == "Noche" & stage == "S"){
                epi$dur3[i] <- 1
                if (st == "Dia"){st <- "Noche"} else if (st == "Noche"){st <- "Dia"}
                i <- i + 1
            } else {
                i <- i + 1
            }
        } else {
            epi$dur3[i] <- 0
            i <- i + 1
        }
    }
    
# Borrar lo que ya no sirve
    epi <- select(epi, -dur, -dur2, -ndino, -dianoc)

# Fabricar el dianoc final, ya se hicieron las correcciones y todo
    # Indices el correlativo de dia noc
    ndino <- bind_rows(
                filter(epi, dino=="Dia", dur3==1) %>% select(dino, indx) %>% mutate(n = 1:n()),
                filter(epi, dino=="Noche", dur3==1) %>% select(dino, indx) %>% mutate(n = 1:n())) %>%
                arrange(indx) %>% mutate(indx2 = lead(indx))
    ndino$indx2[nrow(ndino)]  <- nrow(epi)
 
    # Correlativo de dianoc final
    for (i in 1:nrow(ndino)){
        epi[ndino$indx[i]:ndino$indx2[i], "ndinofinal"] <- ndino$n[i]
    }
    

    # Corregir el epi$dino porque puede estar descuadrado
    epi$dinofinal <- NA
    i <- filter(epi, dur3==1) %>% summarize(i = min(indx)) %>% pull(i)
    epi$dinofinal[i] <- epi$dino[i]
    i <- i + 1
    st <- epi$dino[i]

    while (i <= nrow(epi)){
        dino  <- epi$dino[i]
        dur3  <- epi$dur3[i]
        
        if (dino != st & dur3 == 1){
            if (st == "Dia"){st <- "Noche"} else if (st == "Noche"){st <- "Dia"}
            epi$dinofinal[i] <- st
            i <- i + 1
        } else {
            epi$dinofinal[i] <- st
            i <- i + 1
        }
    }

    # Resta 1 a ndino de dia para que empiece del 0 y hace el correlativo
    epi <- mutate(epi, ndinofinal = ifelse(dinofinal == "Dia", ndinofinal - 1, ndinofinal))
    
    # Hace el dianocfinal
    epi <- mutate(epi, ndinofinal = ifelse(ndinofinal < 10, 
                                           paste("0", ndinofinal, sep = ""),
                                           ndinofinal))
    epi <- mutate(epi, periodo = paste(dinofinal, ndinofinal)) %>% rename(dianoc = dinofinal)
    
    # Limpiar y entregar
    epi <- select(epi, stage, ini, indx, duracion, estado, periodo, dianoc) %>%
           filter(is.na(dianoc) == FALSE) %>% rename(fec = ini)
    return(epi)
}
