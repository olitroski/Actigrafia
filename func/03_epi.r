## ------------------------------------------------------------------------------------ #
## --- Create EPI - Detección los episodios de sueño y vigilia + noche o dia ---------- #
## ------------------------------------------------------------------------------------ #
# La logica es como ahora tengo el stage actigrafico alisado, puedo contruir periodos y 
# hacerlos consecutivos y hacerles un summarize y buscar la noche y el día.
create.epi <- function(awd = NULL){
    # ----- Crear el pseudo epi ------------------------------------------------------------ #
    # Como ya tenemso los estados para cada epoch numerados se puede hacer la detección
    # de noche y dia

    # --- Crear un epi FTW --- #
    epi <- group_by(awd, stage)
    epi <- summarize(epi, ini = min(fec), fin = max(fec), 
        duracion = difftime(fin, ini, units = "mins"), estado = unique(acti2))
    epi <- as.data.frame(epi)
    epi <- arrange(epi, ini) %>% select(-fin)
    epi <- mutate(epi, duracion = as.numeric(duracion) + 1)   # Corrige la duracion

    # --- Detección de la noche --- #
    epi$periodo <- NA
    fec <- epi$ini[1]
    i <- 1
    Nnoc <- 1
    while (fec < epi$ini[dim(epi)[1]]){
        # Si el i es mayor al dim
        if (i > dim(epi)[1]){
            break()
        # Si es W pasa al siguiente
        } else if (epi$estado[i] == "W"){
            i <- i + 1
        # Si es Sueño lo considera
        } else if (epi$estado[i] == "S"){
            # fecha|hora de la linea '>=' a 'fec'
            if (epi$ini[i] >= fec){
                # Corrección por si es past 00
                if (hour(epi$ini[i]) < 20){
                    hora <- hm(paste(hour(epi$ini[i]) + 24, ":", minute(epi$ini[i]), sep=""))
                } else {
                    hora <- hm(paste(hour(epi$ini[i]), ":", minute(epi$ini[i]), sep=""))
                }
                # Que la hora del posix sea mayor a ininoc
                if (hora > ininoc){
                    # Que la linea tenga más de dursleep
                    if (epi$duracion[i] >= dursleep){
                        # Si llegamos acá la primera vez será la primera noche tons 
                        # hay que modificar el fec + 24h para el proximo dia
                        minplus <- hm("24:00") - (hora - ininoc)
                        fec <- epi$ini[i] + minplus
                        # Rellenar el período
                        epi$periodo[i] <- paste("Noche", 
                            if (Nnoc < 10){paste("0", Nnoc, sep = "")})
                        # Mueve el indice y el Nnoc
                        i <- i + 1
                        Nnoc <- Nnoc + 1
                    } else {i <- i + 1}
                } else {i <- i + 1}
            } else {i <- i + 1}
        # Si es sueño pero no cumplió
        } else {i <- i + 1}
    }

    # Rellena los espacios NA
    epi$periodo[1:(which(epi$periodo == "Noche 01")-1)] <- "PreNoc1"
    for (i in 1:dim(epi)[1]){
        if (i >= dim(epi)[1]){
            break()
        } else if (is.na(epi$periodo[i+1])){
            epi$periodo[i+1] <- epi$periodo[i]  
        } else {
            i <- i + 1
        }
    }

    # --- Detección del día ------- #
    # Este usa la detección de la noche como argumento
    noches <- unique(epi$periodo)
    noches <- noches[grep("Noche", noches)]
    epi2 <- NULL
    for (noc in noches){
        nocdata <- filter(epi, periodo == noc)
        
        for (i in 1:dim(nocdata)[1]){
            # Que sea pasado "inidia"
            # Si la hora está entre 20 y 24 sumale un dia a fec
            hora <- hm(paste(hour(nocdata$ini[i]), ":", minute(nocdata$ini[i]), sep = ""))
            if (hora >= ininoc & hora < hms("23:59:59")){
                fec <- (date(nocdata$ini[i]) + days(1)) + inidia
            } else {
                fec <- (date(nocdata$ini[i]) + days(0)) + inidia
            }
            
            # Checar todo
            if (nocdata$ini[i] > fec){
                # Además es vigilia
                if (nocdata$estado[i] == "W"){
                    # Además dura más de durawake
                    if (nocdata$duracion[i] >= durawake){
                        nocdata$periodo[i:dim(nocdata)[1]] <- sub("Noche ", "Dia ", nocdata$periodo[i])
                        i <- dim(nocdata)[1] + 1    # Pa salir
                    }
                } else {i <- i + 1}
            } else {i <- i + 1}
        }

        # Combina
        epi2 <- bind_rows(epi2, nocdata)
        
    }

    # Retorno y un arreglin final
    epi2 <- rename(epi2, fec = ini)
    return(epi2)
}
