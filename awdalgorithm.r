# ------------------------------------------------------------------------------------- #
# ----- Replica del algoritmo del actividom --- 24.09.2019 v1.0 ----------------------- #
# ------------------------------------------------------------------------------------- #
# Antecedentes
rm(list=ls())
source("D:/OneDrive/GitHub/sources/exploratory/local_sources.r")
setwd("D:/OneDrive/INTA/Actigrafia")
library(tictoc)
source("settings.r")
dir()
#stop()

## ------------------------------------------------------------------------------------ #
## ----- Create ACV - Estado (W|S), fechas y horas partir de un awd ------------------- #
## ------------------------------------------------------------------------------------ #
# okeep("statedur", fun = TRUE)
# awdfile <- "BenjaminVenegas.AWD"
create.acv <- function(awdfile = NULL, sensi = NULL){
    # Leer archivo
    awd <- readLines(awdfile)
    awd.head <- awd[1:7]   
    awd.data <- awd[-(1:7)]
    
    # Corrige si encuentra M's
    if (length(grep("M", awd.data)) > 0){
        M <- grep("M", awd.data)
        awd.data[M] <- sub("M", "", awd.data[M])
    }
    awd.data <- as.numeric(awd.data)
    rm(M)
    
    # --- Antecedentes del header  --------------------------------------------- #
    # Variables originales
    name <- awd.head[1]
    date <- awd.head[2]
    hour <- awd.head[3]
    epoch.len <- as.numeric(awd.head[4])
    edad <- awd.head[5]
    serie <- awd.head[6]
    sexo <- awd.head[7]
    
    # Corrección del epoch, si 4 es 1 minuto, si 1 es 25 segundos
    if (epoch.len == 1){
        epoch.len <- 25
    } else if (epoch.len == 4){
        epoch.len <- 60
    } else {
        stop("error, algo pasó con el epoch len")
    }
    print(paste("Sensibilidad: ", sensi, "- Duracion epoch (secs):", epoch.len))
        
    # Crear fecha-hora inicial y Cambiar tambien el mes a ingles
    fec.ini <- paste(awd.head[2], awd.head[3])
    mes <- data.frame(eng = as.character(c(1:12)),
        esp = c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic"),
        stringsAsFactors = FALSE)
    mes <- filter(mes, esp == unlist(str_split(fec.ini, "-", 3))[2])
    fec.ini <- str_replace(fec.ini, mes[1,2], mes[1,1])
    fec.ini <- dmy_hm(fec.ini)
    
    # Crear Secuencia de fechas
    epoch <- 15
    fec.ini <- fec.ini + seconds(seq(0, by = epoch.len, length.out = length(awd.data)))
    awd <- data.frame(nombre = awd.head[1], act = awd.data, fec = fec.ini, stringsAsFactors = FALSE)

    # Refritos de la hora
    awd <- mutate(awd, 
        abs = hour(fec), 
        dec = hour(fec)+minute(fec)/60,
        min = dec - abs, 
        half = ifelse(min < 0.5, abs, abs + 0.5)
        ) %>% select(-min)
    
    # Refritos de la actividad
    awd <- mutate(awd, act2 = ifelse(act > 0, act, NA))
    p98 <- quantile(awd$act2, 0.98, na.rm = TRUE)
    awd <- mutate(awd, act3 = ifelse(act < p98, act, p98), act3 = round(act3, 0))
    awd <- ordervar(awd, c("act2", "act3"), after = "act")
    
    ## Algoritmo del Actiwatch
    act <- awd$act3
    ini <- 1
    fin <- length(act)

    # Secencia para determinar multiplcadores epoch.len <- 60
    if (epoch.len == 15){
        pre <- c(8:1)
        pos <- c(1:8)
        multi <- c(rep(1/25,4), rep(1/5,4), 4, rep(1/5,4), rep(1/25,4))

    } else if (epoch.len == 30){
        pre <- c(4:1)
        pos <- c(1:4)
        multi <- c(1/25, 1/25, 1/5 ,1/5 , 2 , 1/5, 1/5, 1/25, 1/25)

    } else if (epoch.len == 60){
        pre <- c(2:1)
        pos <- c(1:2)
        multi <- c(1/25, 1/5, 1, 1/5, 1/25)

    } else if (epoch.len == 120){
        pre <- 1
        pos <- 1
        multi <- c(0.12, 1/2, 0.12)

    } else {
        error("En la seleccion del multiplicador")

    }

    # La magia... en una variable
    awd$acti <- NA
    for (i in 1:fin){
        # Crear el vector de indices y extraerlo desde el de actividad
        indx <- c(i - pre, i, i + pos)
        indx[indx < 1] <- 0
        indx[indx > fin] <- 0
        epoch.data <- act[indx]
        
        # Agregar ceros si el i está cercano al inicio o fin
        if (i < length(indx)){
            epoch.data <- c(rep(0, length(indx) - length(epoch.data)), epoch.data)
        } else if (i >= (fin - 8)) {
            epoch.data <- c(epoch.data, rep(0, length(indx) - length(epoch.data)))
        } 
        
        # Calculos, en A20, A40 y A80
        A <- sum(multi * epoch.data)
        
        # Decisiones según la sensibilidad
        if (sensi == 20){
            if (A > 20){awd$acti[i] <- "W"} else {awd$acti[i] <- "S"}
        } else if (sensi == 40){
            if (A > 40){awd$acti[i] <- "W"} else {awd$acti[i] <- "S"}
        } else if (sensi == 80){
            if (A > 80){awd$acti[i] <- "W"} else {awd$acti[i] <- "S"}
        }
        
        
    }

    # Alguna cosa adicional y pa fuera
    awd <- mutate(awd, index = 1:dim(awd)[1])
    
    ## Na que pa fuera, calcular ahora el estado actigrafico corregido 
    # Calcular cuantas lineas tiene el statedur
    tdiff <- difftime(awd$fec[2], awd$fec[1], units = "secs")
    tdiff <- as.numeric(tdiff)
    tdiff <- statedur / tdiff

    # Buscar el estado inicial
    ws <- "NotOK"
    i <- 1
    while (ws != "Ok"){
        # Captura el trozo y ve cuantos hay
        stXmin <- awd$acti[i:(i + tdiff - 1)]
        nW <- length(which(stXmin == "W")) 
        nS <- length(which(stXmin == "S")) 
        
        # Lógica para detener
        if (nW == tdiff){
            state <- "W"
            ws = "Ok"
        } else if (nS == tdiff){
            state <- "S"
            ws = "Ok"
        } else {
            i <- i + 1
        }
    }

    # Si hay algo antes del 1° estado marcar como NC y crea variable
    awd$acti2 <- NA
    if (i > 1){
        awd$acti2[1:(i-1)] <- "NC"
    }

    # Listo, ahora a contar desde i con state como inicial
    for (x in i:(dim(awd)[1] - tdiff + 1)){
        # Capturar el trozo
        stXmin <- awd$acti[x:(x + tdiff - 1)]
        
        # Contar el state
        if (length(which(stXmin == state)) > 0){
            awd$acti2[x] <- state
        } else if (length(which(stXmin == state)) == 0){
            # Cambio estado
            state <- awd$acti[x]
            awd$acti2[x] <- state
        }
    }
    # Arreglar el final
    awd$acti2[(dim(awd)[1] - tdiff + 2):(dim(awd)[1])] <- state

    # Ahora si estamos listos... pa fuera
    # Jaja nope aun
        # ----- Calcular los estados a partir el acti2 ----------------------------------- #
    # Buscar indices que contengan W y S
    epi <- awd$acti2
    w <- which(epi == "W")
    s <- which(epi == "S")

    # Hacer diferncias al i+1 para ver dónde comienza W y S
    epiw <- data.frame(indx = w)
    epiw <- mutate(epiw, m = c(0, w[-length(w)]), d = m - indx)
    epiw <- filter(epiw, d < -1)
    epiw <- mutate(epiw, stage = paste("W-", row.names(epiw), sep = ""))

    epis <- data.frame(indx = s)
    epis <- mutate(epis, m = c(0, s[-length(s)]), d = m - indx)
    epis <- filter(epis, d < -1)
    epis <- mutate(epis, stage = paste("S-", row.names(epis), sep = ""))

    # Corrige el final
    epi <- bind_rows(epiw, epis) %>% select(indx, stage) %>% arrange(indx)
    epi <- bind_rows(epi, data.frame(indx = dim(awd)[1], stage = NA))

    # Agregar el stage al awd
    awd$stage <- NA
    for (i in 1:(dim(epi)[1]-1)){
        ini <- epi$indx[i]
        fin <- epi$indx[i+1] - 1
        awd$stage[ini:fin] <- epi$stage[i]
    }
    # Corregir el último y el primero
    awd$stage[dim(awd)[1]] <- epi$stage[dim(epi)[1]-1]
    #awd <- filter(awd, acti2 != "NC")
    
    # --- ahora siiii  :) ---------- #
    return(awd)
}


## ------------------------------------------------------------------------------------ #
## --- Create EPI - Detección de la noche o dia---------------------------------------- #
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


# Prueba
stop()
awdfile <- "BenjaminVenegas.AWD"
awd <- create.acv(awdfile, sensi = 40)
epi <- create.epi(awd)
# saveRDS(awd, "awd.rds")
# saveRDS(epi, "epi.rds")



## ------------------------------------------------------------------------------------ #
## ----- Actograma con ggplot --------------------------------------------------------- #
## ------------------------------------------------------------------------------------ #
epi <- readRDS("epi.rds")
awd <- readRDS("awd.rds")
head(epi)

create.actogram <- function(epifile = NULL, awdfile = NULL){}

# Trajinando el epi primero
epi <- cbind(epi, data.frame(str_split(epi$periodo, " ", simplify = TRUE), stringsAsFactors = FALSE))
epi <- rename(epi, dianoc = X1, nper = X2)
# epi <- mutate(epi, fec2 = fec + minutes(duracion))
# epi <- ordervar(epi, 'fec2', after = 'fec')

# # Corregir las horas agregando la duracion al 'fec' per = "03"
# temp <- NULL
# for (per in unique(epi$nper)){
#     epidata <- filter(epi, nper == per) %>% arrange(fec) 
#     
#     # Horas de inicio
#     hrstart <- epidata$fec[1]
#     if (hour(hrstart) < ininoc){
#         temp.date <- (date(hrstart) - 1) + ininoc
#     }
#     dec <- hour(temp.date) + minute(temp.date)/60
#     epidata$hini <- as.numeric(difftime(epidata$fec, temp.date, units = 'hours') + dec)
#     
#     # Horas de fin
#     hrstart <- epidata$fec2[1]
#     if (hour(hrstart) < ininoc){
#         temp.date <- (date(hrstart) - 1) + ininoc
#     }
#     dec <- hour(temp.date) + minute(temp.date)/60
#     epidata$hfin <- as.numeric(difftime(epidata$fec2, temp.date, units = 'hours') + dec)
#     
#     # epidata <- select(epidata, -fec2)
#     temp <- rbind(temp, epidata)
# }
# epi <- temp
# head(epi)


# Combinar el epi con el awd
epi <- select(epi, stage, duracion, periodo, dianoc, nper)
head(epi)
head(awd)
awd <- omerge(awd, epi, byvar = "stage", keep = TRUE)       # Se va el primer dia
awd <- awd$match
awd <- arrange(awd, index) %>% select(-merge, -act2, -half)
# awd <- mutate(awd, fec2 = fec + minutes(duracion))

# Corregir hora inicio de cada etapa al formato continuo de hora
temp <- NULL
for (per in unique(awd$nper)){        # 'fec' per = "03"
    per.data <- filter(awd, nper == per) %>% arrange(fec) 
    hrstart <- per.data$fec[1]
    
    # Necesito el ininoc con fecha
    if (hour(hrstart) < ininoc){
        temp.date <- (date(hrstart) - 1) + ininoc
    } else {
        temp.date <- date(hrstart)
    }
    
    # Cuanta diferencia entre la fecha y el temp.date
    per.data <- mutate(per.data, delta = difftime(fec, temp.date, units = 'hours'))
    per.data <- mutate(per.data, hrdec = as.numeric(20 + delta)) %>% select(-delta)    # <<<<<< arreglar esto
    temp <- rbind(temp, per.data)
}
awd <- temp
head(awd)


# Grafico
per <- "03"
gdata <- filter(awd, nper == per) %>% arrange(fec) 
head(gdata)
nrow(gdata) /5

# Para las escalas
temp <- gdata$hrdec
temp <- floor(temp)
temp <- unique(temp)
temp <- c(temp, max(temp)+1)

# Labels
glab <- NULL
for (i in temp){
    if (i >= 48){
        glab <- c(glab, i - 48)
    } else if (i >= 24) {
        glab <- c(glab, i - 24)
    } else {
        glab <- c(glab, i)
    }
}

# Lineas al inicio, dia, y fin
ylinea <- c(gdata$hrdec[1],
            max(gdata$hrdec[gdata$dianoc == "Noche"]),
            gdata$hrdec[nrow(gdata)])

# sueño data para el background
sdata <- filter(gdata, acti2 == "S")
sdata <- group_by(sdata, stage)
sdata <- as.data.frame(summarize(sdata, min = min(hrdec), max = max(hrdec)))

# Limites de Y
max(gdata$act3)


# Actograma de 1 periodo completo
g <- ggplot(data = gdata, aes(x = hrdec, y = act3)) 
for (i in 1:nrow(sdata)){
    g <- g + annotate("rect", fill = "red",
        xmin = sdata$min[i], xmax = sdata$max[i], 
        ymin = -Inf, ymax = Inf)
}
g

i <- 1
    g + annotate("rect", fill = "red",
        xmin = sdata$min[i], xmax = sdata$max[i], 
        ymin = -Inf, ymax = Inf)
g


i <- 1
g + annotate("rect", fill = "red", xmin=sdata$min[i], xmax=sdata$max[i], ymin=-Inf,ymax=Inf)
g






g <- g + geom_col() + scale_y_continuous(limits = c(0, 1500), expand = c(0,0))
g <- g + scale_x_continuous(labels = glab, breaks = temp, limits = c(min(temp), max(temp)), expand = c(0.01,0.01))
g <- g + ylab(format(date(gdata$fec[1]), "%a %d-%b-%Y")) + xlab(NULL)
g <- g + geom_vline(xintercept = ylinea, colour = "red")

g



head(gdata)
