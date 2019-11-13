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
# stop()

## ------------------------------------------------------------------------------------ #
## ----- Create ACV - Estado (W|S), fechas y horas partir de un awd ------------------- #
## ------------------------------------------------------------------------------------ #
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
    
    # Corrección del epoch, si 4 = 1 minuto, si 1 = 15 segundos, 2 = 30 secs
    if (epoch.len == 1){
        epoch.len <- 25
    } else if (epoch.len == 4){
        epoch.len <- 60
    } else {
        stop("error, algo pasó con el epoch len")
    }
    print(paste("Sensibilidad: ", sensi, "- Duracion epoch (secs):", epoch.len))
        
    # Crear fecha-hora inicial y Cambiar tambien el mes a ingles "se trabaja en character"
    fec.ini <- paste(awd.head[2], awd.head[3])
    mes <- data.frame(eng = as.character(c(1:12)),
        esp = c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic"),
        stringsAsFactors = FALSE)
    mes <- filter(mes, esp == unlist(str_split(fec.ini, "-", 3))[2])
    
    # Asume que está en español, se corrige si inglés.
    if (nrow(mes) > 0){
        fec.ini <- str_replace(fec.ini, mes[1,2], mes[1,1])
        fec.ini <- dmy_hm(fec.ini)
    } else {
        fec.ini <- dmy_hm(fec.ini)
    }
    
    # --- Crear Secuencia de fechas ---------------------------------------------- #
    fec.ini <- fec.ini + seconds(seq(0, by = epoch.len, length.out = length(awd.data)))
    awd <- data.frame(nombre = awd.head[1], act = awd.data, fec = fec.ini, stringsAsFactors = FALSE)
    awd <- mutate(awd, dec = hour(fec)+minute(fec)/60)
    
    # Recorte de peaks de actividad al percentil 98
    awd <- mutate(awd, act2 = ifelse(act > 0, act, NA))
    p98 <- quantile(awd$act2, 0.98, na.rm = TRUE)
    awd <- mutate(awd, act3 = ifelse(act < p98, act, p98), act3 = round(act3, 0))
    awd <- ordervar(awd, c("act2", "act3"), after = "act")
    
    ## Algoritmo del Actiwatch con el vector de actividad 3, el recortado
    ini <- 1
    fin <- nrow(awd)

    # Secuncia para determinar multiplcadores 
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

    # La magia... en una variable <<<< mini mitter algorithm >>>>
    # Weno, vamoa a intentar que sea en parallel
    act <- awd$act3
    acti.st <- rep(NA, times = fin)
    for (i in 1:fin){                                                                           # xx
        # Crear el vector de indices y extraerlo desde el de actividad
        indx <- c(i - pre, i, i + pos)
        indx[indx < 1] <- 0
        indx[indx > fin] <- 0
        epoch.data <- act[indx]
        
        # Si el trozo esta cerca del inicio|fin agrega ceros para tener una secuencia con N ok
        if (i < length(indx)){
            epoch.data <- c(rep(0, length(indx) - length(epoch.data)), epoch.data)
        } else if (i >= (fin - 8)) {
            epoch.data <- c(epoch.data, rep(0, length(indx) - length(epoch.data)))
        } 
        
        # Calculos, ponderados al multiplicador
        A <- sum(multi * epoch.data)
        
        # Decisiones según la sensibilidad
        if (sensi == 20){
            if (A > 20){acti.st[i] <- "W"} else {acti.st[i] <- "S"}
        } else if (sensi == 40){
            if (A > 40){acti.st[i] <- "W"} else {acti.st[i] <- "S"}
        } else if (sensi == 80){
            if (A > 80){acti.st[i] <- "W"} else {acti.st[i] <- "S"}
        }
    }
    
    # Agrega un indice variable para no depender del row.name o el index
    awd$acti <- acti.st
    awd <- mutate(awd, index = 1:dim(awd)[1])
    
    # ----- Calcular ahora el estado actigrafico corregido --------------------------------------- #
    # Calcular cuantas lineas tiene el statedur (duracion para cambio de estado)
    tdiff <- difftime(awd$fec[2], awd$fec[1], units = "secs")
    tdiff <- as.numeric(tdiff)
    tdiff <- statedur / tdiff

    # Buscar el STATE inicial... si, el primero [1]
    ws <- "NotOK"
    i <- 1
    while (ws != "Ok"){
        # Captura el primer STATE y ve cuantos S|W hay hasta que encuentra 5
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

    # Listo, ahora a contar desde i con state como inicial                                      # xx
    acti.fix <- rep(1:fin)
    acti <- awd$acti
    for (x in i:(nrow(awd) - tdiff + 1)){
        # Capturar el trozo
        stXmin <- acti[x:(x + tdiff - 1)]
        
        # Contar el STATE
        if (length(which(stXmin == state)) > 0){
            acti.fix[x] <- state
        } else if (length(which(stXmin == state)) == 0){
            # Cambio estado
            state <- acti[x]      # El de mini mitter
            acti.fix[x] <- state
        }
    }
    
    # Arreglar el final
    acti.fix[(nrow(awd) - tdiff + 2):nrow(awd)] <- state
    awd$acti2 <- acti.fix


    # ----- Calcular los STAGES a partir el acti2 ----------------------------------- #
    # Buscar indices que contengan W y S
    epi <- awd$acti2
    w <- which(epi == "W")
    s <- which(epi == "S")

    # Hacer diferencias al i+1 para ver dónde comienza W y S
    epiw <- data.frame(indx = w)
    epiw <- mutate(epiw, m = c(0, w[-length(w)]), d = m - indx)     # m = indx corrido un espacio
    epiw <- filter(epiw, d < -1)        # Si se salta el d será menor a un lugar (< -1)
    epiw <- mutate(epiw, stage = paste("W-", row.names(epiw), sep = ""))

    epis <- data.frame(indx = s)
    epis <- mutate(epis, m = c(0, s[-length(s)]), d = m - indx)
    epis <- filter(epis, d < -1)
    epis <- mutate(epis, stage = paste("S-", row.names(epis), sep = ""))

    # Corrige el final
    epi <- bind_rows(epiw, epis) %>% select(indx, stage) %>% arrange(indx)
    epi <- bind_rows(epi, data.frame(indx = nrow(awd), stage = NA))

    # Agregar el stage al awd
    stage <- rep(NA, nrow(awd))
    for (i in 1:(nrow(epi) - 1)){
        ini <- epi$indx[i]
        fin <- epi$indx[i+1] - 1
        stage[ini:fin] <- epi$stage[i]
    }
    awd$stage <- stage
    
    # Corregir el último y el primero
    awd$stage[nrow(awd)] <- epi$stage[nrow(epi)-1]
    #awd <- filter(awd, acti2 != "NC")
    
    # --- ahora siiii  :) ---------- #
    return(awd)
}

stop()
test <- create.acv("BenjaminVenegas.AWD", sensi = 40)
library(microbenchmark)
microbenchmark(create.acv(awdfile, sensi = 40), times = 20)



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
# Ejemplo
epifile <- readRDS("epi.rds")
awdfile <- readRDS("awd.rds")

create.actogram <- function(epifile = NULL, awdfile = NULL){}
    epi <- epifile
    awd <- awdfile

    # Trajinando el epi primero
    epi <- cbind(epi, data.frame(str_split(epi$periodo, " ", simplify = TRUE), stringsAsFactors = FALSE))
    epi <- rename(epi, dianoc = X1, nper = X2)

    # Merge epi - awd
    epi <- select(epi, stage, duracion, periodo, dianoc, nper)
    awd <- omerge(awd, epi, byvar = "stage", keep = TRUE)       # Se va el primer dia
    awd <- awd$match
    awd <- arrange(awd, index) %>% select(-merge, -act2, -half)

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


    ## ----- Grafico ------------------------------------------------------------------ #
    per <- "03"
    gdata <- filter(awd, nper == per) %>% arrange(fec) 
    head(gdata)
    nrow(gdata) /5

    # Escala de X 
    temp <- gdata$hrdec
    temp <- floor(temp)
    temp <- unique(temp)
    temp <- c(temp, max(temp)+1)

    # Labels (horas)
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
    limY <-  c(0, ceiling(max(gdata$act3)/100)*100)
    limX <- c(min(temp), max(temp))

    # Actograma de 1 periodo completo
    g <- ggplot(data = gdata, aes(x = hrdec, y = act3)) 
    g <- g + scale_y_continuous(limits = limY, expand = c(0, 0, 0.05, 0))
    g <- g + scale_x_continuous(labels = glab, breaks = temp, limits = c(min(temp), max(temp)), expand = c(0.01, 0))
        
    for (i in 1:nrow(sdata)){
        g <- g + annotate("rect", fill = "skyblue1", alpha = 0.5,
            xmin = sdata$min[i], xmax = sdata$max[i], 
            ymin = -Inf, ymax = Inf)
    }
    
    g <- g + geom_area() + geom_vline(xintercept = ylinea, colour = "red")
    g <- g + ylab(format(date(gdata$fec[1]), "%a %d-%b-%Y")) + xlab(NULL)

    
    
    # A probar en la versión base

    par(mar=c(2,2,0,2) + 0.5, xaxs = 'i', yaxs = 'i')
    plot(gdata$hrdec, gdata$act3, type='n', ylab='', axes=FALSE, xlim=limX, ylim=limY)
    for (i in 1:nrow(sdata)){
        rect(sdata$min[i], 0, sdata$max[i], limY[2], col = alpha("skyblue", 0.5), border = "skyblue")
    }
    par(new=TRUE)
    plot(gdata$hrdec, gdata$act3, type='h', col='grey20', ylab='', axes=FALSE, xlim=limX, ylim=limY)
    abline(v = ylinea, col = "red")
    title(ylab = (format(date(gdata$fec[1]), "%A %d, %m--%Y")), line = 0.5)
    axis(side = 1, at = temp, labels = glab)
    box()
    mtext(format(date(gdata$fec[nrow(gdata)]), "%A %d, %m--%Y"), side = 4, line = 0.5)
    

    
     
#     png('actogram.png', width = 1200, height = 230, units = "px" )
#     dev.off()

    ??col
    
