## ------------------------------------------------------------------------------------ #
## ----- Create ACV - Estado (W|S), fechas y horas partir de un awd ------------------- #
## ------------------------------------------------------------------------------------ #
create.acv <- function(awdfile = NULL, sensi = NULL){
    # Librerias
    library(dplyr); library(lubridate); library(stringr)

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
    
    #----- Antecedentes del header  --------------------------------------------- 
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
    print(paste("File:", awdfile, " - Sensi:", sensi, " - Sampling:", epoch.len, " segundos", sep = ""))
        
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
    
    #----- Calcular ahora el estado actigrafico corregido -------------------------------
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


    # ----- Calcular los STAGES a partir el acti2 ----------------------------------- 
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

# stop()
# awdfile <- "BenjaminVenegas.AWD"; sensi <- 40
# system.time(create.acv("BenjaminVenegas.AWD", sensi = 40))
