# -*- coding: ANSI -*-
## ------------------------------------------------------------------------------------ #
## ----- Create ACV - Estado (W|S), fechas y horas partir de un awd ------------------- #
## ------------------------------------------------------------------------------------ #
create.acv <- function(awdfile = NULL, sensi = NULL, edit = FALSE){
	cat(paste0("| Iniciando Archivo: ", awdfile, "\n"))
	
    # Leer archivo
    awd <- readLines(awdfile)
    awd.head <- awd[1:7]   
    awd.data <- awd[-(1:7)]
    
    # Corrige si encuentra M's
    if (length(grep("M", awd.data)) > 0){
        M <- grep("M", awd.data)
        awd.data[M] <- sub("M", "", awd.data[M])
        rm(M)
    }
    awd.data <- as.numeric(awd.data)
   
    # Revisar que solo hayan numeros desde la posición 8 en adelante
    numtest <- as.numeric(awd.data)
    if (sum(is.na(numtest)) > 0){
        stop("Hay un valor no numerico en el AWD a partir linea 8")
    }
    
    
    # ----------------------------------------------------------------------------------- #
    # ----- Antecedentes del header  ---------------------------------------------------- # 
    # ----------------------------------------------------------------------------------- #
    ## Procesar los antecedentes del header
    # Variables originales
    name <- awd.head[1]
    date <- awd.head[2]
    hour <- awd.head[3]
    epoch.len <- as.numeric(awd.head[4])
    edad <- awd.head[5]
    serie <- awd.head[6]
    sexo <- awd.head[7]
    
    # Identificar el epoch, si 4 = 1 minuto, si 1 = 15 segundos, 2 = 30 secs
    if (epoch.len == 1){
        epoch.len <- 15
    } else if (epoch.len == 2) {
        epoch.len <- 30
    } else if (epoch.len == 4){
        epoch.len <- 60
    } else {
        stop("error, algo pasó con el epoch len")
    }
	
	# cat(paste("|--- cAcv: Sens = ", sensi, " - Samp = ", epoch.len, "secs \n", sep = ""))
        
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
    
    # --- Crear Secuencia de fechas (Crea el AWD) ------------------------------------- #
    fec.ini <- fec.ini + seconds(seq(0, by = epoch.len, length.out = length(awd.data)))
    awd <- data.frame(nombre = awd.head[1], act = awd.data, fec = fec.ini, stringsAsFactors = FALSE)
    awd <- mutate(awd, dec = hour(fec)+minute(fec)/60)
    
    # Recorte de peaks de actividad al percentil 98
    awd <- mutate(awd, act2 = ifelse(act > 0, act, NA))
    p98 <- quantile(awd$act2, 0.98, na.rm = TRUE)                                       # >>>> el centile <<<<
    awd <- mutate(awd, act3 = ifelse(act < p98, act, p98), act3 = round(act3, 0))
    awd <- ordervar(awd, c("act2", "act3"), after = "act")
    
    
    # --------------------------------------------------------------------------------- #
    # ----- Algoritmo del Actiwatch - MiniMitter -------------------------------------- #
    # --------------------------------------------------------------------------------- #
	cat("|--- cAcv: Algorithm Minimitter -Start-\n")
    ## Usa el vector de actividad recortado con el percentil 98
    ini <- 1
    fin <- nrow(awd)

    # Secuencia para determinar multiplcadores 
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
    
    # Algoritmo de detección
    act <- awd$act3
    acti.st <- rep(NA, times = fin)
    for (i in 1:fin){                                                                           # xx
        # Crear el vector de indices y extraerlo desde el de actividad
        indx <- c(i - pre, i, i + pos)
        indx[indx < 1] <- 0
        indx[indx > fin] <- 0
        epoch.data <- act[indx]     # <<<< la secuencia de actividad >>>>
        
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
    
    
    # ----------------------------------------------------------------------------------- #
    # ---- Calcular ahora el estado actigrafico corregido ------------------------------- #
    # ----------------------------------------------------------------------------------- #
	cat("|--- cAcv: Estado Actigrafico -Start-\n")
    # Con la regla de que para que cambie debe pasar 5 (statdur) minutos
    # Calcular cuantas lineas tiene el statedur (duracion para cambio de estado)
    tdiff <- difftime(awd$fec[2], awd$fec[1], units = "secs")
    tdiff <- as.numeric(tdiff)
    tdiff <- set$statedur / tdiff

    # Buscar el STATE inicial como los primeros "statedur" (5) epoch iguales
    # pueden entonces quedar algunos epoch iniciales sin estado corregido.
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
    
    # Si es que parte desde el inicio en el mismo estado obliga a que parta del epoch 2
    if (i == 1){
        i <- 2
    }

    # Listo, ahora a contar desde i con "obj::state" como inicial
    acti.fix <- rep(1:fin)      # de aqui sale el acti2[1]=1
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
    
    
    # ----------------------------------------------------------------------------------- #
    # ----- Calcular los STAGES a partir el acti2 --------------------------------------- # 
    # ----------------------------------------------------------------------------------- #
#     # Buscar indices que contengan W y S
#     epi <- awd$acti2
#     w <- which(epi == "W")
#     s <- which(epi == "S")
# 
#     # Hacer diferencias al i+1 para ver dónde comienza W y S
#     epiw <- data.frame(indx = w)
#     epiw <- mutate(epiw, m = c(0, w[-length(w)]), d = m - indx)     # m = indx corrido un espacio
#     epiw <- filter(epiw, d < -1)        # Si se salta el d será menor a un lugar (< -1)
#     epiw <- mutate(epiw, stage = paste("W-", row.names(epiw), sep = ""))
# 
#     epis <- data.frame(indx = s)
#     epis <- mutate(epis, m = c(0, s[-length(s)]), d = m - indx)
#     epis <- filter(epis, d < -1)
#     epis <- mutate(epis, stage = paste("S-", row.names(epis), sep = ""))
# 
#     # Corrige el final
#     epi <- bind_rows(epiw, epis) %>% select(indx, stage) %>% arrange(indx)
#     epi <- bind_rows(epi, data.frame(indx = nrow(awd), stage = NA))
# 
#     # Agregar el stage al awd
#     stage <- rep(NA, nrow(awd))
#     for (i in 1:(nrow(epi) - 1)){
#         ini <- epi$indx[i]
#         fin <- epi$indx[i+1] - 1
#         stage[ini:fin] <- epi$stage[i]
#     }
#     awd$stage <- stage
#     
#     # Corregir el último y el primero
#     awd$stage[nrow(awd)] <- epi$stage[nrow(epi)-1]
#     #awd <- filter(awd, acti2 != "NC")


    # ---- Cambiar nombres de variables y ordenar ------------------------------------- #
    awd <- select(awd, index, nombre, act, act3, fec, dec, acti, acti2)
    names(awd) <- c("indx","nombre","act.raw", "act.smooth", "time", "hrdec", "st.mm", "st.stable")
    
    # Copia elementos para la edicion
    awd$act.edit <- awd$act.smooth
    awd$st.edit <- awd$st.stable
    awd$filter <- NA
    
    # quitar minutos iniciales sin estado
    ini <- nrow(awd)
    awd <- filter(awd, st.edit == "S" | st.edit =="W")
    fin <- nrow(awd)
    cat(paste("|--- cAcv: Lineas borradas al inicio =", ini - fin, "\n"))
    
    # Agregar el nombre el archivos
    awd$filename <- awdfile
    
    # --- ahora si, listo  :) ---------- #
    return(awd)
}
