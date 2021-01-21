#' @title Crea el primer ACV
#' @description ACV es Activity, esta funcion toma el raw data y hace la primera pasada de computos. Crea la secuencia, quita los marcadores M, aplica el algoritmo del minimitter y el estado actigrafico suavizado segun el valor de consolidacion de estado. Hace varios recortes que ya documentare.
#' @param awdfile es el string del archivo awd (el original)
#' @param set El objeto de settings
#' @param finalizar Default en False, es pera guardar un acv original con mas data al cerrar el sujeto
#' @return Un data frame con toda la data necesaria para comenzar el proceso
#' @export
#' @examples
#' # act <- create.acv("2086-308-045 CHT Visit2.AWD", sensi = set$sensivar)
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom lubridate seconds
#' @importFrom lubridate minute
#' @importFrom lubridate dmy_hm
#' @importFrom stats quantile
#' @importFrom stringr str_split

create.acv <- function(awdfile, set, finalizar = FALSE){
    esp <- fec <- act3 <- index <- nombre <- dec <- acti2 <- st.edit <- NULL
    newfec <- N <- newact <- NULL
    minimo <- act.raw <-act.smooth <- hrdec <- NULL
	cat(paste0("| Iniciando Archivo: ", awdfile, "\n"))

	# ----------------------------------------------------------------------------------- #
	# ----- Cargar datos ---------------------------------------------------------------- #
	# ----------------------------------------------------------------------------------- #
    awd <- readLines(awdfile, warn = FALSE)
    awd.head <- awd[1:7]
    awd.data <- awd[-(1:7)]

    # Corrige si encuentra M's
    if (length(grep("M", awd.data)) > 0){
        M <- grep("M", awd.data)
        awd.data[M] <- sub("M", "", awd.data[M])
        rm(M)
    }
    awd.data <- as.numeric(awd.data)

    # Revisar que solo hayan numeros desde la posicion 8 en adelante
    numtest <- as.numeric(awd.data)
    if (sum(is.na(numtest)) > 0){
        stop("Hay un valor no numerico en el AWD a partir linea 8")
    }
    rm(numtest, awd)

    
    # ----------------------------------------------------------------------------------- #
    # ----- Antecedentes del header  ---------------------------------------------------- #
    # ----------------------------------------------------------------------------------- #
    ## Procesar los antecedentes del header
    # Variables originales
    name <- awd.head[1]
    date <- awd.head[2]
    hour <- awd.head[3]
    epoch.len <- as.numeric(awd.head[4])
    # edad <- awd.head[5]
    # serie <- awd.head[6]
    # sexo <- awd.head[7]

    # Identificar el epoch, si 4 = 1 minuto, si 1 = 15 segundos, 2 = 30 secs
    if (epoch.len == 1){
        epoch.len <- 15
    } else if (epoch.len == 2) {
        epoch.len <- 30
    } else if (epoch.len == 4){
        epoch.len <- 60
    } else {
        stop("error, algo paso con el epoch len")
    }
    
	# cat(paste("|--- cAcv: Sens = ", sensi, " - Samp = ", epoch.len, "secs \n", sep = ""))
    
    # Crear fecha-hora inicial y Cambiar tambien el mes a ingles "se trabaja en string"
    fec.ini <- paste(date, hour)
    mes <- data.frame(eng = as.character(c(1:12)),
        esp = c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic"),
        stringsAsFactors = FALSE)
    mes <- filter(mes, esp == unlist(str_split(fec.ini, "-", 3))[2])
    
    # Asume que esta en espanol, se corrige si ingles.
    if (nrow(mes) > 0){
        fec.ini <- str_replace(fec.ini, mes[1,2], mes[1,1])
        fec.ini <- dmy_hm(fec.ini)
    } else {
        fec.ini <- dmy_hm(fec.ini)
    }

    # --- Crear Secuencia de fechas (Crea el AWD) ------------------------------------- #
    fec.ini <- fec.ini + seconds(seq(0, by = epoch.len, length.out = length(awd.data)))
    awd <- data.frame(nombre = name, act = awd.data, fec = fec.ini, stringsAsFactors = FALSE)

    # Recorte de peaks de actividad al percentil 98
    awd <- mutate(awd, act2 = ifelse(act > 0, act, NA))
    p98 <- quantile(awd$act2, 0.98, na.rm = TRUE)                                       # >>>> el centile <<<<
    awd <- mutate(awd, act3 = ifelse(act < p98, act, p98), act3 = round(act3, 0))
    awd <- ordervar(awd, c("act2", "act3"), after = "act")
    
    # Hora decimal
    awd <- mutate(awd, dec = hour(fec)+minute(fec)/60)
    rm(fec.ini, awd.data, p98, mes, date, hour)
    

    # --------------------------------------------------------------------------------- #
    # ----- Algoritmo del Actiwatch - MiniMitter -------------------------------------- #
    # --------------------------------------------------------------------------------- #
	cat("|--- cAcv: Algorithm Minimitter -Start-\n")

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
        stop("En la seleccion del multiplicador")
    }

    # Algoritmo de deteccion
	sensi <- set$sensivar
    fin <- nrow(awd)
    act <- awd$act3
    acti.st <- rep(NA, times = fin)
    
    for (i in 1:fin){                                                                           # xx
        # Crear el vector de indices y extraerlo desde el de actividad, con ajuste inicio y fin
        indx <- c(i - pre, i, i + pos)
        
        temp1 <- which(indx < 1)
        indx[indx < 1] <- 0

        temp2 <- which(indx > fin)
        indx[indx > fin] <- 0
        
        # Secuencia de actividad y multiplicador
        epoch.data <- act[indx]     
            
        if (length(temp1) > 0){
            A <- sum(multi[-temp1] * epoch.data)
        } else if (length(temp2) > 0){
            # A <- sum(multi[-temp2] * epoch.data)
            A <- A  # copia el A anterior pa mantener estado
        } else {
            A <- sum(multi * epoch.data)
        }
        
        # Decisiones segun la sensibilidad
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

    rm(fin, act, acti.st, pre, pos, multi, i, indx, epoch.data, A, temp1, temp2, sensi)

    
    # ----------------------------------------------------------------------------------- #
    # ---- Calcular ahora el estado actigrafico corregido ------------------------------- #
    # ----------------------------------------------------------------------------------- #
	cat("|--- cAcv: Estado Actigrafico -Start-\n")
    # Con la regla de que para que cambie debe pasar 5 (statdur) minutos
    # Calcular cuantas lineas tiene el statedur (duracion para cambio de estado)
    tdiff <- difftime(awd$fec[2], awd$fec[1], units = "secs")
    tdiff <- as.numeric(tdiff)
    tdiff <- set$statedur / tdiff

    # Buscar el "STATE inicial" como los primeros "statedur" (5) epoch iguales
    # pueden entonces quedar algunos epoch iniciales sin estado corregido.
    ws <- "NotOK"
    i <- 1
    while (ws != "Ok"){
        # Captura el primer STATE y ve cuantos S|W hay hasta que encuentra 5
        stXmin <- awd$acti[i:(i + tdiff - 1)]
        nW <- length(which(stXmin == "W"))
        nS <- length(which(stXmin == "S"))

        # Logica para detener
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
    # if (i == 1){
    #     i <- 2
    # }
    if (i > 1){
        temp.ini <- 1:(i-1)
    }

    # Listo, ahora a contar desde i con "obj::state" como inicial
    acti <- awd$acti
    acti.fix <- rep(1:length(acti))     # de aqui sale el acti2[1]=1
    for (x in i:(nrow(awd) - tdiff + 1)){
        # Capturar el trozo
        stXmin <- acti[x:(x + tdiff - 1)]
        
        # Contar el STATE
        if (length(which(stXmin == state)) > 0){
            acti.fix[x] <- state
        } else if (length(which(stXmin == state)) == 0){
            # Cambio estado ejem si state == W y se encuentra S mas adelante
            # W W W W W[S S S S S]S S S S W W W W W
            #           x
            state <- acti[x]      # El de mini mitter
            acti.fix[x] <- state
        }
    }

    # Arreglar el final
    acti.fix[(nrow(awd) - tdiff + 2):nrow(awd)] <- state
    awd$acti2 <- acti.fix
    awd$acti2[temp.ini] <- NA 

    rm(i, nS, nW, state, stXmin, tdiff, ws, x, acti.fix, acti, temp.ini)

    # ----------------------------------------------------------------------------------- #
    # ---- Ordenar y terminar  ---------------------------------------------------------- #
    # ----------------------------------------------------------------------------------- #
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

    # <<<< Dejarlo en minutos >>>>>
    if (finalizar == FALSE){
        # Pasar a minutos y usar pa sumar actividad
        awd <- dplyr::mutate(awd, newfec = format(time, format = "%Y-%m-%d %H:%M"))
        awd <- dplyr::group_by(awd, newfec)
        
        # Sacar los minutos incompletos y etiquetar minimos
        awd <- mutate(awd, N = n()) %>% filter(N == 4)
        awd <- mutate(awd, minimo = min(time), minimo = ifelse(minimo == time, 1, 0))
        
        # Calcular el resto de las variables
        awd <- mutate(awd, 
                      act.raw = sum(act.raw),
                      act.smooth = sum(act.smooth),
                      time = min(time),
                      hrdec = min(hrdec),
                      act.edit = act.smooth)
                      
        # Filtrar y terminar
        awd <- as.data.frame(awd)
        awd <- filter(awd, minimo == 1) %>% select(-N, -minimo, -newfec)
        awd <- mutate(awd, indx = 1:nrow(awd))
        write.table(awd, "clipboard-16384", sep="\t", row.names=FALSE)
        
        return(awd)        

    } else {
        # --- ahora si, listo  :) ---------- #
        return(awd)
    }
}
