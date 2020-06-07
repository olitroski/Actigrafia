#' @title Leer Settings
#' @description Evalua si existe o no un archivo de settings, sino crea uno y lo carga en el globalenv.
#' @param awdpath el directorio seleccionado en la app que debiera ser el awdfolder() o algo asi.
#' @return una lista con los parametros
#' @export
#' @importFrom stringr str_split

## ----- Lectura del archvio de settings <<- al global environment directo ----- #
getset <- function(awdpath){
    # awdpath <- "D:/OneDrive/INTA/Actigrafia/testfolder"
    setwd(awdpath)
    
    # Ruta del settings
    setpath <- file.path(awdpath, "settings.sleep")
    
    # Funcion para leer elementos tipo hora
    leer <- function(var = NULL){
        temp <- file[grep(var, file)]
        temp <- str_split(temp, "=", simplify = TRUE)
        temp <- temp[1,2]
        temp <- lubridate::hm(temp)
        return(temp)
    }
    
    
    
    # --- Si es que existe lo carga como el original ----------------
    if (file.exists(setpath) == TRUE){
        # Leer el archivo
        file <- readLines(setpath)
        
        # Inicio de la noche
        ininoc <- leer("ininoc")
        
        # Inicio dle dia
        inidia <- leer("inidia")
        
        # Primer sueno y vigilia
        dursleep <- leer("dursleep")
        dursleep <- lubridate::minute(dursleep)
        
        durawake <- leer("durawake")
        durawake <- lubridate::minute(durawake)
        
        # Duracion del cambio estado
        statedur <- leer("statedur")
        statedur <- lubridate::period_to_seconds(statedur)
        
        # Sensibilidad minimitter
        sensivar <- file[grep("sensi", file)]
        sensivar <- str_split(sensivar, "=", simplify = TRUE)
        sensivar <- sensivar[1,2]
        sensivar <- as.numeric(sensivar)
        
        set <- list(ininoc=ininoc, inidia=inidia, dursleep=dursleep,
                    durawake=durawake, statedur = statedur, sensivar=sensivar)
        return(set)
        
        
        
    # --- Si no existe hay que crearlo ------------------------------
    } else {
        # Crear vector de lines 
        newset <- c(
            "Valores de configuracion del sistema, modificar la linea correspondiente.",
            " ",
            "---- Parametros de Sleep ---------------------",
            "Inicio noche: Hora a partir del cual comienza a buscar noche en formato 'hh:mm'",
            "ininoc = 20:00",
            " ",
            "Primer sueno: Tiempo minimo de sueno para determinar que comienza la noche en formato 'hh:mm'. ",
            "Para replicar resultados de actividorm deber ser (minutos - 1) porque ese programa tiene un error.",
            "dursleep = 00:30",
            " ",
            " ",
            "---- Parametros de la Vigilia -------------------",
            "Inicio dia: Hora a partir de la cual comienza a buscar el inicio del dia en formato 'hh:mm'",
            "inidia = 06:00",
            " ",
            "Primera vigilia: Tiempo minimo de vigilia para determinar que comienza el dia en formato 'hh:mm'.",
            "durawake = 00:30",
            " ",
            " ", 
            "---- Parametros Adicionales ---------------------",
            "Cambio de estado: Cantidad minima de minutos para determinar un cambio de estado de sueno o vigilia. Formato 'hh:mm'",
            "statedur = 00:05",
            " ",
            "Sensibilidad: Valor para la deteccion de sueno o vigilia, puede ser: 20, 40, 80.",
            "sensi = 20",
            " ",
            " ",
            " "
        )
        
        # Guardar
        writeLines(newset, setpath)
        
        # El resto lo copio del anterior, me da lata hacerlo mas rapido.
        # Leer el archivo
        file <- readLines(setpath)
        
        # Inicio de la noche
        ininoc <- leer("ininoc")
        
        # Inicio dle dia
        inidia <- leer("inidia")
        
        # Primer sueno y vigilia
        dursleep <- leer("dursleep")
        dursleep <- lubridate::minute(dursleep)
        
        durawake <- leer("durawake")
        durawake <- lubridate::minute(durawake)
        
        # Duracion del cambio estado
        statedur <- leer("statedur")
        statedur <- lubridate::period_to_seconds(statedur)
        
        # Sensibilidad minimitter
        sensivar <- file[grep("sensi", file)]
        sensivar <- str_split(sensivar, "=", simplify = TRUE)
        sensivar <- sensivar[1,2]
        sensivar <- as.numeric(sensivar)
        
        set <- list(ininoc=ininoc, inidia=inidia, dursleep=dursleep,
                    durawake=durawake, statedur = statedur, sensivar=sensivar)
        return(set)
    }
}

# Probarlo
# stop()
# set <- getset("D:/OneDrive/INTA/Actigrafia/testfolder")
# set
# rm(set)
