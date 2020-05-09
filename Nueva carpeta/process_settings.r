## ----- Lectura del archvio de settings <<- al global environment directo ----- #
getset <- function(){
    currentdir <- getwd()
    setfile <- file.path(currentdir, "settings.lab")

    # Leer el archivo 
    file <- readLines(setfile)
    
    leer <- function(var = NULL){
        temp <- file[grep(var, file)]
        temp <- str_split(temp, "=", simplify = TRUE)
        temp <- temp[1,2] 
        temp <- lubridate::hm(temp)
        return(temp)
    }
    
    # Inicio de la noche
    ininoc <- leer("ininoc")
    # ininoc <<- leer("ininoc")
    
    # Inicio dle dia
    inidia <- leer("inidia")
    # inidia <<- leer("inidia")

       
    # Primer sue?o y vigilia
    dursleep <- leer("dursleep")
    dursleep <- lubridate::minute(dursleep)
    # dursleep <<- minute(dursleep)
    
    durawake <- leer("durawake")
    durawake <- lubridate::minute(durawake)
    # durawake <<- minute(durawake)

        
    # Duracion del cambio estado
    statedur <- leer("statedur")
    statedur <- lubridate::period_to_seconds(statedur)
    # statedur <<- period_to_seconds(statedur)

    # Sensibilidad minimitter
    sensivar <- file[grep("sensi", file)]
    sensivar <- str_split(sensivar, "=", simplify = TRUE)
    sensivar <- sensivar[1,2]
    sensivar <- as.numeric(sensivar)
    # sensivar <<- as.numeric(sensivar)
    
    set <- list(ininoc=ininoc, inidia=inidia, dursleep=dursleep, 
                durawake=durawake, statedur = statedur, sensivar=sensivar)
    return(set)
    
}
