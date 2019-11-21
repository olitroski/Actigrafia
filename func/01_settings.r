## ----- Lectura del archvio de settings <<- al global environment directo ----- #
getset <- function(){

    # Leer el wd para capturar el archivo
    library(stringr)
    library(lubridate)
    currentdir <- getwd()
    setfile <- file.path(currentdir, "parametros.set")

    # Leer el archivo 
    file <- readLines(setfile)
    
    leer <- function(var = NULL){
        temp <- file[grep(var, file)]
        temp <- str_split(temp, "=", simplify = TRUE)
        temp <- temp[1,2] 
        temp <- hm(temp)
        return(temp)
    }
    
    # Inicios 
    ininoc <<- leer("ininoc")
    inidia <<- leer("inidia")
       
    # Primer sueño y vigilia
    dursleep <- leer("dursleep")
    dursleep <<- minute(dursleep)
    
    durawake <- leer("durawake")
    durawake <<- minute(durawake)
    
    # Duracion del cambio estado
    statedur <- leer("statedur")
    statedur <<- period_to_seconds(statedur)

    # Sensibilidad minimitter
    sensivar <- file[grep("sensi", file)]
    sensivar <- str_split(sensivar, "=", simplify = TRUE)
    sensivar <- sensivar[1,2]
    sensivar <<- as.numeric(sensivar)
}
