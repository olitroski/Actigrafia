# --------------------------------------------------------------------------- #
# ----- Comandos de inicio de la app ---------------------------------------- #
# --------------------------------------------------------------------------- #
rm(list=ls())

# --- 1. Primero descargar el version.file ---------------------------------- #
vf <- readLines("https://raw.githubusercontent.com/olitroski/Actigrafia/master/version.file")
vf <- vf[grep("current", vf)]
vf <- strsplit(vf, ";", fixed = TRUE)
version <- vf[[1]][2]
versurl <- vf[[1]][3]
rm(vf)


# --- 2. wd al user folder para ver si existe la app ------------------------ #
usrfolder <- Sys.getenv("USERPROFILE")
setwd(usrfolder)
app <- dir.exists("app_sueno")


# --- 3. Si hay app captura la version, checa si la misma que github -------- #
if (app == TRUE){
    # Leer la versión del local app
    setwd(file.path(usrfolder, "app_sueno"))
    localv <- readLines("version.file")
    localv <- localv[grep("current", localv)]
    localv <- strsplit(localv, ";", fixed = TRUE)
    localv <- localv[[1]][2]
    
    # Si es la misma anota
    if (version == localv){version <- TRUE} else {version <- FALSE}
    rm(localv)
} 


# --- 4. Acciones dependiendo del status de la app y su version
# No está la app -> descargar
if (app == FALSE){
    # Descargamos ahi el zip
    setwd(usrfolder)
    download.file(url = versurl, destfile = file.path(usrfolder, "app_sueno.zip"))
    unzip("app_sueno.zip", exdir = "app_sueno") 
    file.remove("app_sueno.zip")
    
    # Crear el bat en el escritorio
    bat <- paste("R CMD BATCH '", file.path(usrfolder, "app_sueno/exe_control.R'"), sep = "")
    bat <- c(bat, "pause")
    batf <- paste(usrfolder, "\\Desktop\\holi.bat", sep = "")
    writeLines(text = bat, batf)
    rm(bat, batf)
    
    
# Está la app, pero no es la versión correcta
} else if (app == TRUE & version == FALSE){
    # Borramos la carpeta primero (unlink)
    unlink(file.path(usrfolder, "app_sueno"), force = TRUE, recursive = TRUE)
    
    # Y descargamos e instalamos
    setwd(usrfolder)
    download.file(url = versurl, destfile = file.path(usrfolder, "app_sueno.zip"))
    unzip("app_sueno.zip", exdir = "app_sueno") 
    file.remove("app_sueno.zip")
    
    # Crear el bat en el escritorio
    bat <- paste("R CMD BATCH '", file.path(usrfolder, "app_sueno/exe_control.R'"), sep = "")
    bat <- c(bat, "pause")
    batf <- paste(usrfolder, "\\Desktop\\holi.bat", sep = "")
    writeLines(text = bat, batf)
    rm(bat, batf)
    
# Está la app y es la última versión
} else if (app == TRUE & version == TRUE){
    # Todo Ok, ejecuta la app
    file.path(usrfolder, "app_sueno/appdeprueba.R")

# Si no hay nada de esto es error
} else {
    stop("Algo pasó... para resetear borrar la carpeta 'app_sueno' de la carpeta del usuario")
}










