# ----------------------------------------------------------------------------- #
# ----- Main Script para procesar archivos de actigrafia - Nov.2019 ----------- #
# ----- Lab. Sueño - INTA - U.Chile - O.Rojas - oliver.rojas.b@gmail.com ------ #
# ----------------------------------------------------------------------------- #
rm(list=ls())

# ----- Cargar funciones, settings --------------------------------------------
# Folder del programa
mainfolder <- "D:/OneDrive/INTA/Actigrafia"
setwd(mainfolder)

# Cargar funciones y objetos
source("function_loader.R")
lapply(X=dir()[grep("func_", dir())], FUN=function_loader)
load.library()          # librerias
set <- getset()         # <<<<"configuracion.set">>>>

# Cargar App
# runApp(launch.browser = TRUE)
stop()




# --- Pruebas --------------------------------------------------------------------
# # Cargar un sujeto valido
awdfolder <- "D:/OneDrive/INTA/Actigrafia/testfolder/test_kansas"
setwd(awdfolder)
archivos <- dir()
archivos <- archivos[grep(".[Aa][Ww][Dd]$", archivos)]
awdfile <- archivos[1]
awdfile <- str_replace(awdfile, ".AWD", "")
rm(archivos)

# Cargar data para un grafico
gdata <- check.acvfilter(awdfile)
gdata <- gdata$semiper
gdata <- gdata$per01


names(gdata)
lapply(gdata, function(x) min(x$time))
otable("filter", data = gdata)


windows()
create.plotActo(gdata)





        



