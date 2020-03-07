# ----------------------------------------------------------------------------- #
# ----- Main Script para procesar archivos de actigrafía - Nov.2019 ----------- #
# ----- Lab. Sueño - INTA - U.Chile - O.Rojas - oliver.rojas.b@gmail.com ------ #
# ----------------------------------------------------------------------------- #
rm(list = ls())

# ----- Cargar funciones, settings --------------------------------------------
# Folder del programa
mainfolder <- "D:/OneDrive/INTA/Actigrafia"
setwd(mainfolder)

# Cargar funciones y objetos
source("function_loader.R")
lapply(X = dir()[grep("func_", dir())], FUN = function_loader)
load.library()          # librerias
set <- getset()         # <<<<"configuracion.set">>>>

# Cargar App
stop()
runApp(launch.browser = TRUE)




# --- Pruebas ----------------------------------------------------------------------
# # Cargar un sujeto valido
awdfolder <- "D:/OneDrive/INTA/Actigrafia/testfolder/test_kansas"
setwd(awdfolder)
archivos <- dir()
archivos <- archivos[grep(".[Aa][Ww][Dd]$", archivos)]
awdfile <- "2058-010-310 NYU Visit3"
awdfile <- str_replace(awdfile, ".AWD", "")


# Cargar data para un grafico
semiper <- check.acvfilter(awdfile)
semiper <- semiper$semiper
names(semiper)
semiperdf <- semiper$per00
View(semiperdf)
otable("st.edit", data = semiperdf)


windows()
create.plotSimple(semiperdf)

windows()
create.plotActo(semiperdf)

windows()
create.actogram(semiper)


names(semiperdf)


        



