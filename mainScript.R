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

# Cargar el directorio guardado, para el app shiny
# awdfolder <- load.savedir(mainfolder)

# Cargar App
# runApp(launch.browser = TRUE)

# stop()


# ----- Para pruebas -------------------------------------------------- 
# awdfolder <- "D:/OneDrive/INTA/Patricio Peirano/2019.12 Kansas/kansas"
# # sensi <- 40
# setwd(awdfolder)
# archivos <- dir()
# archivos <- archivos[grep(".[Aa][Ww][Dd]$", archivos)]
# 
# # Toda la vuelta del primer procesado
# awdfile <- archivos[1]
# acv <- create.acv(awdfile, set$sensivar)
# semiper <- create.semiper(awdfile, acv)
# filter.stats <- create.firstfilter(awdfile, semiper)
# acv.edit <- create.acvedit(awdfile, acv, filter.stats)
# 
# windows()
# create.actogram(awdfile)
# dev.off()

