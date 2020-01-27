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

# stop()
# 
# 
# # ----- Para pruebas -------------------------------------------------- 
# sensi <- 40
awdfolder <- "D:/OneDrive/INTA/Actigrafia/testfolder/test_kansas"
setwd(awdfolder)
archivos <- dir()
archivos <- archivos[grep(".[Aa][Ww][Dd]$", archivos)]
awdfile <- archivos[1]
awdfile <- str_replace(awdfile, ".AWD", "")


# 
# # Toda la vuelta del primer procesado
# acv <- create.acv(awdfile, set$sensivar)
# semiper <- create.semiper(awdfile, acv)
# filter.stats <- create.firstfilter(awdfile, semiper)
# acv.edit <- create.acvedit(awdfile, acv, filter.stats)
# 
# semiper2 <- create.semiper(awdfile, acv.edit)
# create.actogram(semiper2)
# 
# 
# # Jugar con los tama?os
# setwd(mainfolder)
# png("ola k ase.png", width = 1000, height = 1320)
# create.actogram(semiper2)
# dev.off()
# 
# 
# 1320/11
# 
# # Cada plot suma 120 pixeles al actograma.
# 
# 





