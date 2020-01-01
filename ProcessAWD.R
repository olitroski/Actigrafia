# ----------------------------------------------------------------------------- #
# ----- Main Script para procesar archivos de actigrafía - Nov.2019 ----------- #
# ----- Lab. Sueño - INTA - U.Chile - O.Rojas - oliver.rojas.b@gmail.com ------ #
# ----------------------------------------------------------------------------- #
rm(list=ls())

# ----- Cargar funciones, settings -------------------------------------------- #
# Folder del programa
mainfolder <- "D:/OneDrive/INTA/Actigrafia"
setwd(mainfolder)
source("function_loader.R")

# Cargar funciones mias y settings
function_loader(dir = "func_awd")
function_loader(dir = "func_acti")
getset()     # <<<<"configuracion.set">>>>


# ----- Cargar el file name -------------------------------------------------- #
# Cargar carpeta de trabajo donde estan los awd
# awdfolder <- file.choose()
# awdfolder <- dirname(awdfolder)
# awdfolder <- "D:/OneDrive/INTA/Actigrafia/testfolder"
# sensi <- 40
awdfolder <- "D:/OneDrive/INTA/Patricio Peirano/2019.12 Kansas/kansas"

# Cargar solo los awd
setwd(awdfolder)
archivos <- dir()
archivos <- archivos[grep(".[Aa][Ww][Dd]$", archivos)]

# ----- Procesamiento de datos ----------------------------------------------- #

# Crear el epi
awdfile <- archivos[1]
acv <- create.acv(awdfile, sensivar)
epi <- create.epi(acv)
create.actogram(epi, acv, awdfile)


stop()
View(acv)








# para pruebas
stop()
setwd(file.path(mainfolder, "testfolder"))
archivo <- "BenjaminVenegas.AWD"
prename <- paste(sub(".[Aa][Ww][Dd]", "", archivo))








