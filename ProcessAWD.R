# ----------------------------------------------------------------------------- #
# ----- Main Script para procesar archivos de actigrafía - Nov.2019 ----------- #
# ----- Lab. Sueño - INTA - U.Chile - O.Rojas - oliver.rojas.b@gmail.com ------ #
# ----------------------------------------------------------------------------- #
# Librerias
library(stringr)
library(dplyr)
library(lubridate)


# ----- Cargar funciones, settings -------------------------------------------- #
# Folder del programa
progfolder <- "D:/OneDrive/INTA/Actigrafia"

# Cargar funciones mias
source(file.path(progfolder, "func", "order.var.r"))

# Cargar las settings
setwd(progfolder)
source(file.path(progfolder, "func", "01_settings.r"))
getset()


# ----- Cargar el file name -------------------------------------------------- #
# Cargar carpeta de trabajo donde estan los awd
# awdfolder <- file.choose()
# awdfolder <- dirname(awdfolder)
# 
# # Cargar solo los awd
# setwd(awdfolder)
# archivo <- dir()
# archivo <- archivo[grep(".[Aa][Ww][Dd]$", archivo)]
setwd("D:/OneDrive/INTA/Actigrafia/testfolder")
archivo <- "BenjaminVenegas.AWD"

# ----- Procesamiento de datos ----------------------------------------------- #
# Crear el awd
source(file.path(progfolder, "func", "02_acv.r"))
awd <- create.acv(archivo, sensivar)

# Crear el epi
source(file.path(progfolder, "func", "03_epi.r"))
epi <- create.epi(awd)




stop()
rm(list=ls())
head(awd)
head(epi)






