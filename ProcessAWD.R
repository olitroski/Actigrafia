# ----------------------------------------------------------------------------- #
# ----- Main Script para procesar archivos de actigrafía - Nov.2019 ----------- #
# ----- Lab. Sueño - INTA - U.Chile - O.Rojas - oliver.rojas.b@gmail.com ------ #
# ----------------------------------------------------------------------------- #
library(stringr)
progfolder <- "D:/OneDrive/INTA/Actigrafia"

# Cargar carpeta de trabajo donde estan los awd
awdfolder <- file.choose()
awdfolder <- dirname(awdfolder)

# Cargar solo los awd
setwd(awdfolder)
archivos <- dir()
archivos <- archivos[grep(".[Aa][Ww][Dd]$", archivos)]

# Cargar las settings
setwd(progfolder)
source(file.path(progfolder, "func", "01_settings.r"))
getset()

# Crear el awd

