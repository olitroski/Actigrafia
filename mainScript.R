# ----------------------------------------------------------------------------- #
# ----- Main Script para procesar archivos de actigrafía - Nov.2019 ----------- #
# ----- Lab. Sueño - INTA - U.Chile - O.Rojas - oliver.rojas.b@gmail.com ------ #
# ----------------------------------------------------------------------------- #
# ----- Cargar funciones, settings --------------------------------------------
rm(list = ls())
mainfolder <- "D:/OneDrive/INTA/Actigrafia"
setwd(mainfolder)
source("appLoader.R")

# Cargar App
# runApp(launch.browser = TRUE)


stop()




# --- Pruebas ----------------------------------------------------------------------
# # Cargar un sujeto valido
awdfolder <- "D:/OneDrive/INTA/Actigrafia/testfolder/test_kansas"
setwd(awdfolder)
archivos <- dir()
archivos <- archivos[grep(".[Aa][Ww][Dd]$", archivos)]
awdfile <- "2058-010-310 NYU Visit3"
awdfile <- str_replace(awdfile, ".AWD", "")

# ----- Para probar un epi ------------------------------------------
semiper <- check.acvfilter(awdfile)
library(tictoc)
tic(); test <- create.epi(semiper); toc()

rm(archivos, awdfolder)
stop()
# semiperdf <- semiper$per00
# View(semiperdf)
# otable("st.edit", data = semiperdf)


# ----- Para probar gráficos ----------------------------------------
semiper <- check.acvfilter(awdfile)
semiper <- semiper$semiper
semiperdf <- semiper$per01
gdata <- semiperdf

create.plotActo(gdata)

# View(semiperdf)
# otable("st.edit", data = semiperdf)

# ----- Probar y trabajar el EPI ------------------------------------
setwd("D:/OneDrive/INTA/Actigrafia/testfolder/test_kansas")
acv <- check.acvfilter("2058-001-368 JRG Baseline")
per <- "per00 - martes 15/07/14"
stagesTable(acv, per)

