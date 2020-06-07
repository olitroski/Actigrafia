# ----------------------------------------------------------------------------- #
# ----- Main Script para procesar archivos de actigrafía - Nov.2019 ----------- #
# ----- Lab. Sueño - INTA - U.Chile - O.Rojas - oliver.rojas.b@gmail.com ------ #
# ----------------------------------------------------------------------------- #
# ----- Settings para kansas  -------------------------------------------------
# SleepSearchTime (hora a partir del cual comienza a buscar noche) y el de día: 20:00 / 6:00
# SleepDuration (tiempo minimo para determinar que comineza la noche) y de vigilia: 1800 / 1800
# StateFilterDuration (tiempo para cambio de estado): 300
# Sensibilidad: 20.0
# ----- Cargar funciones, settings --------------------------------------------
rm(list = ls())
mainfolder <- "D:/OneDrive/INTA/Actigrafia"
setwd(mainfolder)
source("appLoader.R")

# Cargar App
# runApp(launch.browser = TRUE)
runApp(display.mode = "normal")
stop()




# --- Analisis inicial de un sujeto -------------------------------------------
# Cargar un sujeto valido
setwd("D:/OneDrive/INTA/Actigrafia/testfolder")
awdfile <- "2086-309-362 AJF Visit2.AWD"

# Determinar S|W y estado actigrafico
acv <- create.acv(awdfile, set)
head(acv)

# Cortar en semi periodos de dia y noche
semiper <- create.semiper(acv, set)
names(semiper)

# Crear el primer filtro
firstFilter <- create.firstfilter(awdfile, semiper)
firstFilter

# create.acvedit, que integra el primer filtro al acv
acvedit <- create.acvedit(awdfile, acv, firstFilter)
names(acvedit)
tail(acvedit)

# Crea el acveditRDS que combina filtro y ACV para utilizar
acveditRDS <- check.acvfilter(awdfile, set)
names(acveditRDS)
names(acveditRDS$semiper)
acveditRDS$timelist
stop()


# Actograma
windows()
create.actogram(acveditRDS$semiper)
View(acv$filter == acvedit$filter)


