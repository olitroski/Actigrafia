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
acv <- create.acv(awdfile, sensi = set$sensivar)
head(acv)

# Cortar en semi periodos de dia y noche
semiper <- create.semiper(acv)
names(semiper)

# Crear el primer filtro
filter.stats <- create.firstfilter(awdfile, semiper)
filter.stats

# create.acvedit, que integra el primer filtro al acv
acvedit <- create.acvedit(awdfile, acv, filter.stats)
names(acvedit)
tail(acvedit)

# Crea el acveditRDS que combina filtro y ACV para utilizar
acveditRDS <- check.acvfilter(sub(".AWD", "", awdfile))
names(acveditRDS)
names(acveditRDS$semiper)
acveditRDS$timelist


# Actograma
windows()
create.actogram(acveditRDS$semiper)



tail(acv)
tail(acvedit)

View(acv$filter == acvedit$filter)









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

