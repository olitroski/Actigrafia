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
awdfile <- "2086-313-404 BJI Baseline.AWD"

# Determinar S|W y estado actigrafico
acv <- create.acv(awdfile, sensi = set$sensivar)
View(acv)

# Cortar en semi periodos de dia y noche
semiper <- create.semiper(acv)
names(semiper)
head(semiper$d1)

# Crear el primer filtro
filter.stats <- create.firstfilter(awdfile, semiper)
filter.stats

# create.acvedit, que 
acvedit <- create.acvedit(awdfile, acv, filter.stats)
names(acvedit)

# Crea el acveditRDS que combina filtro y acv
acveditRDS <- check.acvfilter(sub(".AWD", "", awdfile))
names(acveditRDS)

# Actograma
windows()
create.actogram(acveditRDS$semiper)










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

