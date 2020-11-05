# ----------------------------------------------------------------------------- #
# ----- Script para pruebas de las salidas y pasos en general ----------------- #
# ----- No es para producción    === PARA PRUEBAS ===         ----------------- #
# ----------------------------------------------------------------------------- #

# ----- Settings para kansas  -------------------------------------------------
# SleepSearchTime (hora a partir del cual comienza a buscar noche) y el de día: 20:00 / 6:00
# SleepDuration (tiempo minimo para determinar que comineza la noche) y de vigilia: 1800 / 1800
# StateFilterDuration (tiempo para cambio de estado): 300
# Sensibilidad: 20.0


# ----- Cargar carpeta de trabajo --------------------------------------------- #
# Actualizar el Package
# mainfolder <- "D:/OneDrive/INTA/Actigrafia"
# setwd(mainfolder)
# source("D:/OneDrive/INTA/Actigrafia/compiApp.R")

# Espacio de trabajo
rm(list = ls())
mainfolder <- "D:/OneDrive/INTA/Actigrafia"
setwd(mainfolder)


# ----- Cargar todo para hacer pruebas ---------------------------------------- #
# Cargar y/o Instalar librerias
# Listar lo instalado
packlist <- c("utf8", "sourcetools","tidyselect","fastmap","xtable", "httpuv","zip",
              "backports","assertthat","tibble","pkgconfig","R6", "kableExtra", "Hmisc",
              "openxlsx", "fs", "shinyFiles","shiny","rmarkdown","haven","stringr",
              "purrr","lubridate","dplyr")
new.packages <- packlist[!(packlist %in% installed.packages()[,"Package"])]

# Instalar si no estan
if (length(new.packages) > 0) {
    install.packages(new.packages) 
}
# Cargar
for (lib in packlist) {
    eval(parse(text = paste0("library(",lib,")")))
}
# limpiar
rm(packlist, new.packages, lib)


# | -- Cargar funciones al Gloval Env --------------------------------------- #
# Cargar el folder
cat("---Cargando funciones---\n")
setwd(file.path(mainfolder, "funciones"))
funciones <- dir()[grep("[.Rr]$", dir())]
# Cargar
for (fun in funciones) {
    print(paste("Loading function", fun))
    source(fun)
}    
# Limpiar
rm(fun, funciones)
setwd(mainfolder)

# ----- Ejecutar la aplicacion ---------------------------------------------- # 
# Cargar App
# runApp(display.mode = "normal")
stop()




# --- Analizar un sujeto ------------------------------------------------------
# Cargar un sujeto valido
setwd("D:/OneDrive/INTA/Actigrafia/testfolder")
awdfile <- "2058-001-368 JRG Baseline.AWD"
set <- getset(getwd())


# --- Analisis inicial de un sujeto -------------------------------------------
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



# --- Revisar create.plotActo -------------------------------------------------
setwd("D:/OneDrive/INTA/Actigrafia/testfolder")
set <- getset(getwd())
acveditRDS <- check.acvfilter("2058-001-368 JRG Baseline.AWD", set)
filterRDS <- readRDS("2058-001-368 JRG Baseline.edit.RDS")

# Revisar acvedit
names(acveditRDS)
acveditRDS$timelist
acveditRDS <- acveditRDS$semiper

# Revisar filtros
filterRDS$filter

# Crear un plot simple para actograma
gdata <- acveditRDS[["per00"]]
create.plotActo(gdata, set, filterRDS)


# Tamaño
w <- 1400
h <- (length(acveditRDS) * 110 + 220) * 1.5
png("plot.png", width = w, height = h, pointsize = 20)
create.actogram(semiper = acveditRDS, set = set, filterRDS = filterRDS)
dev.off()




# --- revisar archivos RDS ---------------------------------------------------
setwd("D:/OneDrive/INTA/Actigrafia/testfolder")
awd <- c("2058-001-368 JRG Baseline", "2058-002-298 MLR Baseline", "2058-004-433 AJM Baseline",
         "2058-007-464 JMH Baseline", "2058-009-577 KNC Baseline", "2058-012-833 DFW Visit3")
set <- getset(getwd())

i <- 2
acv <- readRDS(paste0(awd[i], ".acvedit.RDS"))
head(acv)
tail(acv)






# --- Construcción del EPI ----------------------------------------------------
setwd("D:/OneDrive/INTA/Actigrafia/testfolder")
set <- getset(getwd())
awd <- c("2058-001-368 JRG Baseline", "2058-002-298 MLR Baseline", "2058-004-433 AJM Baseline",
         "2058-007-464 JMH Baseline", "2058-009-577 KNC Baseline", "2058-012-833 DFW Visit3")

# Probar
acvedit <- check.acvfilter(paste0(awd[1], ".AWD"), set)
filter <-  readRDS(paste0(awd[1], ".edit.RDS"))
epi <- create.epi(acvedit, filter, set)
epi <- epi$epiviejo

# library(microbenchmark)
# microbenchmark(create.epi(acvedit, filter, set), times = 50)
# 
# # Probar en loop
# for (i in 1:length(awd)){
#     # Datos necesarios
#     # i <- 1
#     print(i)
#     set <- getset(getwd())
#     acvedit <- check.acvfilter(paste0(awd[i], ".AWD"), set)
#     filter <-  readRDS(paste0(awd[i], ".edit.RDS"))
# 
#     # Lista la funcion
#     epi <- create.epi(acvedit, filter, set)
# }
# 
# # Revisar contenido
# names(epi)
# head(epi$epi)

# Cargar funciones de analisis
source("D:/OneDrive/INTA/AplicacionesVarias/Stats EPI - ARQ/00_function_check.epidata.R")
source("D:/OneDrive/INTA/AplicacionesVarias/Stats EPI - ARQ/00_function_expand.epidata.R")
source("D:/OneDrive/INTA/AplicacionesVarias/Stats EPI - ARQ/01_function_readfolder_epi.R")
source("D:/OneDrive/INTA/AplicacionesVarias/Stats EPI - ARQ/02_function_readfolder_arq.R")
source("D:/OneDrive/INTA/AplicacionesVarias/Stats EPI - ARQ/04_function_ValidEvents.R")
source("D:/OneDrive/INTA/AplicacionesVarias/Stats EPI - ARQ/05_function_hi.R")
source("D:/OneDrive/INTA/AplicacionesVarias/Stats EPI - ARQ/06_function_conteo.R")
source("D:/OneDrive/INTA/AplicacionesVarias/Stats EPI - ARQ/07_function_duration.R")
source("D:/OneDrive/INTA/AplicacionesVarias/Stats EPI - ARQ/08_function_maxdur.R")
source("D:/OneDrive/INTA/AplicacionesVarias/Stats EPI - ARQ/09_function_latencia.R")
source("D:/OneDrive/INTA/AplicacionesVarias/Stats EPI - ARQ/10_function_24h.R")
source("D:/OneDrive/INTA/AplicacionesVarias/Stats EPI - ARQ/11_function_combi24h.R")


# Pre-procesar
epi <- function_ValidEvents(epi)                # 1
drop <- epi$drop
epi <- epi$datos

# Validar que se puede analizar
epi <- select(epi, -actividad)
check.epidata(epi)                              # 2

# Pasar las funciones
horaini <- function_hi(epi)                     # 3
conteo <- function_conteo(epi)                  # 4
duracion <- function_duration(epi)              # 5
maximos <- function_duracionMax(epi)            # 6
latencia <- function_latencia(epi)              # 7
CausaEfecto <- function_combi24h(epi)           # 8


# NO pasa si es NULL
par24horas <- function_24h(epi)                 # 9
drop <- bind_rows(drop, par24horas$sinpar)
par24horas <- par24horas$conpar





# --- Revisar la secuencia de detección ---------------------------------------
setwd("D:/OneDrive/INTA/Actigrafia/testfolder")
awdfile <- "2058-001-368 JRG Baseline.AWD"
dir()
set <- getset(getwd())
acv <- create.acv(awdfile, set)
head(acv)

semiper <- create.semiper(acv, set)
names(semiper)
head(semiper$d0)

filter.stats <- create.firstfilter(awdfile, semiper)
readRDS("2058-001-368 JRG Baseline.edit.RDS")

acv.edit <- create.acvedit(awdfile, acv, filter.stats)




# --- Tabla de estados ---------------------------------------------------------
setwd("D:/OneDrive/INTA/Actigrafia/testfolder")
set <- set <- getset(getwd())
acvedit <- check.acvfilter("2058-001-368 JRG Baseline", set)
names(acvedit$semiper)
filter <- readRDS("2058-001-368 JRG Baseline.edit.RDS")
str(filter)

# input$perChoose
periodos <- acvedit$timelist
periodos <- paste(periodos$period, "-", periodos$tlist)

# tabla
tablaEstados <- stagesTable(acvedit, "per00")

# Lo que se muestra
data <- filter(tablaEstados, estado == "S")
data <- c(paste(data$inicio, "-", data$termino))


# Mostrar la duracion
input <- data[4] # Elije el 4
data <- str_split(input, "-", simplify = TRUE)
data <- dmy_hm(data[1])
data <- format(data,  format = "%d-%m-%Y %H:%M")
data <- filter(tablaEstados(), inicio == data)
cat(data$duracion)
