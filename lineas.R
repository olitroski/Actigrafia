rm(list = ls())
script <- NULL
lineas <- NULL

setwd("D:/OneDrive/INTA/Actigrafia/funciones")
archivos <- dir()
archivos <- archivos[grep(".R", archivos)]
for (file in archivos){
    temp <- readLines(file)
    script <- c(script, file)
    lineas <- c(lineas, length(temp))
}


setwd("D:/OneDrive/INTA/Actigrafia")
archivos <- dir()
archivos <- archivos[grep(".R", archivos)]
for (file in archivos){
    temp <- readLines(file)
    script <- c(script, file)
    lineas <- c(lineas, length(temp))
}


data <- data.frame(script = script, lineas = lineas)
sum(data$lineas)

