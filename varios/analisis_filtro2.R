# Test para evaluar una maquina que prediga el comportamiento de la actividad
# y algunas estadísticas para hacer un mejor filtro inicial en OlitoApp


# ----- Prueba de concepto ----------------------------------------------------
# Cargar un sujeto valido
library(aod)
library(ggplot2)
setwd("D:/OneDrive/INTA/Actigrafia/testfolder")
awdfile <- "2086-309-362 AJF Visit2.AWD"

# Funcion para sacar estadisticas de un semiperiodo
statdf <- function(semidf){
    # Captura la actividad y stage calcula media, sd
    ms <- mean(semidf$act.edit[semidf$st.edit == "S"])
    ss <-   sd(semidf$act.edit[semidf$st.edit == "S"])
    mw <- mean(semidf$act.edit[semidf$st.edit == "W"])
    sw <-   sd(semidf$act.edit[semidf$st.edit == "W"])
    
    pctS <- sum(semidf$st.edit == "S")/nrow(semidf)
    pctW <- sum(semidf$st.edit == "W")/nrow(semidf)
    
    fec <- semidf$time
    ini <- min(fec)
    fin <- max(fec)
    
    return(data.frame(meanS = ms, sdS = ss, meanW = mw, sdW = sw, pctS=pctS, pctW=pctW, ini = ini, fin = fin))
}

# Función para tomar un periodo y separar en 2 calcular stats
# El set ini dia está en "6H 0M 0S", asi que usamos "xscale" para separar en 
# semi periodos, sería > 30
procPer <- function(per){
    # Dividir en 2
    noc <- filter(per, xscale < 30)
    dia <- filter(per, xscale >= 30)
    
    # Estadisticas
    if (nrow(noc) > 0 & nrow(dia) > 0){
        filtro <- group_by(noc, filter) %>% summarize(n = n()) %>% filter(n == max(n)) %>% pull(filter)
        statNoc <- statdf(noc)
        statNoc <- mutate(statNoc, periodo = "Noche", filter = filtro) %>% select(-ini, -fin)
        
        filtro <- group_by(dia, filter) %>% summarize(n = n()) %>% filter(n == max(n)) %>% pull(filter)
        statDia <- statdf(dia)
        statDia <- mutate(statDia, periodo = "Dia", filter = filtro) %>% select(-ini, -fin)
        
        statData <- rbind(statNoc, statDia)
        
    } else if (nrow(noc) > 0 ){
        filtro <- group_by(noc, filter) %>% summarize(n = n()) %>% filter(n == max(n)) %>% pull(filter)
        statData <- statdf(noc)
        statData <- mutate(statData, periodo = "Noche", filter = filtro) %>% select(-ini, -fin)

    } else if (nrow(dia) > 0){
        filtro <- group_by(dia, filter) %>% summarize(n = n()) %>% filter(n == max(n)) %>% pull(filter)
        statData <- statdf(dia)
        statData <- mutate(statData, periodo = "Dia", filter = filtro) %>% select(-ini, -fin)
        
    } else {
        stop("Algo pasó")
    }
    
    return(statData)
}

# Los datos serán periodos completos con check.acvfilter
datos <- check.acvfilter(sub(".AWD", "", awdfile))
datos <- datos$semiper

# Juntar los datos
statData <- bind_rows(lapply(datos, procPer))

# Funció para procesar un archivo
procAWD <- function(awdfile){
    datos <- check.acvfilter(awdfile)$semiper
    statData <- lapply(datos, procPer)
    statData <- bind_rows(statData, .id = "PerName")
    statData <- mutate(statData, file = awdfile)
    return(statData)
}

rm(datos, statData, awdfile)

# ----- Capturar los datos ----------------------------------------------------
# Vector de valores
archivos <- dir()
archivos <- archivos[grep("finish", archivos)]
archivos <- str_replace(archivos, ".finish.RDS", "")

# Calcular la data
statData <- lapply(archivos, procAWD)
statData <- bind_rows(statData, .id = "num")
head(statData)

# Dejar una muestra balanceada entre NA y 2
nas <- filter(statData, is.na(filter))
dos <- filter(statData, filter == 2)
nas <- sample_n(nas, size = nrow(dos))

logdata <- bind_rows(nas, dos)
rm(dos, nas, statData)


# ----- Ejecutar un analisis logistico ----------------------------------------
# Cargar todo
head(logdata)
logdata <- mutate(logdata, filter = ifelse(is.na(filter), 0, 1))
logdata <- select(logdata, filter, meanS, sdS, pctS, meanW, sdW, pctW, periodo)
logdata <- mutate(logdata, nas = apply(logdata, 1, function(x) sum(is.na(x))))
logdata <- filter(logdata, nas == 0)
apply(logdata, 2, function(x) sum(is.na(x)))

write_dta(logdata, "logdata.dta", version = 13)



# Attempt
fit <- glm(filter ~ meanS + sdS + pctS + meanW + sdW + pctW, data = logdata, family = "binomial")
summary(fit)

fit <- glm(filter ~ meanS + sdS + pctS + meanW + sdW, data = logdata, family = "binomial")
summary(fit)

fit <- glm(filter ~ meanS + pctS + meanW + sdW, data = logdata, family = "binomial")
summary(fit)

fit <- glm(filter ~ pctS, data = logdata, family = "binomial")
summary(fit)




