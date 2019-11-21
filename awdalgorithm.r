# ------------------------------------------------------------------------------------- #
# ----- Replica del algoritmo del actividom --- 24.09.2019 v1.0 ----------------------- #
# ------------------------------------------------------------------------------------- #
# Antecedentes
rm(list=ls())
source("D:/OneDrive/GitHub/sources/exploratory/local_sources.r")
setwd("D:/OneDrive/INTA/Actigrafia")
library(tictoc)
source("settings.r")
dir()
# stop()


## ------------------------------------------------------------------------------------ #
## ----- Actograma con ggplot --------------------------------------------------------- #
## ------------------------------------------------------------------------------------ #
# Ejemplo
epifile <- readRDS("epi.rds")
awdfile <- readRDS("awd.rds")

create.actogram <- function(epifile = NULL, awdfile = NULL){}
    epi <- epifile
    awd <- awdfile

    # Trajinando el epi primero
    epi <- cbind(epi, data.frame(str_split(epi$periodo, " ", simplify = TRUE), stringsAsFactors = FALSE))
    epi <- rename(epi, dianoc = X1, nper = X2)

    # Merge epi - awd
    epi <- select(epi, stage, duracion, periodo, dianoc, nper)
    awd <- omerge(awd, epi, byvar = "stage", keep = TRUE)       # Se va el primer dia
    awd <- awd$match
    awd <- arrange(awd, index) %>% select(-merge, -act2, -half)

    # Corregir hora inicio de cada etapa al formato continuo de hora
    temp <- NULL
    for (per in unique(awd$nper)){        # 'fec' per = "03"
        per.data <- filter(awd, nper == per) %>% arrange(fec) 
        hrstart <- per.data$fec[1]
        
        # Necesito el ininoc con fecha
        if (hour(hrstart) < ininoc){
            temp.date <- (date(hrstart) - 1) + ininoc
        } else {
            temp.date <- date(hrstart)
        }
        
        # Cuanta diferencia entre la fecha y el temp.date
        per.data <- mutate(per.data, delta = difftime(fec, temp.date, units = 'hours'))
        per.data <- mutate(per.data, hrdec = as.numeric(20 + delta)) %>% select(-delta)    # <<<<<< arreglar esto
        temp <- rbind(temp, per.data)
    }
    awd <- temp


    ## ----- Grafico ------------------------------------------------------------------ #
    per <- "03"
    gdata <- filter(awd, nper == per) %>% arrange(fec) 
    head(gdata)
    nrow(gdata) /5

    # Escala de X 
    temp <- gdata$hrdec
    temp <- floor(temp)
    temp <- unique(temp)
    temp <- c(temp, max(temp)+1)

    # Labels (horas)
    glab <- NULL
    for (i in temp){
        if (i >= 48){
            glab <- c(glab, i - 48)
        } else if (i >= 24) {
            glab <- c(glab, i - 24)
        } else {
            glab <- c(glab, i)
        }
    }

    # Lineas al inicio, dia, y fin
    ylinea <- c(gdata$hrdec[1],
                max(gdata$hrdec[gdata$dianoc == "Noche"]),
                gdata$hrdec[nrow(gdata)])

    # sueño data para el background
    sdata <- filter(gdata, acti2 == "S")
    sdata <- group_by(sdata, stage)
    sdata <- as.data.frame(summarize(sdata, min = min(hrdec), max = max(hrdec)))

    # Limites de Y
    limY <-  c(0, ceiling(max(gdata$act3)/100)*100)
    limX <- c(min(temp), max(temp))

    # Actograma de 1 periodo completo
    g <- ggplot(data = gdata, aes(x = hrdec, y = act3)) 
    g <- g + scale_y_continuous(limits = limY, expand = c(0, 0, 0.05, 0))
    g <- g + scale_x_continuous(labels = glab, breaks = temp, limits = c(min(temp), max(temp)), expand = c(0.01, 0))
        
    for (i in 1:nrow(sdata)){
        g <- g + annotate("rect", fill = "skyblue1", alpha = 0.5,
            xmin = sdata$min[i], xmax = sdata$max[i], 
            ymin = -Inf, ymax = Inf)
    }
    
    g <- g + geom_area() + geom_vline(xintercept = ylinea, colour = "red")
    g <- g + ylab(format(date(gdata$fec[1]), "%a %d-%b-%Y")) + xlab(NULL)

    
    
    # A probar en la versión base

    par(mar=c(2,2,0,2) + 0.5, xaxs = 'i', yaxs = 'i')
    plot(gdata$hrdec, gdata$act3, type='n', ylab='', axes=FALSE, xlim=limX, ylim=limY)
    for (i in 1:nrow(sdata)){
        rect(sdata$min[i], 0, sdata$max[i], limY[2], col = alpha("skyblue", 0.5), border = "skyblue")
    }
    par(new=TRUE)
    plot(gdata$hrdec, gdata$act3, type='h', col='grey20', ylab='', axes=FALSE, xlim=limX, ylim=limY)
    abline(v = ylinea, col = "red")
    title(ylab = (format(date(gdata$fec[1]), "%A %d, %m--%Y")), line = 0.5)
    axis(side = 1, at = temp, labels = glab)
    box()
    mtext(format(date(gdata$fec[nrow(gdata)]), "%A %d, %m--%Y"), side = 4, line = 0.5)
    

    
     
#     png('actogram.png', width = 1200, height = 230, units = "px" )
#     dev.off()

    ??col
    
