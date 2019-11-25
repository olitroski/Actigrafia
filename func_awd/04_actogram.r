## ------------------------------------------------------------------------------------ #
## ----- Actograma con ggplot --------------------------------------------------------- #
## ------------------------------------------------------------------------------------ #
# La idea es tomar el epi y awd creado por mis funciones y crear una base de datos nueva
# para hacer actogramas, de momento guardar cada actograma en png no más
# Ejemplo
okeep(c("okeep", "omerge", "ordervar"))
awd <- readRDS("BenjaminVenegas_acv.rds")
epi <- readRDS("BenjaminVenegas_epi.rds")


##----- Grafico ---------------------------------------------------------------#
actogram.per <- function(gdata = NULL){
    # ---- Datos grafico -----
    # Escala de X 
    xscale <- gdata$dec.seq
    xscale <- floor(xscale)
    xscale <- unique(xscale)
    xscale <- c(xscale, max(xscale)+1)

    # Labels (horas de 0 a 24)
    xlabel <- NULL
    for (i in xscale){
        if (i >= 48){
            xlabel <- c(xlabel, i - 48)
        } else if (i >= 24) {
            xlabel <- c(xlabel, i - 24)
        } else {
            xlabel <- c(xlabel, i)
        }
    }

    # Lineas al inicio, dia, y fin
    ylinea <- c(gdata$dec.seq[1],
                max(gdata$dec.seq[gdata$dianoc == "Noche"]),
                gdata$dec.seq[nrow(gdata)])

    # sueño data para el background
    sdata <- filter(gdata, acti2 == "S")
    sdata <- group_by(sdata, stage)
    sdata <- as.data.frame(summarize(sdata, min = min(dec.seq), max = max(dec.seq)))

    # Limites de Y
    limY <-  c(0, ceiling(max(gdata$act3)/100)*100)
    limX <- c(min(xscale), max(xscale))

    # ----- Grafico base ------
    # Los colores para ponerlos con alpha en rgb <<col2rgb("skyblue", alpha = 0.5)/255>>
    par(mar=c(2,2,0,2) + 0.5, xaxs = 'i', yaxs = 'i')
    plot(gdata$dec.seq, gdata$act3, type='n', ylab='', axes=FALSE, xlim=limX, ylim=limY)
    for (i in 1:nrow(sdata)){
        rect(sdata$min[i], 0, sdata$max[i], limY[2], col = rgb(0.529,0.808, 0.922,0.5), border = "skyblue")
    }
    par(new=TRUE)
    plot(gdata$dec.seq, gdata$act3, type='h', col='grey20', ylab='', axes=FALSE, xlim=limX, ylim=limY)
    abline(v = ylinea, col = "red")
    title(ylab = (format(date(gdata$fec[1]), "%a%d, %b%y")), line = 0.5)
    axis(side = 1, at = xscale, labels = xlabel)
    box()
    # mtext(format(date(gdata$fec[nrow(gdata)]), "%A %d, %m--%Y"), side = 4, line = 0.5)
    mtext(paste(format(date(gdata$fec[nrow(gdata)]), "%a%d"), ", P=", gdata$nper[1], sep = ""), side = 4, line = 0.5)

}

stop()

##----- Crear todos los graficos ----------------------------------------------#
# Con la función del grafico por día que se hagan todos los graficos
create.actogram <- function(epi = NULL, awd = NULL){
    # Separar el período
    epi <- cbind(epi, 
                 data.frame(str_split(epi$periodo, " ", simplify = TRUE), 
                            stringsAsFactors = FALSE))
    epi <- rename(epi, dianoc = X1, nper = X2)

    # Merge epi-awd
    epi <- select(epi, stage, duracion, periodo, dianoc, nper)
    awd <- omerge(awd, epi, byvar = "stage", keep = TRUE)       # Se va el primer dia
    awd <- awd$match
    awd <- arrange(awd, index) %>% select(-merge, -act2)

    # Crear la secuencia continua de horas decimales -----
    # Con hora de inicio en dec, se hace una secuencia según el epoch
    temp <- NULL
    for (per in unique(awd$nper)){        
         per = "05"
        gdata <- filter(awd, nper == per) %>% arrange(fec) 
        
        # Hora decimal continua desde el inicio
        desde <- gdata$dec[1]
        hasta <- nrow(gdata)
        epoch <- gdata$dec[2] - gdata$dec[1]
        secuencia <- seq(from = desde, by = epoch, length.out = hasta)
        gdata$dec.seq <- secuencia
        temp <- rbind(temp, gdata)     # <<<< considerar sacar esto
        
        # La parte de los graficos
        gfile <- paste(per, ".png", sep = "")
        png(gfile, width = 1000, height = 180, units = "px")
        actogram.per(gdata)
        dev.off()
    }    
    awd <- temp
}
