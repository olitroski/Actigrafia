## ------------------------------------------------------------------------------------ #
## ----- Actograma con ggplot --------------------------------------------------------- #
## ------------------------------------------------------------------------------------ #
# La idea es tomar el epi y awd creado por mis funciones y crear una base de datos nueva
# para hacer actogramas, de momento guardar cada actograma en png no más


##----- Grafico ---------------------------------------------------------------#
plot.period <- function(gdata = NULL){
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
    
    # Limites de dia o noche o ambos
    dianocdata <- group_by(gdata, dianoc)
    dianocdata <- as.data.frame(summarize(dianocdata, min = min(dec.seq), max = max(dec.seq)))
    

    # ----- Grafico ------------------------------------------------------------------------------
    # Los colores para ponerlos con alpha en rgb <<col2rgb("skyblue", alpha = 0.5)/255>>
    
    # Plot en blanco con Margenes nulos
    par(mar=c(2,2,0,2) + 0.5, xaxs = 'i', yaxs = 'i')
    plot(gdata$dec.seq, gdata$act3, type='n', ylab='', axes=FALSE, xlim=limX, ylim=limY)
    
    # Los indicadores de dia noche    
    day <- filter(dianocdata, dianoc == "Dia")
    noc <- filter(dianocdata, dianoc == "Noche")
    if (nrow(day) == 1){
        rect(day$min[1], 0, day$max[1], limY[2], col = rgb(1,1,0,0.3), border = "yellow")
    }
    if (nrow(noc) == 1){
        rect(noc$min[1], 0, noc$max[1], limY[2], col = "grey90", border = "grey90")
    }
    
    # Los indicadores de sueño  steelblue3  rgb(0.5294,0.8078,0.9216,0.5)
    for (i in 1:nrow(sdata)){
        rect(sdata$min[i], 0, sdata$max[i], limY[2], col = "turquoise1", border = "turquoise1")
    }
    
    # Añadir nuevo grafico encima
    par(new=TRUE)
    plot(gdata$dec.seq, gdata$act3, type='h', col='grey20', ylab='', axes=FALSE, xlim=limX, ylim=limY)
    
    # Agrega más detalls
    abline(v = ylinea, col = "red")
    title(ylab = (format(date(gdata$fec[1]), "%a%d, %b%y")), line = 0.5)
    axis(side = 1, at = xscale, labels = xlabel)
    box()
    # mtext(format(date(gdata$fec[nrow(gdata)]), "%A %d, %m--%Y"), side = 4, line = 0.5)
    mtext(paste(format(date(gdata$fec[nrow(gdata)]), "%a%d"), ", P=", gdata$nper[1], sep = ""), side = 4, line = 0.5)

}


##----- Crear todos los graficos ----------------------------------------------#
# Con la función del grafico por día que se hagan todos los graficos
create.actogram <- function(epi = NULL, awd = NULL, awdfile = NULL){
   
    # Separar el período
    epi <- cbind(epi, data.frame(str_split(epi$periodo, " ", simplify = TRUE), 
                      stringsAsFactors = FALSE))
    epi <- rename(epi, dianoc = X1, nper = X2)

    # Merge epi-awd
    epi <- select(epi, stage, duracion, periodo, dianoc, nper)
    awd <- omerge(awd, epi, byvar = "stage", keep = TRUE, output = FALSE)       # Se va el primer dia
    awd <- awd$match
    awd <- arrange(awd, index) %>% select(-merge, -act2)

    # Crear la secuencia continua de horas decimales -----
    # Con hora de inicio en dec, se hace una secuencia según el epoch
    temp <- list()
    for (per in unique(awd$nper)){           # per = "01"
        gdata <- filter(awd, nper == per) %>% arrange(fec) 
        
        # Hora decimal continua desde el inicio
        desde <- gdata$dec[1]
        hasta <- nrow(gdata)
        epoch <- gdata$dec[2] - gdata$dec[1]
        secuencia <- seq(from = desde, by = epoch, length.out = hasta)
        gdata$dec.seq <- secuencia
        
        # La parte de los graficos
        gfile <- paste(per, ".png", sep = "")
        png(gfile, width = 1000, height = 180, units = "px")
            plot.period(gdata)
        dev.off()
        
        # Guardar los datos de cada gdata
#         temp[[per]] <- gdata
    }    
    saveRDS(temp, "gdata.rds")

    ## Apilar los graficos
    # Las fotos
    fotos <- dir()
    fotos <- fotos[grep(".png", fotos)]
    fotos <- sort(fotos, decreasing = TRUE)
    imgs <- NULL
    for (f in fotos){
        temp <- image_read(f)
        imgs <- c(temp, imgs)
    }
    
    img.apilada <- image_append(imgs, stack = TRUE)
    prename <- sub(".[Aa][Ww][Dd]", "", awdfile)
    actoname <- paste(prename, "_acto.png", sep = "")
    image_write(img.apilada, path = actoname, format = "png")
    
    # Zip archivos
    zip.files <- list.files(pattern = ".png$")
    zip.files <- append(zip.files, "gdata.rds")
    zip::zipr(zipfile = paste(prename, "_acto.zip", sep = ""), files = zip.files)
    for (f in zip.files){file.remove(f)}
}
