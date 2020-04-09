## ------------------------------------------------------------------------------- #
## ----- Plot para cada periodo -------------------------------------------------- #
## ------------------------------------------------------------------------------- #
# Toma un data.frame de la lista "semiper" y con eso hace el gráfico para actograma
create.plotActo <- function(gdata, pct.y = 1){
    # ---- Data para los ejes ---------------------------------------------------- #
    # Hora decimal continua
    lim <- as.numeric(set$ininoc)/3600
    gdata <- mutate(gdata, xscale = ifelse(hrdec < lim,  hrdec + 24, hrdec))

    # X: Escala y etuquetas
    xscale <- seq(as.numeric(set$ininoc)/3600, length.out = 25)
    xlabel <- ifelse(xscale >= 48, xscale - 48, 
                     ifelse(xscale >= 24, xscale - 24, xscale))
    
    # Y: Lineas al inicio, dia, y fin
    ylinea <- as.numeric(c(set$ininoc, set$inidia + hours(24), 
                           set$ininoc + hours(24)))/3600
    
    # Y: Limites  1100 porque si no mas
    if (max(gdata$act.edit) > 0){
        limY <- c(0, ceiling(max(gdata$act.edit)/10)*10)
        limY[2] <- limY[2] * pct.y
    } else {
        limY <- c(0, 1100)
        limY[2] <- limY[2] * pct.y
    }
    
    # X: Limites
    limX <- c(min(xscale), max(xscale))
    
    
    # --- sueño y wake data para el background (indices) -------------------------- #
    sdata <- find.segment(gdata, "st.edit", "S")
    wdata <- find.segment(gdata, "st.edit", "W")
    
    # Solución por si no hay nada de sueño o vigilia 
    # (agrega 1 epoch falso al final) quitando un epoch al que tiene datos
    if (nrow(wdata) == 0){statusW <- FALSE} else {statusW <- TRUE}
    if (nrow(sdata) == 0){statusS <- FALSE} else {statusS <- TRUE}

    if (statusW == FALSE){    # No hay vigilia, solo sueño
        wdata[1,"ini"] <- sdata[nrow(sdata), "fin"]
        wdata[1,"fin"] <- sdata[nrow(sdata), "fin"]
        sdata[nrow(sdata), "fin"] <- sdata[nrow(sdata), 2] - 1
    } 
    if (statusS == FALSE){    # No hay sueño, solo vigilia
        sdata[1,"ini"] <- wdata[nrow(wdata), "fin"]
        sdata[1,"fin"] <- wdata[nrow(wdata), "fin"]    
        wdata[nrow(wdata), "fin"] <- wdata[nrow(wdata), "fin"] - 1
    }
    
    # Queda un gap entre sueño y viglia porque queda 1 minuto (intervalo) blanco
    # se agrega un minuto al final
    n <- nrow(gdata)
    sdata <- mutate(sdata, fin = fin + 1)
    wdata <- mutate(wdata, fin = fin + 1)
    
    # Dada la corrección se excede el último intervalo en S o W
    if (sdata$fin[nrow(sdata)] == (n + 1)){
        sdata$fin[nrow(sdata)] <- nrow(gdata)
    } else if (wdata$fin[nrow(wdata)] == (n + 1)){
        wdata$fin[nrow(wdata)] <- nrow(gdata)
    } else {
        stop("Error en la corrección intervalos adyacentes")
    }

    # Sueño y wake data para el background (valores)
    sdata <- mutate(sdata, ini = gdata$xscale[ini], fin = gdata$xscale[fin])
    wdata <- mutate(wdata, ini = gdata$xscale[ini], fin = gdata$xscale[fin])

    
    # ---- Filtros y ediciones ---------------------------------------------------- #
    # El filtro de inicio
    fdata <- find.segment(gdata, filter, 1)
    if (nrow(fdata) > 0){
        fdata <- mutate(fdata, ini = gdata$xscale[ini], 
                        fin = gdata$xscale[fin])
    }
    
    # Filtro para periodos desde la app
    f2sleep <- find.segment(gdata, filter, 2)
    if (nrow(f2sleep) > 0){
        f2sleep <- mutate(f2sleep, ini = gdata$xscale[ini], 
                          fin = gdata$xscale[fin])
    }
    
    # Filtro para sleep to wake = 3
    f2wake <- find.segment(gdata, filter, 3)
    if (nrow(f2wake) > 0){
        f2wake <- mutate(f2wake, ini = gdata$xscale[ini], 
                         fin = gdata$xscale[fin])
    }    
    
    
    # ----- Grafico ----------------------------------------------------------------
    # Plot en blanco con Margenes nulos
    par(mar=c(0,3,0,2) + 0.5, xaxs = 'i', yaxs = 'i')
    plot(gdata$xscale, gdata$act.edit, type = 'n', ylab = '', 
         axes = FALSE, xlim = limX, ylim = limY)
    
    # <<SLEEP>>: Los indicadores de sueño  <<col2rgb("skyblue3", alpha = 0.5)/255>>
    for (i in 1:nrow(sdata)){
        rect(sdata$ini[i], 0, sdata$fin[i], limY[2], 
             col = rgb(0.4235,0.6510,0.8039,0.5), border = "skyblue3")
    }
    
    # <<WAKE>>: Los indicadores de vigilia <<col2rgb("gold2", alpha = 0.5)/255>>
    for (i in 1:nrow(wdata)){
        rect(wdata$ini[i], 0, wdata$fin[i], limY[2], 
             col = rgb(0.9333,0.7882,0.0000,0.5), border = "gold2")
    }
    
    # FILTRO: indicadores para el filtro de dia o noche completo
    if (nrow(fdata) > 0){
        for (i in 1:nrow(fdata)){
            rect(fdata$ini[i], 0, fdata$fin[i], limY[2], 
                 col = "red", border = "red")
        }
    }
    
    # 2 WAKE --- Muestra las modifiaciones desde sueño a vigilia
    if (nrow(f2wake) > 0){
        for (i in 1:nrow(f2wake)){
            rect(f2wake$ini[i], limY[2] - 10, f2wake$fin[i], limY[2], 
                 col = "red", border = "red")
        }
    }
    
    # 2 SLEEP --- Muestral los modificaciones desde vigilia a sueño
    if (nrow(f2sleep) > 0){
        for (i in 1:nrow(f2sleep)){
            rect(f2sleep$ini[i], 0, f2sleep$fin[i], limY[2], 
                 col = "orchid", border = "red")
        }
    }    
    
    # Añadir nuevo grafico encima: Los datos de actividad
    par(new=TRUE)
    plot(gdata$xscale, gdata$act.edit, type='h', 
         col='grey20', ylab='', axes=FALSE, xlim=limX, ylim=limY)
    
    # Agrega más detalls
    abline(v = ylinea, col = "red")
    
    yfec <- paste(format(date(gdata$time[1]), "%a"),
                  format(date(gdata$time[1]), "%d-%m"), sep = "\n")
    mtext(yfec, side = 2, las = 1, line = 0.5, cex = 0.8)

    box()
}
