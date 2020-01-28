## ------------------------------------------------------------------------------------ #
## ----- Plot para cada periodo ------------------------------------------------------- #
## ------------------------------------------------------------------------------------ #
# Toma un data.frame de la lista "semiper" y con eso hace el gráfico
create.plotSimple <- function(gdata, pct.y = 1, limites = NULL, lw = 1){
    # ---- Data para los ejes --------------------------------------------------------- #
    # # Hora decimal continua.... se pasó al "check.acvfilter.R"
    # lim <- as.numeric(set$ininoc)/3600
    # semiperdf <- mutate(semiperdf, xscale = ifelse(hrdec < lim,  hrdec + 24, hrdec))
    
    # X: Escala y etuquetas
    xscale <- seq(as.numeric(set$ininoc)/3600, length.out = 25)
    xlabel <- ifelse(xscale >= 48, xscale - 48, ifelse(xscale >= 24, xscale - 24, xscale))
    
    # Y: Lineas al inicio, dia, y fin
    ylinea <- as.numeric(c(set$ininoc, set$inidia + hours(24), set$ininoc + hours(24)))/3600
    
    # Y: Limites 
    limY <-  c(0, ceiling(max(gdata()$act.edit)/10)*10)     # experimento 
    limY[2] <- limY[2] * pct.y
    
    # Limite en X
    if (class(limites) == "NULL"){
        limX <- c(min(xscale), max(xscale))
    } else {
        limX <- limites
    }

    # --- sueño y wake data para el background (indices) ------------------------------ 
    sdata <- find.segment(gdata(), st.edit, "S")
    wdata <- find.segment(gdata(), st.edit, "W")
    
    # Si no se hace esto queda un gap entre el sueño y el dia que se ve en el plot autmentado
    # PUede que se pasen asi que hay que restar también
    sdata <- mutate(sdata, fin = fin + 1)
    wdata <- mutate(wdata, fin = fin + 1)
    
    if ((nrow(gdata()) + 1) == sdata$fin[nrow(sdata)]){
        sdata$fin[nrow(sdata)] <- nrow(gdata())
    } else {
        wdata$fin[nrow(wdata)] <- nrow(gdata())
    }

    
    # Solución por si no hay nada de sueño o vigilia (agrega 1 epoch al final)
    n <- nrow(gdata())
    if (nrow(wdata) == 0 & sdata[nrow(sdata),2] == n){
        sdata[nrow(sdata), 2] <- sdata[nrow(sdata), 2] - 1
        wdata[1,1] <- sdata[nrow(sdata), 2]
        wdata[1,2] <- sdata[nrow(sdata), 2]
    } else if (nrow(sdata) == 0 & wdata[nrow(wdata),2] == n){
        wdata[nrow(wdata), 2] <- wdata[nrow(wdata), 2] - 1
        sdata[1,1] <- wdata[nrow(wdata), 2]
        sdata[1,2] <- wdata[nrow(wdata), 2]    
    }

    # Sueño y wake data para el background (valores)
    sdata <- mutate(sdata, ini = gdata()$xscale[ini], fin = gdata()$xscale[fin])
    wdata <- mutate(wdata, ini = gdata()$xscale[ini], fin = gdata()$xscale[fin])

    
    # ---- Filtros y ediciones --------------------------------------------------------
    # test 
    # gdata()$filter[1400:1440] <- 1
    # gdata()$filter[ 300:400] <- 2
    # gdata()$filter[1000:1150] <- 3
    
    # El filtro que retira semi.periodos completos
    fdata <- find.segment(gdata(), filter, 1)
    if (nrow(fdata) > 0){
        fdata <- mutate(fdata, ini = gdata()$xscale[ini], fin = gdata()$xscale[fin])
    }
    
    # Filtro para wake to sleep = 2
    f2sleep <- find.segment(gdata(), filter, 2)
    if (nrow(f2sleep) > 0){
        f2sleep <- mutate(f2sleep, ini = gdata()$xscale[ini], fin = gdata()$xscale[fin])
    }
    
    # Filtro para sleep to wake = 3
    f2wake <- find.segment(gdata(), filter, 3)
    if (nrow(f2wake) > 0){
        f2wake <- mutate(f2wake, ini = gdata()$xscale[ini], fin = gdata()$xscale[fin])
    }    
    
    
    # ----- Grafico ------------------------------------------------------------------------------
    # Los colores para ponerlos con alpha en rgb <<col2rgb("skyblue3", alpha = 0.5)/255>>
    # Plot en blanco con Margenes nulos
    par(mar=c(2,2,0,2) + 0.5, xaxs = 'i', yaxs = 'i')
    plot(gdata()$xscale, gdata()$act.edit, type='n', ylab='', axes=FALSE, xlim=limX, ylim=limY)
    
    # <<SLEEP>>: Los indicadores de sueño  <<  col2rgb("skyblue3", alpha = 0.5)/255  >>
    for (i in 1:nrow(sdata)){
        rect(sdata$ini[i], 0, sdata$fin[i], limY[2], col = rgb(0.4235,0.6510,0.8039,0.5), border = "skyblue3")
    }
    
    # <<WAKE>>: Los indicadores de vigilia <<  col2rgb("gold2", alpha = 0.5)/255  >>
    for (i in 1:nrow(wdata)){
        rect(wdata$ini[i], 0, wdata$fin[i], limY[2], col = rgb(0.9333,0.7882,0.0000,0.5), border = "gold2")
    }
    
    # FILTRO: indicadores para el filtro de dia o noche completo
    if (nrow(fdata) > 0){
        for (i in 1:nrow(fdata)){
            rect(fdata$ini[i], 0, fdata$fin[i], limY[2], col = "red", border = "red")
        }
    }
    
    # 2 WAKE --- Muestra las modifiaciones desde sueño a vigilia
    if (nrow(f2wake) > 0){
        for (i in 1:nrow(f2wake)){
            rect(f2wake$ini[i], limY[2] - 5, f2wake$fin[i], limY[2], col = rgb(0.9333,0.7882,0.0000,1), border = "black")
        }
    }
    
    # 2 SLEEP --- Muestral los modificaciones desde vigilia a sueño
    if (nrow(f2sleep) > 0){
        for (i in 1:nrow(f2sleep)){
            rect(f2sleep$ini[i], limY[2] - 5, f2sleep$fin[i], limY[2], col = rgb(0.4235,0.6510,0.8039,1), border = "black")
        }
    }    
    
    # Añadir nuevo grafico encima
    par(new=TRUE)
    plot(gdata()$xscale, gdata()$act.edit, type='h', lwd = lw, col='grey20', ylab='', axes=FALSE, xlim=limX, ylim=limY)

    # Agrega más detalls
    abline(v = ylinea, col = "red")
    title(ylab = (format(date(gdata()$time[1]), "%A %d, %m--%Y")), line = 0.5)
    axis(side = 1, at = xscale, labels = xlabel)
    box()
   
    # Etiqueta en Y
    mtext(format(date(gdata()$time[nrow(gdata())]), "%A %d, %m--%Y"), side = 4, line = 0.5)

}
