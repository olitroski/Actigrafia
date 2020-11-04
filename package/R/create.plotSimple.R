#' @title Plot simple para periodo.
#' @description Hace un plot individual para un periodo en version simple para el actograma
#' @param gdata datos que saca el check.acvfilter
#' @param set Settings file
#' @param filterRDS lista del poll de los filtros en la app
#' @param pct.y Factor de multiplicacion para el eje y
#' @param limites Vector de limites para el eje x
#' @param lw Numeric de weight para las bandas del grafico
#' @return plot de un periodo
#' @export
#' @examples
#' # setwd("D:/OneDrive/INTA/Actigrafia/testfolder")
#' # set <- getset(getwd())
#' # acveditRDS <- check.acvfilter("2058-001-368 JRG Baseline.AWD", set)
#' # acveditRDS <- acveditRDS$semiper
#' # filterRDS <- readRDS("2058-001-368 JRG Baseline.edit.RDS")
#' # gdata <- acveditRDS[["per08"]]
#' # create.plotSimple(gdata, set, filterRDS)
#' @importFrom grDevices rgb
#' @import graphics

## ------------------------------------------------------------------------------- #
## ----- Plot para cada periodo -------------------------------------------------- #
## ------------------------------------------------------------------------------- #
# Toma un data.frame de la lista "semiper" y con eso hace el grafico
# pct.y = 1; limites = NULL; lw = 1

create.plotSimple <- function(gdata, set, filterRDS, pct.y = 1, limites = NULL, lw = 1){
    options(warn = 2)
    # nulos de paquete
    st.edit <- fin <- ini <- tipo <- NULL
    
    
    # ---- Inicio y fin de registro + filtro tipo 4 ------------------------------ #
    # Lo primero será capturar las fechas del filtro en formato del gráfico
    iniSubj <- filterRDS$header[4]
    iniSubj <- str_split(iniSubj, ": ", simplify = TRUE)[2]
    
    if (iniSubj == " -No determinado- "){
        iniSubj <- NA
    } else {
        iniSubj <- dmy_hm(iniSubj)
        if (iniSubj %in% gdata$time){
            iniSubj <- gdata$xscale[which(gdata$time == iniSubj)]
        } else {
            iniSubj <- NA
        }    
    }
    
    # Fin registro
    finSubj <- filterRDS$header[5]
    finSubj <- str_split(finSubj, ": ", simplify = TRUE)[2]
    
    if (finSubj == "-No determinado- "){
        finSubj <- NA
    } else {
        finSubj <- dmy_hm(finSubj) #+ minutes(1)
        if (finSubj %in% gdata$time){
            finSubj <- gdata$xscale[which(gdata$time == finSubj)]
        } else {
            finSubj <- NA
        }
    }

    # Filtro: Linea mover noche
    moverND <- dplyr::filter(filterRDS$filter, tipo == "Mover")
    if (nrow(moverND) > 0){
        moverND <- moverND[["ini"]]
        moverND <- dmy_hm(moverND)
        moverND <- moverND[moverND %in% gdata$time]
        moverND <- gdata$xscale[moverND == gdata$time]
    } else {
        moverND <- NA
    }
    
    
    # ---- Data para los ejes ----------------------------------------------------
    # X: Escala y Etiquetas
    xscale <- seq(as.numeric(set$ininoc)/3600, length.out = 25)
    xlabel <- ifelse(xscale >= 48, xscale - 48, ifelse(xscale >= 24, xscale - 24, xscale))
    
    # X: Lineas al inicio, ini-dia, y fin (solo es necesario el de la mitad)
    ylinea <- as.numeric(c(set$ininoc, set$inidia + hours(24), set$ininoc + hours(24))) / 3600
    ylinea[1] <- NA
    ylinea[3] <- NA

    # Y: Limites  1100 porque si no mas
    if (max(gdata$act.edit) > 0){
        chy <- (ceiling(max(gdata$act.edit)/10)*10) * pct.y
        limY <-  c(0, chy)
    } else {
        limY <- c(0, 1100)
    }

    # Limite en X
    if (class(limites) == "NULL"){
        limX <- c(min(xscale), max(xscale))
    } else {
        limX <- limites
    }

    
    # --- Colorear Sueno y Wake en background (ini|fin xscale) ------------------- #
    sdata <- find.segment(gdata, st.edit, "S")
    wdata <- find.segment(gdata, st.edit, "W")

    # Solucion por si no hay nada de sueno o vigilia
    # (agrega 1 epoch falso al final) quitando un epoch al que tiene datos
    if (nrow(wdata) == 0){statusW <- FALSE} else {statusW <- TRUE}
    if (nrow(sdata) == 0){statusS <- FALSE} else {statusS <- TRUE}

    if (statusW == FALSE){    # No hay vigilia, solo sueno
        wdata[1,"ini"] <- sdata[nrow(sdata), "fin"]
        wdata[1,"fin"] <- sdata[nrow(sdata), "fin"]
        sdata[nrow(sdata), "fin"] <- sdata[nrow(sdata), 2] - 1
    }
    if (statusS == FALSE){    # No hay sueno, solo vigilia
        sdata[1,"ini"] <- wdata[nrow(wdata), "fin"]
        sdata[1,"fin"] <- wdata[nrow(wdata), "fin"]
        wdata[nrow(wdata), "fin"] <- wdata[nrow(wdata), "fin"] - 1
    }

    # Queda un gap entre sueno y viglia porque queda 1 minuto (intervalo) blanco
    # se agrega un minuto al final
    n <- nrow(gdata)
    sdata <- mutate(sdata, fin = fin + 1)
    wdata <- mutate(wdata, fin = fin + 1)

    # Dada la correccion se excede el ultimo intervalo en S o W
    if (sdata$fin[nrow(sdata)] == (n + 1)){
        sdata$fin[nrow(sdata)] <- nrow(gdata)
    } else if (wdata$fin[nrow(wdata)] == (n + 1)){
        wdata$fin[nrow(wdata)] <- nrow(gdata)
    } else {
        stop("Error en la correccion intervalos adyacentes")
    }

    # Sueno y wake data para el background (valores)
    sdata <- mutate(sdata, ini = gdata$xscale[ini], fin = gdata$xscale[fin])
    wdata <- mutate(wdata, ini = gdata$xscale[ini], fin = gdata$xscale[fin])


    # ---- Coloreado de filtros -----------------------------------------------------
    # Filtro de tipo "Excluir"
    fdata <- find.segment(gdata, filter, "Excluir")
    if (nrow(fdata) > 0){
        fdata <- mutate(fdata, ini = gdata$xscale[ini], fin = gdata$xscale[fin])
    }

    # Filtro Inicio y termino del registro
    f2sleep <- rbind(find.segment(gdata, filter, "Ini"),
                     find.segment(gdata, filter, "Fin"))
    if (nrow(f2sleep) > 0){
        f2sleep <- mutate(f2sleep, ini = gdata$xscale[ini], fin = gdata$xscale[fin])
    }

    # Filtro agregar actividad
    f2wake <- find.segment(gdata, filter, "Actividad")
    if (nrow(f2wake) > 0){
        f2wake <- mutate(f2wake, ini = gdata$xscale[ini], fin = gdata$xscale[fin])
    }


    # ----- Grafico ----------------------------------------------------------------
    # Los colores para ponerlos con alpha en rgb
    # <<col2rgb("skyblue3", alpha = 0.5)/255>>

    # Plot en blanco con Margenes nulos
    par(mar=c(2,2,0,2) + 0.5, xaxs = 'i', yaxs = 'i')
    plot(gdata$xscale, gdata$act.edit, type='n', ylab='', axes=FALSE,
         xlim=limX, ylim=limY)

    # <<SLEEP>>: Los indicadores de sueno  <<col2rgb("skyblue3", alpha = 0.5)/255>>
    for (i in 1:nrow(sdata)){
        rect(sdata$ini[i], 0, sdata$fin[i], limY[2],
             col = rgb(0.4235,0.6510,0.8039,0.5), border = "skyblue3")
    }

    # <<WAKE>>: Los indicadores de vigilia <<col2rgb("gold2", alpha = 0.5)/255>>
    for (i in 1:nrow(wdata)){
        rect(wdata$ini[i], 0, wdata$fin[i], limY[2],
             col = rgb(0.9333,0.7882,0.0000,0.5), border = "gold2")
    }

    # FILTRO 1: indicadores para el filtro de dia o noche completo
    if (nrow(fdata) > 0){
        for (i in 1:nrow(fdata)){
            rect(fdata$ini[i], 0, fdata$fin[i], limY[2],
                 col = rgb(1, 0, 0, 0.3), border = rgb(1, 0, 0, 0.3))
        }
    }

    # FILTRO 2: Agregados en la app
    if (nrow(f2sleep) > 0){
        for (i in 1:nrow(f2sleep)){
            rect(f2sleep$ini[i], 0, f2sleep$fin[i], limY[2], col = rgb(0, 1, 0, 0.4), border = rgb(0, 1, 0, 0.3))
            # rect(f2sleep$ini[i], 0, f2sleep$fin[i], limY[2], col = rgb(0, 0.3921, 0, 0.3))
        }
    }
    
    # Modificar Actividad 3: Modifiaciones desde SLEEP -> WAKE
    if (nrow(f2wake) > 0){
        for (i in 1:nrow(f2wake)){
            rect(f2wake$ini[i], limY[2] - 30, f2wake$fin[i], limY[2],
                 col = "red", border = "red")
        }
    }


    # Anadir grafico nuevo encima
    par(new=TRUE)
    plot(gdata$xscale, gdata$act.edit, type='h', lwd = lw, col='grey20',
         ylab='', axes=FALSE, xlim=limX, ylim=limY)
    
    # Linea de incio dia
    abline(v = ylinea, col = "red", lwd = 1)
    
    # Linea de mover noche
    abline(v = moverND, col = "magenta", lwd = 2)
    
    # Linea de inicio y fin de período
    abline(v = iniSubj, col = "green4", lwd = 2)
    abline(v = finSubj, col = "green4", lwd = 2)

    # Agrega mas detalles
    title(ylab = (format(date(gdata$time[1]), "%A %d, %m--%Y")), line = 0.5)
    axis(side = 1, at = xscale, labels = xlabel)
    box()

    # Etiqueta en Y
    mtext(format(date(gdata$time[nrow(gdata)]), "%A %d, %m--%Y"), side = 4, line = 0.5)
}
