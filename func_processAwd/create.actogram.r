## -------------------------------------------------------------------------------- #
## ----- Actograma con ggplot not ------------------------------------------------- #
## -------------------------------------------------------------------------------- #
# La idea es tomar un acv y mostrarlos de 20 a 20 horas asi aseguramos todos los d?as
create.actogram <- function(semiper, fy = 1){
    # Valores 
    xscale2 <- seq(as.numeric(set$ininoc)/3600, length.out = 25)
    xlabel2 <- ifelse(xscale2 >= 48, xscale2 - 48, 
                      ifelse(xscale2 >= 24, xscale2 - 24, xscale2))
    limX2 <- c(min(xscale2), max(xscale2))
    
    # Se saca toda la info desde la lista <semiper> filtros todo...   :)
    nplot <- length(semiper)
    par(mfrow = c(nplot + 2, 1))

    par(mar=c(2, 3, 0, 2) + 0.5 , xaxs = 'i', yaxs = 'i')
    plot(0, type = 'n', ann = FALSE, axes = FALSE, xlim = limX2)
    axis(side = 1, at = xscale2, labels = xlabel2)
    
    for (g in 1:nplot){
        create.plotActo(semiper[[g]], fy)
    }
    
    par(mar=c(0, 3, 2, 2) + 0.5)
    plot(0, type = 'n', ann = FALSE, axes = FALSE, xlim = limX2)
    axis(side = 3, at = xscale2, labels = xlabel2)

}

#create.actogram(awdfile)
