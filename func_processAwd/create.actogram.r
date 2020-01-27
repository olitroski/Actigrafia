## ------------------------------------------------------------------------------------ #
## ----- Actograma con ggplot --------------------------------------------------------- #
## ------------------------------------------------------------------------------------ #
# La idea es tomar un acv y mostrarlos de 20 a 20 horas asi aseguramos tener todos los d?as
# Solo se superponen sue?o y vigilia, la idea es que sirva para editar el awd.
create.actogram <- function(semiper, fy = 1){
   
    # Combinar noche->dia mismo periodo
    per <- unique(str_sub(names(semiper), 2, 2))
    perlist <- list()
    for (p in per){
        i <- grep(paste(p, "$", sep = ""), names(semiper))
        if (length(i) == 2){
            temp <- bind_rows(semiper[[i[2]]], semiper[[i[1]]]) %>% arrange()
        } else {
            temp <- semiper[[i[1]]]
        }
        
        if (as.numeric(p) < 10){p <- paste("0", p, sep ="")}
        cmd <- paste("perlist <- append(perlist, list(per", p, " = temp))", sep = "")
        eval(parse(text=cmd))
    }
    semiper <- perlist
    # rm(per, perlist, p, cmd, i, temp, acv.edit)
    
    ## ---- Hace el actograma -----------------------------------------------------------------------
    xscale2 <- seq(as.numeric(set$ininoc)/3600, length.out = 25)
    xlabel2 <- ifelse(xscale2 >= 48, xscale2 - 48, ifelse(xscale2 >= 24, xscale2 - 24, xscale2))
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









