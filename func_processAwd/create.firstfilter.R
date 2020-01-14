# -------------------------------------------------------------------------------------- #
# ----- Script para crear el primer filtro a partir de un "semiper" object  ------------ #
# ----- Lab. Sueño - INTA O.Rojas - oliver.rojas.b@gmail.com - 11.01.2020 -------------- #
# -------------------------------------------------------------------------------------- #
# Funcion evaluar los semiperiodos obtenidos de la secuencia:
#   create.acv > create.semiper = semiper (objeto lista)
#   y crear estadisticas para hacer el filtro y crear el <<edit.file>>
create.firstfilter <- function(awdfile, semiper){
    # Funciona para sacar stats de cada df de la lista
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
        
        
    # Ahora tomar la lista que resulta y compilar
    templist <- lapply(semiper, function(x) statdf(x))
    
    # Separa dianoc y periodo
    dfnames <- names(templist)
    allstats <- NULL
    for (df in dfnames){
        dftemp <- templist[[df]]
        dftemp$dianoc <- str_sub(df, 1,1)
        dftemp$per <- str_sub(df, 2,2)
        allstats <- bind_rows(allstats, dftemp)
    }
    
    
    # El primer filtro serán solo los NA del meanW a futuro espero crear algo más sofisticado.
    allstats$filter <- ifelse(is.na(allstats$meanW), 1, NA)
    
    # El archivo edit
    name <- paste(str_replace(awdfile, ".[Aa][Ww][Dd]", ""), ".edit", sep = "")
    cat(paste("Se crea el archivo de filtro", name, "\n"))
    header <-c("Archivo de edición", name, "Creado el ", as.character(Sys.time()), 
               "------------------------------------")

    filtro <- filter(allstats, filter == 1)
    filtro <- mutate(filtro, ini = format(filtro$ini, format = "%Y/%m/%d %H:%M"),
                             fin = format(filtro$fin, format = "%Y/%m/%d %H:%M"))
    filtro <- paste("filtro: ", filtro$ini, " - ", filtro$fin, sep = "")
    filtro <- c(header, filtro)
    
    # Guarda y sale
    writeLines(filtro, name)
    return(allstats)
}
