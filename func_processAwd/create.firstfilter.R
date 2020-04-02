# -------------------------------------------------------------------------------------- #
# ----- Script para crear el primer filtro a partir de un "semiper" object  ------------ #
# ----- Lab. Sueño - INTA O.Rojas - oliver.rojas.b@gmail.com - 11.01.2020 -------------- #
# -------------------------------------------------------------------------------------- #
# Funcion evaluar los semiperiodos obtenidos de la secuencia:
#   create.acv > create.semiper = semiper (objeto lista)
#   y crear estadisticas para hacer el filtro y crear el <<edit.file>>
create.firstfilter <- function(awdfile, semiper){
    # cat(paste("Exec(create.firstfilter)"), awdfile, "\n")
    
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
    
    # data.frame de stats version vector
    allstats <- lapply(semiper, function(x) statdf(x))
    period <- names(allstats)
    allstats <- bind_rows(allstats) %>% mutate(dianoc = str_sub(period, 1, 1), 
                                            per = str_sub(period, 2, 2),
                                            filter = ifelse(is.na(meanW), 1, NA))   # <- este es el primer filtro

    # El primer filtro serán solo los NA del meanW a futuro espero crear algo más sofisticado.
    allstats$filter <- ifelse(is.na(allstats$meanW), 1, NA)
    
    
    # <<<<<< acá va el segundo filtro >>>>>>>>>> el de machine learning
    #
    #
    #
    #
    # ==========================================
    
    # Ordenar e indexar
    filtro <- allstats %>% filter(filter == 1) %>% arrange(ini) %>% mutate(tipo = 1, id = NA) %>% select(id, ini, fin, tipo)
    
    if (dim(filtro)[1] > 0){
        filtro <- mutate(filtro, id = 1:nrow(filtro))
    }
    
    
    # El archivo edit
    name <- paste(str_replace(awdfile, ".[Aa][Ww][Dd]", ""), ".edit.RDS", sep = "")
    cat(paste("|--- Se crea el archivo de filtro", name, "\n"))
    
    header <-c("Archivo Filtro",
               str_c("Sujeto: ", name),
               str_c("Creado: ", as.character(Sys.time())),
               "Inicia:  -No determinado- ",
               "Termina: -No determinado- ",
               "------------------------------------")

    # Guarda y sale
    saveRDS(object = list(header = header, filter = filtro), file = name)
    return(filtro)
}
