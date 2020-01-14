# Revisar si hay periodos sin actigrafo y marcarolos
# Opera sobre una liseta de la funcion create.semiper
# lista <- create.semiper(acv)
# semidf <- lista[[2]]

statlist <- function(lista=NULL){

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
    templist <- lapply(lista, function(x) statdf(x))

    dfnames <- names(templist)
    allstats <- NULL
    for (df in dfnames){
        dftemp <- templist[[df]]
        dftemp$dianoc <- str_sub(df, 1,1)
        dftemp$per <- str_sub(df, 2,2)
        allstats <- bind_rows(allstats, dftemp)
    }

    # Normalizar    
#     normalize <- function(x) {
#         return((x - mean(x, na.rm=T))/sd(x, na.rm=T))
#     }
#     
#     
#     allstats$meanS <- normalize(allstats$meanS)
#     allstats$sdS <- normalize(allstats$sdS)
#     allstats$meanW <- normalize(allstats$meanW)
#     allstats$sdW <- normalize(allstats$sdW)
    
    allstats$filter <- ifelse(is.na(allstats$meanW), "1", "")
    
    return(allstats)
}

# La otra versión apilando dfs
stats.final <- NULL
for (f in 1:100){
    # Crear el archivo de detección    
    file <- archivos[f]
    acv <- create.acv(file, set$sensivar)
    stats <- statlist(create.semiper(acv))
    stats$file <- file
    
    # Decidir y ampliar
    
    
    stats.final <- bind_rows(stats.final, stats)
}
rm(stats, file, f)

stats.final <- arrange(stats.final, file, ini)
write_dta(stats.final, "stats.final.dta")
write.xlsx(stats.final, "stats.final.xlsx")



ls()
