# ------------------------------------------------------------------------------------- #
# ---- Script para crear "<<acv.edit>>" un data.frame que tendra el acv + la info de -- #
# ---- filtro inicial viene de <acv> y <filter.stats>  -------------------------------- #
# ---- Lab. Sueño - INTA - U.Chile - O.Rojas - oliver.rojas.b@gmail.com - 11.01.2020 -- #
# ------------------------------------------------------------------------------------- #

create.acvedit <- function(awdfile, acv, filter.stats){
    head(acv)
    acv.filter <- acv
    
    # Set filtro
    filtro <- filter(filter.stats, filter == 1) %>% select(ini, fin)
    
    for (f in 1:nrow(filtro)){
        ini <- filtro$ini[f]
        fin <- filtro$fin[f]
        range <- which(acv.filter$time >= ini & acv.filter$time <= fin)
        
        acv.filter$filter[range] <- 1
    }
    
    rdsname <- str_replace(awdfile, ".[Aa][Ww][Dd]$", "_acv.edit.RDS")
    saveRDS(acv.filter, rdsname)
    
#     csvname <- str_replace(awdfile, ".[Aa][Ww][Dd]$", "_acv.edit.csv")
#     write.csv(acv.filter, csvname)
    return(acv.filter)
}

