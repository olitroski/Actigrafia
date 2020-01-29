# ------------------------------------------------------------------------------------- #
# ---- Script para crear "<<acv.edit>>" un data.frame que tendra el acv + la info de -- #
# ---- filtro inicial viene de <acv> y <filter.stats>  -------------------------------- #
# ---- Lab. Sue?o - INTA - U.Chile - O.Rojas - oliver.rojas.b@gmail.com - 11.01.2020 -- #
# ------------------------------------------------------------------------------------- #

create.acvedit <- function(awdfile, acv.filter, filter.stats){
    cat(paste("exec(create.acvedit)", awdfile, "\n"))
	# str(filter.stats)
    # acv.filter <- acv
    # head(acv.filter)
    # f <- 1
    
    # Buscar sección para filtrar y aplicar filtro
    for (f in 1:nrow(filter.stats)){
        ini <- filter.stats$ini[f]
        fin <- filter.stats$fin[f]
        
        range <- which(acv.filter$time >= ini & acv.filter$time <= fin)
        acv.filter$filter[range] <- 1
    }
    
    rdsname <- str_replace(awdfile, ".[Aa][Ww][Dd]$", "_acv.edit.RDS")
    saveRDS(acv.filter, rdsname)
    return(acv.filter)
}

