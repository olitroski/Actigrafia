#' @title Crea archivo edicion de actividad
#' @description Esta funcion toma un awdfile lo que sale de create.acv y el filtro para hacer un data frame con la data etiquetada en al variable filtro, basicamente es un dataframe acv que tiene actualizado los filtros.
#' @param awdfile el string del nobmre de archivo
#' @param acv.filter es un data frame ACV
#' @param filter.stats es el resultado de la funcion create.firstfilter
#' @return el acv con los filtros actualizados y ademas guarda el archivo en RDS
#' @export
#' @examples
#' # setwd("D:/OneDrive/INTA/Actigrafia/testfolder")
#' # acv <- create.acv("2086-308-045 CHT Visit2.AWD", sensi = set$sensivar)
#' # semiper <- create.semiper(acv)
#' # filtros <- create.firstfilter("2086-308-045 CHT Visit2.AWD", semiper)
#' # acvEdit <- create.acvedit("2086-308-045 CHT Visit2.AWD", acv, filtros)
#' @importFrom stringr str_replace

# ------------------------------------------------------------------------------------- #
# ---- Script para crear "<<acv.edit>>" un data.frame que tendra el acv + la info de -- #
# ---- filtro inicial viene de <acv> y <filter.stats>  -------------------------------- #
# ---- Lab. Sueno - INTA - U.Chile - O.Rojas - oliver.rojas.b@gmail.com - 11.01.2020 -- #
# ------------------------------------------------------------------------------------- #

create.acvedit <- function(awdfile, acv.filter, filter.stats){
    # Buscar seccion para filtrar y editar variable
    # for (f in 1:nrow(filter.stats)){
    #     ini <- filter.stats$ini[f]
    #     fin <- filter.stats$fin[f]
    # 
    #     range <- which(acv.filter$time >= ini & acv.filter$time <= fin)
    #     acv.filter$filter[range] <- 1
    # }

    rdsname <- str_replace(awdfile, ".[Aa][Ww][Dd]$", ".acvedit.RDS")
    saveRDS(acv.filter, rdsname)
    return(acv.filter)
}

