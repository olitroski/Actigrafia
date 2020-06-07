#' @title Crear tabla de estados de sleep wake
#' @description Para cada periodo tipo per01 muestra periodos.
#' @param acveditRDS es una lista que saca la funcion check.acvfilter
#' @param per el periodo para seleccionarlo desde la lista
#' @return un data frame con la info del periodo
#' @export
#' @examples
#' # setwd("D:/OneDrive/INTA/Actigrafia/testfolder")
#' # awdfile <- "2086-308-045 CHT Visit2.AWD"
#' # acveditRDS <- check.acvfilter(sub(".AWD", "", awdfile))
#' # acveditRDS <- acveditRDS$semiper
#' # stagesTable(acveditRDS, "per01")
#' @importFrom lubridate as.period


# Funcion para crear la tabla de estados de la pestana de edicion
# Toma como antecedente la acveditRDS() un reactive que esta en forma de poll
# que chequea si tiene cambios, ademas el input$perChoose que es de tipo "per00"
# COmo se usara en varias partes lo mejor es dejar como funcion y ademas como reactivo

stagesTable <- function(acveditRDS, per){
    # Nulos de paquete
    nombre <- time <- st.stable <- st.edit <- inicio <- termino <- estado <- duracion <- NULL

    # Procesar las fechas
    periodo <- str_split(per, " - ", simplify = TRUE)[1]
    periodo <- acveditRDS[["semiper"]][[periodo]]
    periodo <- select(periodo, nombre, time, st.stable, st.edit, filter)

    # Procesado simple de la tabla
    vectSeq <- Vectorize(seq.default, vectorize.args = c("from", "to"))

    # --- Indices y segmentos SUENO -------------------------------------------
    segm <- find.segment(periodo, "st.edit", filtro = "S")

    if (nrow(segm) == 0){
        dataSleep <- NULL
    } else {
        ranges <- vectSeq(from = segm$ini, to = segm$fin, by = 1)

        # Si solo hay uno que quede como lista
        if (nrow(segm) == 1){ranges <- list(ranges)}

        # Stats
        ini     <- lapply(ranges, function(x) min(periodo$time[x]))
        fin     <- lapply(ranges, function(x) max(periodo$time[x]))
        filtro  <- lapply(ranges, function(x) unique(periodo$filter[x]))

        # Reordenar la data
        dataSleep <- data.frame(ini = paste(ini, sep = ","), fin = paste(fin, sep = ","),
                                estado = "S", filtro = paste(filtro, sep = ","), stringsAsFactors = FALSE)
        dataSleep <- mutate(dataSleep, inicio = as_datetime(as.numeric(ini), lubridate::origin),
                            termino = as_datetime(as.numeric(fin), lubridate::origin))
    }


    # --- Indices y segmentos VIGILIA -----------------------------------------
    segm <- find.segment(periodo, "st.edit", filtro = "W")
    if (nrow(segm) == 0){
        dataWake <- NULL
    } else {
        ranges <- vectSeq(from = segm$ini, to = segm$fin, by = 1)
        if (nrow(segm) == 1){ranges <- list(ranges)}
        # Stats
        ini     <- lapply(ranges, function(x) min(periodo$time[x]))
        fin     <- lapply(ranges, function(x) max(periodo$time[x]))
        filtro  <- lapply(ranges, function(x) unique(periodo$filter[x]))
        # Reordenar la data
        dataWake <- data.frame(ini = paste(ini, sep = ","), fin = paste(fin, sep = ","),
                               estado = "W", filtro = paste(filtro, sep = ","), stringsAsFactors = FALSE)
        dataWake <- mutate(dataWake, inicio = as_datetime(as.numeric(ini), lubridate::origin),
                           termino = as_datetime(as.numeric(fin), lubridate::origin))
    }

    # Combinar y terminar
    data <- bind_rows(dataSleep, dataWake)
    data <- select(data, inicio, termino, estado, filtro)
    data <- arrange(data, inicio)
    data <- mutate(data, duracion = as.period(termino - inicio, unit = "minutes"), duracion = as.character(duracion))
    data <- mutate(data, inicio = format(inicio,  format = "%d-%m-%Y %H:%M"),
                   termino = format(termino, format = "%d-%m-%Y %H:%M"))

    # Y el resultado
    return(data)
}













