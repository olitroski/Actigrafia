# Funcion para crear la tabla de estados de la pestaña de edición
# Toma como antecedente la acveditRDS() un reactive que está en forma de poll 
# que chequea si tiene cambios, además el input$perChoose que es de tipo "per00"
# COmo se usará en varias partes lo mejor es dejar como función y además como reactivo

stagesTable <- function(acv, per){
    # Procesar las fechas
    periodo <- str_split(per, " - ", simplify = TRUE)[1]
    periodo <- acv[["semiper"]][[periodo]]
    periodo <- select(periodo, nombre, time, st.stable, st.edit, filter)
    
    # Procesado simple de la tabla
    vectSeq <- Vectorize(seq.default, vectorize.args = c("from", "to"))
    
    # Indices y segmentos SUEÑO
    segm <- find.segment(periodo, "st.edit", filtro = "S")
    ranges <- vectSeq(from = segm$ini, to = segm$fin, by = 1)
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
    
    # Indices y segmentos VIGILIA
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













