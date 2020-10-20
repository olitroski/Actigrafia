#' @title Chequea el acv y el filter
#' @description Esta funcion mantiene actualizada el archivo acv (actividad) y filtro para cada sujeto
#' @param awdfile que seria un string con el nombre del archivo sin la extension
#' @param set el objeto de settings
#' @return una lista con varias cosas, algun dia actualizare esta parte
#' @export
#' @examples
#' # awdfile <- "2086-308-045 CHT Visit2.AWD"
#' # acveditRDS <- check.acvfilter(awdfile, set)
#' @importFrom dplyr group_by
#' @importFrom dplyr n
#' @importFrom dplyr desc
#' @importFrom lubridate as_datetime
#' @importFrom lubridate date
#' @importFrom stringr str_replace
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#'

# ------------------------------------------------------------------------------------- #
# ---- Script para chequear si el acv.edit.RDS y el filter.RDS calzan y son iguales --- #
# ---- Lab. Sueno - INTA - U.Chile - O.Rojas - oliver.rojas.b@gmail.com - 27.01.2020 -- #
# ------------------------------------------------------------------------------------- #
# Check del archivo de filtro y de acv.edit para verificar que tienen lo mismo y
# que ademas deje un output listo para hacer los graficos individuales.
# Funcionamiento
# 1. Leer ambos archivos
# 2. Checar que son iguales
# 3. Procesar el ACV para crear los semiperiodos con la funcion create.semiper
# El input es el reactive awdfile()
# El output es el list del create.semiper
check.acvfilter <- function(awdfile, set){
    indice <- original <- hrdec <- NULL

    # Arreglar el nombre
    awdfile <- sub(".AWD", "", awdfile)

    # Cargar acv y filter ----------------------------------------------------------
    acvfile <- paste(awdfile, ".acvedit.RDS", sep = "")
    acvfile <- readRDS(acvfile)

    filterfile <- paste(awdfile, ".edit.RDS", sep = "")
    filterfile <- readRDS(filterfile)
    filterfile <- filterfile[["filter"]]


    # Chequeo que en acv$filter haya algo diferente de NA  # i <- 1
    time <- acvfile$time
    filtro <- acvfile$filter
    filtroERROR <- rep(NA, nrow(filterfile))
    filtroNA <- NULL

    for (i in 1:nrow(filterfile)){
        # print(i)
        # Buscar los indices de cada linea de filtro en el file acv
        indx <- which(time >= filterfile$ini[i] & time <= filterfile$fin[i])

        # Solo deben haber numeros, nada de NA
        if (sum(is.na(filtro[indx])) != 0){
            filtroERROR <- c(filtroERROR, i)
        }
        # Guarda el index para la otra rueba
        filtroNA <- c(filtroNA, indx)
    }

    # Testear lo de afuera del filtro, que sea == NA
    NAtest <- 1:nrow(acvfile)
    NAtest <- NAtest[-filtroNA]
    NAtest <- sum(!is.na(filtro[NAtest]))

    if (NAtest != 0){
        filtroNA <- 1
    } else {
        filtroNA <- 0
    }

    # Usar el acv.edit para crear los semiperiodos con la funcion ---------
    semiper <- create.semiper(acvfile, set)

    # Redistirbuir los semiperiodos (con codigo del create.actogram())
    # Combinar [noche -> dia] mismo periodo
    per <- data.frame(original = names(semiper),
                      indice = str_replace(names(semiper), "[d|n]", ""),
                      stringsAsFactors = FALSE)
    per <- mutate(per, indx = as.numeric(indice))
    per <- arrange(per, indx, desc(original))
    per <- group_by(per, indice)
    per <- as.data.frame(mutate(per, n = n()))

    perlist <- list()
    for (p in unique(per$indice)){
        # p <- "2"
        temp <- per[per$indice == p, "original"]

        # Combinar si hay 2
        if (length(temp) == 1){
            temp.per <- semiper[[temp[1]]]
        } else if (length(temp) == 2){
            temp.per <- rbind(semiper[[temp[1]]], semiper[[temp[2]]])
        } else {
            stop("No pueden haber mas de 2 semiper")
        }

        # Camiar p
        if (as.numeric(p) < 10){
            p <- paste("0", p, sep ="")
        }

        # Agrega a la lista
        cmd <- paste("perlist <- append(perlist, list(per", p, " = temp.per))", sep = "")
        eval(parse(text=cmd))
    }

    # Corregir la hora continua decimal aca porque en la funcion del grafico da problema
    # Hora decimal continua ------
    lim <- as.numeric(set$ininoc)/3600
    perlist <- base::lapply(X = perlist,
                            function(x) mutate(x, xscale = ifelse(hrdec < lim,
                                                                  hrdec + 24,
                                                                  hrdec)))

    # Capturar el valor inicial de cada lista para usarla en el selectInput
    # timelist <- function(df){ return(min(df$time)) }
    timelist <- sapply(perlist, function(x) min(x[["time"]], na.rm = TRUE))
    timelist <- data.frame(time = timelist)
    timelist <- mutate(timelist, period = row.names(timelist))
    timelist <- mutate(timelist, time = lubridate::as_datetime(time, lubridate::origin))
    timelist <- mutate(timelist, tlist = format(lubridate::date(time), "%A %d/%m/%y"))

    # Y listo... tenemos el filtroNA, filtroERROR, el semiper, y el timelist
    return(list(semiper = perlist,
                filtroNA = filtroNA,
                filtroERROR = filtroERROR,
                timelist = timelist))
}
