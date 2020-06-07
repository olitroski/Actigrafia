#' @title crea el primer filtro
#' @description toma un acv y en virtud de la actividad determina los primeros filtros que existen de forma automatica y ademas guarda el rds
#' @param awdfile el string de nombre de archivo
#' @param semiper el archivo de semiperiodos que sale de create.semiper(acv)
#' @return Devuelte un data frame y un rds al disco
#' @export
#' @examples
#' # acv <- create.acv("2086-308-045 CHT Visit2.AWD", set)
#' # semiper <- create.semiper(acv, set)
#' # filtros <- create.firstfilter("2086-308-045 CHT Visit2.AWD", semiper)
#' @importFrom stats sd
#' @importFrom dplyr bind_rows
#' @importFrom stringr %>%
#' @importFrom stringr str_sub
#' @importFrom stringr str_c
#
# -------------------------------------------------------------------------------------- #
# ----- Script para crear el primer filtro a partir de un "semiper" object  ------------ #
# ----- Lab. Sueno - INTA O.Rojas - oliver.rojas.b@gmail.com - 11.01.2020 -------------- #
# -------------------------------------------------------------------------------------- #
# Funcion evaluar los semiperiodos obtenidos de la secuencia:
#   create.acv > create.semiper = semiper (objeto lista)
#   y crear estadisticas para hacer el filtro y crear el <<edit.file>>
create.firstfilter <- function(awdfile, semiper){
    per <- dianoc <- sdS <- pctS <- meanW  <- sdW  <- prob <- filter1 <-filter2 <- ini <- id <- fin <- tipo <- NULL

    options(scipen = 999)

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
    allstats <- bind_rows(allstats)
    allstats <- mutate(allstats, dianoc = str_sub(period, 1, 1), per = period)
    allstats <- mutate(allstats, per = str_replace(per, "d", ""))
    allstats <- mutate(allstats, per = str_replace(per, "n", ""))
    allstats <- arrange(allstats, per, desc(dianoc))

    # ---- 1. Primer filtro los NA del meanW ------------------------------
    allstats$filter1 <- ifelse(is.na(allstats$meanW), 1, NA)

    # ---- 2. Filtro Segundo de regresion logística -----------------------
    # Crear variable
    allstats <- mutate(allstats, prob = ifelse(dianoc == 'd', -33.36581 - 0.1097845*sdS + 39.62804*pctS - 0.0141863*meanW + 0.0164165*sdW + 4.834989,
                                                              -33.36581 - 0.1097845*sdS + 39.62804*pctS - 0.0141863*meanW + 0.0164165*sdW))
    allstats <- mutate(allstats, prob = 1/(1 + exp(-prob)))
    allstats <- mutate(allstats, filter2 = ifelse(prob >= 0.64, 1, NA))

    # Combinar filtro y borrar
    allstats <- mutate(allstats, filter = ifelse(filter1 == 1 | filter2 == 1, 1, NA))
    allstats <- select(allstats, -filter1, -filter2, -prob)

    # Ordenar e indexar
    filtro <- allstats %>% filter(filter == 1) %>% arrange(ini) %>% mutate(tipo = 1, id = NA) %>% select(id, ini, fin, tipo)
    if (dim(filtro)[1] > 0){filtro <- mutate(filtro, id = seq_along(id))}

    # El archivo edit
    name <- paste(str_replace(awdfile, ".[Aa][Ww][Dd]", ""), ".edit.RDS", sep = "")
    cat(paste("|--- cFirstFilter: Archivo filtro", name, "\n"))

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
