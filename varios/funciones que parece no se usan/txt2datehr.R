#' @title Texto y Date a posix
#' @description Valida un texto y revisa si es hora en formato "hh:mm" si lo es toma el date, arrejunta y transforma en fecha
#' @param date Fecha en formato texto
#' @param txt Hora en algun formato
#' @return dmy_hm
#' @export
#' @examples
#' # date <- "09/02/2000"
#' # txt <- "hh:mm"
#' # txt <- "23:22"
#' # txt <- "02:23"
#' # txt2datehr(date, txt)
#' @importFrom stringr str_split_fixed
#' @importFrom lubridate dmy
#' @importFrom lubridate days
#' @importFrom lubridate dmy_hm

# --------------------------------------------------------------------------- #
# ---- Valida un texto y revisa si es hora en formato "hh:mm" si lo es ------ #
# ---- toma el date, arrejunta y transforma en fecha|hora de lubridate ------ #
# ---- si la hora es menor al set$ininoc suma un día ------------------------ #
# --------------------------------------------------------------------------- #
# date <- "09/02/2000"
# txt <- "hh:mm"
# txt <- "23:22"
# txt <- "02:23"
# txt <- "holi"
# txt <- 23
txt2datehr <- function(date, txt){
    set <- NULL

    # Check txt
    if (grepl(":", txt) == FALSE){
        hora <- NA
    } else {
        # Separar
        hora <- str_split_fixed(txt, ":", 2)
        h <- hora[1,1]
        m <- hora[1,2]
        # Numero
        if (is.na(as.numeric(h)) & is.na(as.numeric(m))){
            hora <- NA
        } else {
            # limites
            h <- as.numeric(h)
            m <- as.numeric(m)
            hlim <- (h >= 0) & (h <= 23)
            mlim <- (m >= 0) & (m <= 59)
            if (hlim & mlim){
                hora <- txt
            } else {
                hora <- NA
            }
        }
    }

    # Si se pasa de las 00 hay que aumentar un día
    if (is.na(hora) == FALSE){
        temp <- hm(txt)
        if (temp < set$ininoc){
            d <- dmy(date)
            d <- d + days(1)
            date <- format(d, format = "%d/%m/%Y")
        }
    }

    # validar hora fecha
    if (is.na(hora)){
        return(NA)
    } else {
        hm <- paste(date, hora)
        hm <- dmy_hm(hm)
        return(hm)
    }
}
# a <- txt2datehr(date, "hh:mm")
# a <- txt2datehr(date, "23:22")
# a <- txt2datehr(date, "02:23")
# a <- txt2datehr(date, "holi")
# a <- txt2datehr(date, "23")
# a <- txt2datehr(date, "36")
