#' @title wrapper de merge estilo stata
#' @description Junta dos bases de datos con un merge y reporteria
#' @param xdf DataFrame, con "idvar" actua de "master"
#' @param ydf DataFrame, con "idvar" actua de "using"
#' @param byvar String, Variable idvar
#' @param keep Logical, si guarda o no el resultado, si no solo da el reporte
#' @param output Logical, si muestra o no el output
#' @return Lista con el merge separado en solo using, master, merge y un reporte
#' @export
#' @examples
#' # Datos para cruzar
#' # data <- mtcars
#' # data$cars <- row.names(mtcars)
#' # xdf <- data[-c(1:8),   c("cars", "mpg", "cyl", "disp", "hp", "drat", "wt")]
#' # ydf <- data[-c(26:32), c("cars", "qsec", "vs", "am", "gear", "carb")]
#' #
#' # omerge(xdf, ydf, "cars")
#' # omerge(xdf, ydf[, c("cars", "vs")], byvar = "cars")
#' # omerge(xdf, ydf, "cars", output = FALSE)
#' # omerge(xdf, ydf, "cars", keep = TRUE)
#' # omerge(xdf, ydf, "cars", keep = TRUE, output = FALSE)
#' @import dplyr
#'
omerge <- function(xdf = NULL, ydf = NULL, byvar = NULL, keep = FALSE, output = TRUE){
    fromX <- fromY <- NULL
    
    # Guardar nombres variables
    xnames <- names(xdf)
    ynames <- names(ydf)

    # Agregar marcadores para detectar origen
    xdf$fromX <- 1
    ydf$fromY <- 2

    # Hacer el merge con todo
    df <- merge(x = xdf, y = ydf, by = byvar, all = TRUE)
    df <- mutate(df, merge = ifelse(is.na(fromX), 0, 1) + ifelse(is.na(fromY), 0, 2))
    df <- select(df, -fromX, -fromY)
    df$merge <- factor(df$merge, levels = c(1,2,3), labels = c("Only in master", "Only in using", "Matched observations"))

    # Reporte
    if (output){
        # Tabla reporte
        tabla <- group_by(df, merge) %>% summarise(Conteo = n())
        tabla <- mutate(tabla, merge = as.character(merge)) %>% rename(Status = merge)
        tabla <- as.data.frame(tabla)
        n <- sum(tabla$Conteo, na.rm = TRUE)
        tabla[4, "Status"] <- "--- Total ---"
        tabla[4, "Conteo"] <- n

        # Reporte
        cat("--- Reporte variables merge --- \n")
        cat("Key:    ", byvar, "\n",sep = "")
        cat("Master: ", paste(xnames[-grep(byvar, xnames)], collapse = ", "), " \n", sep = "")
        cat("Using:  ", paste(ynames[-grep(byvar, ynames)], collapse = ", "), " \n\n", sep = "")
        print(tabla, row.names = FALSE)
    }

    # Guardar Resultado
    if (keep){
        return(list(master = filter(df, merge == "Only in master"),
                    using  = filter(df, merge == "Only in using"),
                    match  = filter(df, merge == "Matched observations"),
                    all    = df,
                    report = tabla))
    }
}
