#' @title omerge
#' 
#' @description Esta función es un wrapper del merge.table que hace problema cuando
#' la base "using" solo trae una variable. Entonces esta función hace todo el merge
#' completo porque es un cacho ir contando variables mejor que sea todo automatico.
#' 
#' @param xdf DataFrame, con "idvar" actua de "master"
#' @param ydf DataFrame, con "idvar" actua de "using"
#' @param byvar String, Variable idvar
#' @param keep Logical, si guarda o no el resultado, si no solo da el reporte
#' 
#' @return Lista con el merge separado en solo using, master, merge y un reporte
#' 
#' @examples
#' # Need some data
#' data <- mtcars
#' data$cars <- row.names(mtcars)
#' 
#' # Prepara archivos para cruzar 
#' xdf <- data[-c(1:8),   c("cars", "mpg", "cyl", "disp", "hp", "drat", "wt")]
#' ydf <- data[-c(26:32), c("cars", "qsec", "vs", "am", "gear", "carb")]
#' byvar <- "cars"
#' keep <- FALSE
#' 
#' omerge(xdf, ydf, "cars")
#' omerge(xdf, ydf, "cars", output = FALSE)
#' omerge(xdf, ydf, "cars", keep = TRUE)
#' omerge(xdf, ydf, "cars", keep = TRUE, output = FALSE)
#'
omerge <- function(xdf = NULL, ydf = NULL, byvar = NULL, keep = FALSE, output = TRUE){
    require(dplyr)
    
    # Ordena las bases de datos para situar el idvar al comienzo
    xdf <- ordervar(xdf, byvar)
    ydf <- ordervar(ydf, byvar)
    
    # Captura las columnas de la xdf para reutilizar codigo
    cols <- paste("2:", dim(xdf)[2], sep = "")
         
    ## hacer el merge
    df <- merge(x = xdf, y = ydf, by = byvar, all = TRUE)
    
    # Captura de columnas
    master <- cols
    using <- paste(max(eval(parse(text=master)))+1, ":", dim(df)[2], sep = "")
    merged <- paste("2:", dim(df)[2], sep="")
    
    # Hacer el reporte de variables
    varnames <- names(df)
    if (output ==TRUE){cat("\n--- Reporte variables del merge ---")}
    
    master.ini <- eval(parse(text=master))[1]
    master.fin <- max(eval(parse(text=master)))
    if (output ==TRUE){cat("\nMaster", master, "- Inicio:", varnames[master.ini], 
        "\n              Final:", varnames[master.fin], "\n")}
          
    using.ini <- eval(parse(text=using))[1]
    using.fin <- max(eval(parse(text=using)))
    if (output ==TRUE){cat("\nUsing", using, "- Inicio:", varnames[using.ini], 
        "\n              Final:", varnames[using.fin], "\n\n")}
    
    # Suma los NA por fila
    datos <- df
    datos$na_master <- rowSums(data.frame(is.na(datos[, eval(parse(text=master))])))
    datos$na_using  <- rowSums(data.frame(is.na(datos[, eval(parse(text=using ))])))
    datos$na_count  <- rowSums(data.frame(is.na(datos[, eval(parse(text=merged))])))
    
    # LOGICA de la función
    # Si la cantidad de NA en Master es igual al Length de las cols del master entonces no cruzó nada
    # y se clasifica como using, y así ....
    datos <- dplyr::mutate(datos, merge = ifelse(na_master == length(eval(parse(text=master))), 1, 
          ifelse(na_using == length(eval(parse(text=using))), 2, 
          ifelse(na_count == length(eval(parse(text=merged))), 3, 4))))
          
    datos$merge2 <- factor(datos$merge, levels = c(1,2,3,4), 
          labels = c("Only in using", "Only in master", "-No data, check-", "Matched observations"))
          
     # Reporteri­a en pantalla
     report <- data.frame(table(datos$merge2))
     names(report) <- c("StatusMerge", "Count")
     report <- rbind(report, data.frame(StatusMerge = "--- Total ---", Count = sum(report$Count)))
     
     if (output == TRUE){print(report, row.names = FALSE)}
     
     ## Return del data frame
     datos <- dplyr::select(datos, -na_master, -na_using, -na_count, -merge) %>% dplyr::rename(merge = merge2)
     
     u <- dplyr::filter(datos, merge == "Only in using")
     m <- dplyr::filter(datos, merge == "Only in master")
     match <- dplyr::filter(datos, merge == "Matched observations")

     if (keep == TRUE){
        return(list(master = m, using = u, match = match, report = report))
     } 
#      else {
#         return(report)
#      }
}
