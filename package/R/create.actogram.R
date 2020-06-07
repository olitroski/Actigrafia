#' @title Crear actograma
#' @description Toma plot simple y crea un actogama
#' @param semiper corresponde al resutlado de la funcion check.acvfilter en especial el elemento 'semiper'
#' @param fy es el factor de expansion de y
#' @param set Settings file
#' @return un grafico
#' @export
#' @examples
#' # setwd("D:/OneDrive/INTA/Actigrafia/testfolder")
#' # awdfile <- "2086-306-757 NCM Visit2.AWD"
#' # acveditRDS <- check.acvfilter(sub(".AWD", "", awdfile))
#' # acveditRDS <- acveditRDS$semiper
#' # windows()
#' # create.actogram(acveditRDS)
#' @importFrom graphics par
#' @importFrom graphics plot
#' @importFrom graphics axis

## -------------------------------------------------------------------------------- #
## ----- Actograma con ggplot not ------------------------------------------------- #
## -------------------------------------------------------------------------------- #
# La idea es tomar un acv y mostrarlos de 20 a 20 horas asi aseguramos todos los dias
create.actogram <- function(semiper, fy = 1, set = NULL){

    # Valores
    xscale2 <- seq(as.numeric(set$ininoc)/3600, length.out = 25)
    xlabel2 <- ifelse(xscale2 >= 48, xscale2 - 48,
                      ifelse(xscale2 >= 24, xscale2 - 24, xscale2))
    limX2 <- c(min(xscale2), max(xscale2))

    # Se saca toda la info desde la lista <semiper> filtros todo...   :)
    nplot <- length(semiper)
    par(mfrow = c(nplot + 2, 1))

    par(mar=c(2, 3, 0, 2) + 0.5 , xaxs = 'i', yaxs = 'i')
    plot(0, type = 'n', ann = FALSE, axes = FALSE, xlim = limX2)
    axis(side = 1, at = xscale2, labels = xlabel2)

    for (g in 1:nplot){
        create.plotActo(semiper[[g]], fy, set = set)
    }

    par(mar=c(0, 3, 2, 2) + 0.5)
    plot(0, type = 'n', ann = FALSE, axes = FALSE, xlim = limX2)
    axis(side = 3, at = xscale2, labels = xlabel2)

}

#create.actogram(awdfile)
