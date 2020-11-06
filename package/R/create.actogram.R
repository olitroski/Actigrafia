#' @title Crear actograma
#' @description Toma plot simple y crea un actogama
#' @param semiper corresponde al resutlado de la funcion check.acvfilter en especial el elemento 'semiper'
#' @param fy es el factor de expansion de y
#' @param filterRDS es la lista del poll de fitro que carga la app
#' @param set Settings file
#' @return un grafico
#' @export
#' @examples
#' # setwd("D:/OneDrive/INTA/Actigrafia/testfolder")
#' # set <- getset(getwd())
#' # filterRDS <- readRDS("2058-001-368 JRG Baseline.edit.RDS")
#' # acveditRDS <- check.acvfilter("2058-001-368 JRG Baseline.AWD", set)
#' # acveditRDS <- acveditRDS$semiper
#' # windows()
#' # create.actogram(acveditRDS, set, filterRDS)
#' @importFrom graphics par
#' @importFrom graphics plot
#' @importFrom graphics axis

## -------------------------------------------------------------------------------- #
## ----- Actograma con ggplot not ------------------------------------------------- #
## -------------------------------------------------------------------------------- #
# La idea es tomar un acv y mostrarlos de 20 a 20 horas asi aseguramos todos los dias
create.actogram <- function(semiper, set, filterRDS, fy = 1){
    # Valores
    xscale2 <- seq(as.numeric(set$ininoc)/3600, length.out = 25)
    xlabel2 <- ifelse(xscale2 >= 48, xscale2 - 48, ifelse(xscale2 >= 24, xscale2 - 24, xscale2))
    limX2 <- c(min(xscale2), max(xscale2))

    # Se saca toda la info desde la lista <semiper> filtros todo...   :)
    nplot <- length(semiper)
    par(mfrow = c(nplot + 2, 1))
    
    # Marco de arriba
    par(mar=c(2, 3, 0, 2) + 0.5 , xaxs = 'i', yaxs = 'i')
    plot(0, type = 'n', ann = FALSE, axes = FALSE, xlim = limX2)
    axis(side = 1, at = xscale2, labels = xlabel2)

    # Graficos
    for (g in 1:nplot){
        create.plotActo(semiper[[g]], set, filterRDS, pct.y = fy)
    }
    
    # Marco de abajo
    par(mar=c(0, 3, 2, 2) + 0.5)
    plot(0, type = 'n', ann = FALSE, axes = FALSE, xlim = limX2)
    axis(side = 3, at = xscale2, labels = xlabel2)
}

