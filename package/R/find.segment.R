#' @title find.segment
#' @description Encuentra segmentos de algo en un vetor
#' @param df, un data.frame
#' @param var, varible donde buscar
#' @param filtro, un valor que buscar
#' @return data.frame con el inicio y fin (en index) de la posicion del filtro
#' @export
#' @examples
#' # find.segment(mtcars, "am", 1)
#'


# FUncion para encontrar segmentos de cosas en un secuencia
# df <- mtcars
# var <- "gear"
# filtro <- 3

find.segment <- function(df = NULL, var = NULL, filtro = NULL){
    
    tempvar <- df[[substitute(var)]]  
    segm <- which(tempvar == filtro)
    
    # se le resta el vector tipo dplyr::lag() +
    # un valor negativo al inicio
    indx1 <- segm - c(-10, segm[-length(segm)])
    # Esto resulta en un vector de 1 y >1 donde empieza una secuencia
    indx1 <- segm[which(indx1 > 1)]    
    
    # Este es el proceso inverso, se le resta el tipo dplyr::lead() +
    # valor muy grande al final
    indx2 <- segm - c(segm[-1], 1000000)
    # Resulta en val negativos y más grandes que < -1 donde termina el segemento
    indx2 <- segm[which(indx2 < -1)]
    
    # combina y resulta
    return(data.frame(ini = indx1, fin = indx2))
}
# find.segment(mtcars, gear, 3)
# rm(df, var, filtro)
