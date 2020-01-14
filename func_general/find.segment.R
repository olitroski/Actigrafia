# FUncion para encontrar segmentos de cosas en un secuencia
# df <- mtcars
# var <- gear
# filtro <- 3

find.segment <- function(df = NULL, var = NULL, filtro = NULL){
    library(dplyr)
    
    tempvar <- df[[substitute(var)]]  
    segm <- which(tempvar == filtro)

    indx1 <- segm - c(-10, segm[-length(segm)])
    indx1 <- segm[which(indx1 > 1)]    
    
    indx2 <- segm - c(segm[-1], 1000000)
    indx2 <- segm[which(indx2 < -1)]
    
    segmdf <- data.frame(ini = indx1, fin = indx2)
    return(segmdf)

}
    
    
# find.segment(mtcars, gear, 3)
