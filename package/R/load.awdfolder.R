#' @title Carga contenido del folder AWD
#' @description Carga el listado de archivos AWD de un folder
#' @param filelist vector de archivos que se obtiene con dir()
#' @return data.frame con el nombre y status del archivo
#' @export
#' @examples
#' # awdfolder <- "D:/OneDrive/INTA/Patricio Peirano/2019.12 Kansas/kansas"
#' # load.awdfolder(awdfolder)#'
#' @importFrom stringr str_replace
#' @importFrom dplyr %>%
#' @importFrom dplyr arrange


# ----------------------------------------------------------------------------- #
# ----- Load folder, para cargar awd files y su status si hubiera ene2020 ----- #
# ----- Lab. Sueño - INTA - U.Chile - O.Rojas - oliver.rojas.b@gmail.com ------ #
# ----------------------------------------------------------------------------- #

load.awdfolder <- function(filelist = NULL){
    N.Files <- Finalizado <- Sujeto <- NULL
    
    # capturar archivos
    # archivos <- dir()
    archivos <- filelist

    # Solo awd y el sujeto
    awdfiles <- archivos[grep("[.][Aa][Ww][Dd]$", archivos)]

    if (length(awdfiles) > 0) {
        # Nombres
        basename <- str_replace(awdfiles, "[.][Aa][Ww][Dd]$", "")
        
        # N de archivos por sujeto
        # nbase <- map(basename, function(x) {length(grep(x, archivos))}) %>% set_names(basename)
        # nbase <- t(bind_rows(nbase))[,1]
        nbase <- sapply(basename, function(x) length(grep(x, archivos)))
        
        # Terminados    
        terminados <- archivos[grep(".finish.RDS", archivos)]
        terminados <- str_replace(terminados, ".finish.RDS", "")
        terminados <- basename %in% terminados
        
        # Terminar el output
        statusdf <- data.frame(Sujeto = basename, N.Files = nbase, Finalizado = terminados, stringsAsFactors = FALSE)
        row.names(statusdf) <- NULL
        
        # Asignar status
        statusdf <- mutate(statusdf, Status = ifelse(N.Files == 1 & Finalizado == FALSE, 1,
                                                     ifelse(N.Files > 1  & Finalizado == FALSE, 2,
                                                            ifelse(N.Files > 1  & Finalizado == TRUE,  3, 4)))) %>% arrange(Sujeto)
        statusdf$Status <- factor(statusdf$Status, 
                                  levels = c(1, 2, 3, 4), 
                                  labels = c("No procesado", "En edicion", "Terminado", "Con error"))
        return(statusdf)
    
    # Si no hubiera nada de awd    
    } else {
        statusdf <- data.frame(Status = "Directorio sin archivos AWD")
        return(statusdf)
    } 
}
