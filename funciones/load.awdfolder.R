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
    
    # El argumento es la lista de files del folder.
    # archivos <- dir()
    archivos <- filelist

    # Solo awd y el sujeto
    awdfiles <- archivos[grep("[.][Aa][Ww][Dd]$", archivos)]

    if (length(awdfiles) > 0) {
        # Nombres
        basename <- str_replace(awdfiles, "[.][Aa][Ww][Dd]$", "")
        
        # N de archivos por sujeto
        nbase <- sapply(basename, function(x) length(grep(x, archivos)))
        
        # Terminados    
        terminados <- archivos[grep(".finished.RDS", archivos)]
        terminados <- str_replace(terminados, ".finished.RDS", "")
        terminados <- basename %in% terminados
        
        # Terminar el output
        statusdf <- data.frame(Sujeto = basename, 
                               N.Files = nbase, 
                               Finalizado = terminados, 
                               stringsAsFactors = FALSE)
        row.names(statusdf) <- NULL
        
        # Asignar status
        statusdf <- mutate(statusdf, 
                           Status = ifelse(N.Files == 1, "No procesado",
                                           ifelse(N.Files > 1  & Finalizado == FALSE, "En edicion",
                                                  ifelse(N.Files > 1  & Finalizado == TRUE, "Terminado", 
                                                         "Error")))) 
        statusdf <- arrange(statusdf, Sujeto)
        return(statusdf)
    
    # Si no hubiera nada de awd    
    } else {
        statusdf <- data.frame(Status = "Directorio sin archivos AWD")
        return(statusdf)
    } 
}
