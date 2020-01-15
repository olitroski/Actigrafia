# ----------------------------------------------------------------------------- #
# ----- Load folder, para cargar awd files y su status si hubiera ene2020 ----- #
# ----- Lab. Sueño - INTA - U.Chile - O.Rojas - oliver.rojas.b@gmail.com ------ #
# ----------------------------------------------------------------------------- #
# awdfolder <- "D:/OneDrive/INTA/Patricio Peirano/2019.12 Kansas/kansas"
# load.awdfolder(awdfolder)

load.awdfolder <- function(awdfolder){
    # capturar archivos
    setwd(awdfolder)
    archivos <- dir()

    # Solo awd y el sujeto
    awdfiles <- archivos[grep(".[Aa][Ww][Dd]$", archivos)]
    basename <- str_replace(awdfiles, ".[Aa][Ww][Dd]$", "")
    
    # n de archivos por sujeto
    nbase <- map(basename, function(x) {length(grep(x, archivos))}) %>% set_names(basename)
    nbase <- t(bind_rows(nbase))[,1]

    terminados <- archivos[grep("_final.log", archivos)]
    terminados <- str_replace(terminados, "_final.log", "")
    terminados <- basename %in% terminados
    
    # Terminar el output
    statusdf <- data.frame(Sujeto = basename, N.Files = nbase, Finalizado = terminados)
    row.names(statusdf) <- NULL
    
    statusdf <- statusdf %>% mutate(Status = ifelse(N.Files == 1 & Finalizado == FALSE, 1,
                                             ifelse(N.Files > 1  & Finalizado == FALSE, 2,
                                             ifelse(N.Files > 1  & Finalizado == TRUE,  3, 4)))) %>% arrange(Status)
    statusdf$Status <- factor(statusdf$Status, levels = c(1,2,3,4), labels = c("No procesado", "En Edicion", "Finalizado", "Error"))
    return(statusdf)
}
