# Cargador de funciones
# En subcarpetas del wd 
function_loader <- function(dir = NULL, mf = NULL){
    # Capturar el working directory
    if (class(mf) == "NULL"){
        mf <- mainfolder
    } else {
        mf <- getwd()
    }
    
    if (class(dir) == "NULL"){
        stop("Debe ingresar un subfolder")
    }
    
    # ir al folder
    setwd(file.path(mf, dir))
    
    # Cargar cosas
    archivos <- dir()
    archivos <- archivos[grep(".[rR]", archivos)]
    
    for (script in archivos){
        source(script)
    }
    
    # volver
    setwd(mf)
}



#function_loader(dir="func_awd")

