olitoSleep <- function(){
    # Acá se dirige al path
    appDir <- system.file("leapp", package = "olitosleep")
    
    # En esta sección se puede agregar algo para controlar la versión
    if (appDir == "") {
        stop("Could not find myapp. Try re-installing `mypackage`.", call. = FALSE)
    }
    
    # Y se corre la app
    shiny::runApp(appDir, display.mode = "normal")
}